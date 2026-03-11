use super::*;
use crate::ptypes::{Block, Expr, InterpolatedPartKind, Literal};
use std::collections::BTreeMap;

impl<B: JitBackend> Interpreter<B> {
    pub(super) fn eval_block_expr(&mut self, block: &Block) -> InterpResult<Value> {
        let outcome = self.exec_block(block, true)?;
        match outcome.signal {
            ExecSignal::None => Ok(outcome.last_value),
            ExecSignal::Return(_) => Err(InterpreterError::Runtime(
                "return is not allowed in expression block context".to_string(),
            )),
            ExecSignal::Break => Err(InterpreterError::Runtime(
                "break is not allowed in expression block context".to_string(),
            )),
            ExecSignal::Continue => Err(InterpreterError::Runtime(
                "continue is not allowed in expression block context".to_string(),
            )),
        }
    }

    pub(super) fn eval_expr(&mut self, expr: &Expr) -> InterpResult<Value> {
        match expr {
            Expr::Literal(literal) => self.eval_literal(literal),
            Expr::Ident(name) => self.get_var(name).or_else(|_| {
                if self.functions.contains_key(name)
                    || matches!(name.as_str(), "print" | "println" | "len")
                {
                    Ok(Value::Function(name.clone()))
                } else {
                    Err(InterpreterError::UndefinedVariable(name.clone()))
                }
            }),
            Expr::Phrase(value) => Ok(Value::Phrase(value.clone())),
            Expr::EnumCase(value) => Ok(Value::EnumCase(value.clone())),
            Expr::Await(inner) | Expr::Spawn(inner) => self.eval_expr(inner),
            Expr::Try(inner) => match self.eval_expr(inner) {
                Ok(value) => Ok(value),
                Err(_) => Ok(Value::Unit),
            },
            Expr::Rewind {
                subject,
                target,
                condition,
                query,
                else_expr,
            } => {
                let rewound = self.execute_rewind(
                    &subject.as_ref().map(|expr| *expr.clone()),
                    &target.as_ref().map(|expr| *expr.clone()),
                    &condition.as_ref().map(|expr| *expr.clone()),
                    query,
                )?;

                if rewound {
                    Ok(Value::Bool(true))
                } else if let Some(else_expr) = else_expr {
                    self.eval_expr(else_expr)
                } else {
                    Ok(Value::Bool(false))
                }
            }
            Expr::IfExpr {
                cond,
                then_branch,
                else_branch,
            } => {
                if self.eval_expr(cond)?.is_truthy() {
                    self.eval_expr(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.eval_expr(else_branch)
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::MatchExpr { expr, arms } => {
                let target = self.eval_expr(expr)?;
                Ok(self.eval_match_arms(&target, arms)?.unwrap_or(Value::Unit))
            }
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Unary { op, rhs } => self.eval_unary(op, rhs),
            Expr::Grouping(expr) => self.eval_expr(expr),
            Expr::Block(block) => self.eval_block_expr(block),
            Expr::AsyncBlock { body, .. } => self.eval_block_expr(body),
            Expr::Call { callee, args, .. } => {
                let callee_value = self.eval_expr(callee)?;
                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }
                self.invoke_callable(callee_value, evaluated_args)
            }
            Expr::MethodCall {
                object,
                method,
                args,
                ..
            } => self.eval_method_call(object, method, args),
            Expr::FieldAccess { object, field } => {
                let object_value = self.eval_expr(object)?;
                self.lookup_field(object_value, field, false)
            }
            Expr::OptionalFieldAccess { object, field } => {
                let object_value = self.eval_expr(object)?;
                self.lookup_field(object_value, field, true)
            }
            Expr::Index { array, index } => {
                let array_value = self.eval_expr(array)?;
                let index_value = self.eval_expr(index)?;
                self.lookup_index(array_value, index_value)
            }
            Expr::Slice { target, start, end } => {
                let target_value = self.eval_expr(target)?;
                self.eval_slice(target_value, start.as_deref(), end.as_deref())
            }
            Expr::Lambda { .. } => Err(InterpreterError::UnsupportedExpression(
                "lambda".to_string(),
            )),
            Expr::Range {
                start,
                end,
                inclusive,
                step,
            } => self.eval_range(
                start.as_deref(),
                end.as_deref(),
                *inclusive,
                step.as_deref(),
            ),
            Expr::Pipeline { left, right }
            | Expr::PointerPipeline { left, right }
            | Expr::SelectorPipeline { left, right } => self.eval_pipeline(left, right),
            Expr::ListComp {
                expr,
                pattern,
                iter,
                guard,
            } => {
                let iterable = self.eval_expr(iter)?;
                let items = self.into_iterable(iterable)?;
                let mut out = Vec::new();

                for item in items {
                    self.push_scope();
                    let bind = self.bind_pattern(pattern, item, false);
                    if bind.is_err() {
                        self.pop_scope();
                        continue;
                    }

                    let passes_guard = match guard {
                        Some(guard) => self.eval_expr(guard)?.is_truthy(),
                        None => true,
                    };

                    if passes_guard {
                        out.push(self.eval_expr(expr)?);
                    }

                    self.pop_scope();
                }

                Ok(Value::Array(out))
            }
            Expr::Coalesce { left, right } => {
                let left = self.eval_expr(left)?;
                if left == Value::Unit {
                    self.eval_expr(right)
                } else {
                    Ok(left)
                }
            }
            Expr::OptionalCall { callee, args, .. } => {
                let callee_value = self.eval_expr(callee)?;
                if callee_value == Value::Unit {
                    return Ok(Value::Unit);
                }

                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }
                self.invoke_callable(callee_value, evaluated_args)
            }
            Expr::InterpolatedString { parts } => {
                let mut out = String::new();
                for part in parts {
                    match &part.kind {
                        InterpolatedPartKind::Text(text) => out.push_str(text),
                        InterpolatedPartKind::Expr(expr) => {
                            out.push_str(&self.eval_expr(expr)?.to_string())
                        }
                    }
                }
                Ok(Value::String(out))
            }
            Expr::MacroCall { name, args } => {
                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }
                self.call_function(name, evaluated_args)
            }
            Expr::PointerDeref { expr, .. } => self.eval_expr(expr),
            Expr::PointerRef { expr, .. } => self.eval_expr(expr),
            Expr::PointerNew { expr, .. } => self.eval_expr(expr),
            Expr::Branch { name, from, body } => {
                let from_value = self.eval_expr(from)?;
                self.push_scope();
                if let Some(name) = name {
                    self.define_var(name.clone(), from_value, false);
                }
                let result = self.eval_block_expr(body);
                self.pop_scope();
                result
            }
        }
    }

    pub(super) fn eval_literal(&mut self, literal: &Literal) -> InterpResult<Value> {
        match literal {
            Literal::Int(raw) => {
                let parsed = raw.replace('_', "").parse::<i64>().map_err(|_| {
                    InterpreterError::TypeError(format!("invalid integer literal `{raw}`"))
                })?;
                Ok(Value::Int(parsed))
            }
            Literal::Float(raw) => {
                let parsed = raw.replace('_', "").parse::<f64>().map_err(|_| {
                    InterpreterError::TypeError(format!("invalid float literal `{raw}`"))
                })?;
                Ok(Value::Float(parsed))
            }
            Literal::String(value) => Ok(Value::String(value.clone())),
            Literal::Bool(value) => Ok(Value::Bool(*value)),
            Literal::Unit { v, u } => {
                let mut fields = BTreeMap::new();
                if v.contains('.') {
                    let numeric = v.parse::<f64>().map_err(|_| {
                        InterpreterError::TypeError(format!("invalid unit numeric literal `{v}`"))
                    })?;
                    fields.insert("value".to_string(), Value::Float(numeric));
                } else {
                    let numeric = v.parse::<i64>().map_err(|_| {
                        InterpreterError::TypeError(format!("invalid unit numeric literal `{v}`"))
                    })?;
                    fields.insert("value".to_string(), Value::Int(numeric));
                }
                fields.insert("unit".to_string(), Value::String(u.clone()));
                Ok(Value::Struct {
                    name: "unit".to_string(),
                    fields,
                })
            }
            Literal::Array(items) | Literal::Vector(items) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Array(values))
            }
            Literal::Map(entries) => {
                let mut values = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    values.push((self.eval_expr(key)?, self.eval_expr(value)?));
                }
                Ok(Value::Map(values))
            }
            Literal::Struct { name, base, fields } => {
                let mut values = BTreeMap::new();

                if let Some(base) = base {
                    let base_value = self.eval_expr(base)?;
                    match base_value {
                        Value::Struct { fields, .. } => values.extend(fields),
                        other => {
                            return Err(InterpreterError::TypeError(format!(
                                "struct update base must be Struct, found {}",
                                other.type_name()
                            )))
                        }
                    }
                }

                for (field, expr) in fields {
                    values.insert(field.clone(), self.eval_expr(expr)?);
                }

                Ok(Value::Struct {
                    name: name.clone(),
                    fields: values,
                })
            }
            Literal::Tuple(items) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Tuple(values))
            }
        }
    }

    pub(super) fn invoke_callable(&mut self, callee: Value, args: Vec<Value>) -> InterpResult<Value> {
        match callee {
            Value::Function(name) => self.call_function(&name, args),
            other => Err(InterpreterError::TypeError(format!(
                "value of type {} is not callable",
                other.type_name()
            ))),
        }
    }

    pub(super) fn eval_method_call(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Expr],
    ) -> InterpResult<Value> {
        let object_value = self.eval_expr(object)?;
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            evaluated_args.push(self.eval_expr(arg)?);
        }

        match method {
            "len" => {
                if !evaluated_args.is_empty() {
                    return Err(InterpreterError::InvalidArgumentCount {
                        function: format!("{}.len", object_value.type_name()),
                        expected: 0,
                        found: evaluated_args.len(),
                    });
                }
                let len = match object_value {
                    Value::Array(values) => values.len(),
                    Value::Tuple(values) => values.len(),
                    Value::String(value) => value.chars().count(),
                    Value::Map(entries) => entries.len(),
                    Value::Struct { fields, .. } => fields.len(),
                    other => {
                        return Err(InterpreterError::TypeError(format!(
                            "len() is not defined for {}",
                            other.type_name()
                        )))
                    }
                };
                Ok(Value::Int(len as i64))
            }
            _ => Err(InterpreterError::UnsupportedExpression(format!(
                "method call .{method}"
            ))),
        }
    }

    pub(super) fn lookup_field(&self, object: Value, field: &str, optional: bool) -> InterpResult<Value> {
        match object {
            Value::Struct { fields, .. } => Ok(fields.get(field).cloned().unwrap_or(Value::Unit)),
            Value::Unit if optional => Ok(Value::Unit),
            other => Err(InterpreterError::TypeError(format!(
                "field access requires Struct, found {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn lookup_index(&self, target: Value, index: Value) -> InterpResult<Value> {
        match target {
            Value::Array(values) => {
                let idx = self.expect_int(index, "array index")?;
                let idx = self.to_usize(idx, "array index")?;
                values.get(idx).cloned().ok_or_else(|| {
                    InterpreterError::Runtime(format!("array index out of range: {idx}"))
                })
            }
            Value::Tuple(values) => {
                let idx = self.expect_int(index, "tuple index")?;
                let idx = self.to_usize(idx, "tuple index")?;
                values.get(idx).cloned().ok_or_else(|| {
                    InterpreterError::Runtime(format!("tuple index out of range: {idx}"))
                })
            }
            Value::String(value) => {
                let idx = self.expect_int(index, "string index")?;
                let idx = self.to_usize(idx, "string index")?;
                value
                    .chars()
                    .nth(idx)
                    .map(|ch| Value::String(ch.to_string()))
                    .ok_or_else(|| {
                        InterpreterError::Runtime(format!("string index out of range: {idx}"))
                    })
            }
            Value::Map(entries) => {
                for (key, value) in entries {
                    if key == index {
                        return Ok(value);
                    }
                }
                Ok(Value::Unit)
            }
            other => Err(InterpreterError::TypeError(format!(
                "index access is not defined for {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn eval_slice(
        &mut self,
        target: Value,
        start: Option<&Expr>,
        end: Option<&Expr>,
    ) -> InterpResult<Value> {
        let start_idx = if let Some(start) = start {
            let v = self.eval_expr(start)?;
            Some(self.to_usize(self.expect_int(v, "slice start")?, "slice start")?)
        } else {
            None
        };

        let end_idx = if let Some(end) = end {
            let v = self.eval_expr(end)?;
            Some(self.to_usize(self.expect_int(v, "slice end")?, "slice end")?)
        } else {
            None
        };

        match target {
            Value::Array(values) => {
                let len = values.len();
                let start = start_idx.unwrap_or(0).min(len);
                let end = end_idx.unwrap_or(len).min(len);
                if start > end {
                    return Ok(Value::Array(Vec::new()));
                }
                Ok(Value::Array(values[start..end].to_vec()))
            }
            Value::Tuple(values) => {
                let len = values.len();
                let start = start_idx.unwrap_or(0).min(len);
                let end = end_idx.unwrap_or(len).min(len);
                if start > end {
                    return Ok(Value::Tuple(Vec::new()));
                }
                Ok(Value::Tuple(values[start..end].to_vec()))
            }
            Value::String(value) => {
                let chars: Vec<char> = value.chars().collect();
                let len = chars.len();
                let start = start_idx.unwrap_or(0).min(len);
                let end = end_idx.unwrap_or(len).min(len);
                if start > end {
                    return Ok(Value::String(String::new()));
                }
                Ok(Value::String(chars[start..end].iter().collect()))
            }
            other => Err(InterpreterError::TypeError(format!(
                "slice is not defined for {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn eval_range(
        &mut self,
        start: Option<&Expr>,
        end: Option<&Expr>,
        inclusive: bool,
        step: Option<&Expr>,
    ) -> InterpResult<Value> {
        let Some(start) = start else {
            return Err(InterpreterError::UnsupportedExpression(
                "open range start".to_string(),
            ));
        };
        let Some(end) = end else {
            return Err(InterpreterError::UnsupportedExpression(
                "open range end".to_string(),
            ));
        };

        let start_value = self.eval_expr(start)?;
        let start = self.expect_int(start_value, "range start")?;
        let end_value = self.eval_expr(end)?;
        let end = self.expect_int(end_value, "range end")?;
        let step = if let Some(step) = step {
            let step_value = self.eval_expr(step)?;
            self.expect_int(step_value, "range step")?
        } else if start <= end {
            1
        } else {
            -1
        };

        if step == 0 {
            return Err(InterpreterError::Runtime(
                "range step cannot be zero".to_string(),
            ));
        }

        let mut values = Vec::new();
        let mut current = start;

        if step > 0 {
            while current < end || (inclusive && current == end) {
                values.push(Value::Int(current));
                current += step;
            }
        } else {
            while current > end || (inclusive && current == end) {
                values.push(Value::Int(current));
                current += step;
            }
        }

        Ok(Value::Array(values))
    }

    pub(super) fn eval_pipeline(&mut self, left: &Expr, right: &Expr) -> InterpResult<Value> {
        let left_value = self.eval_expr(left)?;

        match right {
            Expr::Call { callee, args, .. } => {
                let callee_value = self.eval_expr(callee)?;
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(left_value);
                for arg in args {
                    call_args.push(self.eval_expr(arg)?);
                }
                self.invoke_callable(callee_value, call_args)
            }
            Expr::Ident(name) => self.call_function(name, vec![left_value]),
            _ => Err(InterpreterError::UnsupportedExpression(
                "pipeline rhs".to_string(),
            )),
        }
    }

    pub(super) fn eval_unary(&mut self, op: &str, rhs: &Expr) -> InterpResult<Value> {
        let rhs = self.eval_expr(rhs)?;
        match op {
            "-" => match rhs {
                Value::Int(v) => Ok(Value::Int(-v)),
                Value::Float(v) => Ok(Value::Float(-v)),
                other => Err(InterpreterError::TypeError(format!(
                    "unary - expects numeric, found {}",
                    other.type_name()
                ))),
            },
            "+" => match rhs {
                Value::Int(v) => Ok(Value::Int(v)),
                Value::Float(v) => Ok(Value::Float(v)),
                other => Err(InterpreterError::TypeError(format!(
                    "unary + expects numeric, found {}",
                    other.type_name()
                ))),
            },
            "!" | "not" => Ok(Value::Bool(!rhs.is_truthy())),
            _ => Err(InterpreterError::UnsupportedExpression(format!(
                "unary operator {op}"
            ))),
        }
    }

    pub(super) fn eval_binary(&mut self, left: &Expr, op: &str, right: &Expr) -> InterpResult<Value> {
        if Self::is_assignment_operator(op) {
            return self.eval_assignment(left, op, right);
        }

        match op {
            "&&" | "and" => {
                let left = self.eval_expr(left)?;
                if !left.is_truthy() {
                    return Ok(Value::Bool(false));
                }
                let right = self.eval_expr(right)?;
                Ok(Value::Bool(right.is_truthy()))
            }
            "||" | "or" => {
                let left = self.eval_expr(left)?;
                if left.is_truthy() {
                    return Ok(Value::Bool(true));
                }
                let right = self.eval_expr(right)?;
                Ok(Value::Bool(right.is_truthy()))
            }
            _ => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                self.apply_binary(op, left, right)
            }
        }
    }

    pub(super) fn is_assignment_operator(op: &str) -> bool {
        matches!(
            op,
            "=" | ":=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
        )
    }

    pub(super) fn eval_assignment(&mut self, left: &Expr, op: &str, right: &Expr) -> InterpResult<Value> {
        let options = AssignOptions {
            allow_define: op == ":=",
            ..AssignOptions::DEFAULT
        };

        let value = if op == "=" || op == ":=" {
            self.eval_expr(right)?
        } else {
            let current = self.read_assignment_target(left)?;
            let rhs = self.eval_expr(right)?;
            let base_op = match op {
                "+=" => "+",
                "-=" => "-",
                "*=" => "*",
                "/=" => "/",
                "%=" => "%",
                "&=" => "&",
                "|=" => "|",
                "^=" => "^",
                "<<=" => "<<",
                ">>=" => ">>",
                _ => unreachable!(),
            };
            self.apply_binary(base_op, current, rhs)?
        };

        self.assign_target(left, value.clone(), options)
    }

    pub(super) fn read_assignment_target(&mut self, target: &Expr) -> InterpResult<Value> {
        match target {
            Expr::Ident(name) => self.get_var(name),
            Expr::FieldAccess { object, field } => {
                let object_value = self.eval_expr(object)?;
                self.lookup_field(object_value, field, false)
            }
            Expr::Index { array, index } => {
                let array_value = self.eval_expr(array)?;
                let index_value = self.eval_expr(index)?;
                self.lookup_index(array_value, index_value)
            }
            _ => Err(InterpreterError::TypeError(
                "invalid assignment target".to_string(),
            )),
        }
    }

    pub(super) fn assign_target(
        &mut self,
        target: &Expr,
        value: Value,
        options: AssignOptions,
    ) -> InterpResult<Value> {
        match target {
            Expr::Ident(name) => {
                self.assign_var(
                    name,
                    value.clone(),
                    options.allow_define,
                    options.ignore_mutability,
                )?;
                Ok(value)
            }
            Expr::FieldAccess { object, field } => {
                let Expr::Ident(object_name) = object.as_ref() else {
                    return Err(InterpreterError::TypeError(
                        "field assignment currently requires identifier object".to_string(),
                    ));
                };

                let mut object_value = self.get_var(object_name)?;
                let Value::Struct { fields, .. } = &mut object_value else {
                    return Err(InterpreterError::TypeError(
                        "field assignment requires Struct value".to_string(),
                    ));
                };

                fields.insert(field.clone(), value.clone());
                self.assign_var(object_name, object_value, false, options.ignore_mutability)?;
                Ok(value)
            }
            Expr::Index { array, index } => {
                let Expr::Ident(array_name) = array.as_ref() else {
                    return Err(InterpreterError::TypeError(
                        "index assignment currently requires identifier target".to_string(),
                    ));
                };

                let index_value = self.eval_expr(index)?;
                let mut container = self.get_var(array_name)?;
                match &mut container {
                    Value::Array(values) => {
                        let idx = self.expect_int(index_value, "array index")?;
                        let idx = self.to_usize(idx, "array index")?;
                        if idx >= values.len() {
                            return Err(InterpreterError::Runtime(format!(
                                "array index out of range: {idx}"
                            )));
                        }
                        values[idx] = value.clone();
                    }
                    Value::Map(entries) => {
                        let mut found = false;
                        for (key, entry_value) in entries.iter_mut() {
                            if *key == index_value {
                                *entry_value = value.clone();
                                found = true;
                                break;
                            }
                        }

                        if !found {
                            entries.push((index_value, value.clone()));
                        }
                    }
                    other => {
                        return Err(InterpreterError::TypeError(format!(
                            "index assignment is not defined for {}",
                            other.type_name()
                        )))
                    }
                }

                self.assign_var(array_name, container, false, options.ignore_mutability)?;
                Ok(value)
            }
            _ => Err(InterpreterError::TypeError(
                "invalid assignment target".to_string(),
            )),
        }
    }

    pub(super) fn assignment_target_name(&self, target: &Expr) -> String {
        match target {
            Expr::Ident(name) => name.clone(),
            Expr::FieldAccess { field, .. } => field.clone(),
            Expr::Index { .. } => "index".to_string(),
            _ => "unknown".to_string(),
        }
    }

    pub(super) fn apply_binary(&self, op: &str, left: Value, right: Value) -> InterpResult<Value> {
        match op {
            "+" => self.binary_add(left, right),
            "-" => self.binary_numeric(op, left, right),
            "*" => self.binary_numeric(op, left, right),
            "/" => self.binary_numeric(op, left, right),
            "%" => self.binary_numeric(op, left, right),
            "==" => Ok(Value::Bool(left == right)),
            "!=" => Ok(Value::Bool(left != right)),
            "<" => self.binary_compare(op, left, right),
            "<=" => self.binary_compare(op, left, right),
            ">" => self.binary_compare(op, left, right),
            ">=" => self.binary_compare(op, left, right),
            "&" => self.binary_bitwise(op, left, right),
            "|" => self.binary_bitwise(op, left, right),
            "^" => self.binary_bitwise(op, left, right),
            "<<" => self.binary_bitwise(op, left, right),
            ">>" => self.binary_bitwise(op, left, right),
            "<@" => self.binary_contains(left, right, false),
            "@>" => self.binary_contains(left, right, true),
            _ => Err(InterpreterError::UnsupportedExpression(format!(
                "binary operator {op}"
            ))),
        }
    }

    pub(super) fn binary_add(&self, left: Value, right: Value) -> InterpResult<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
            (Value::String(a), b) => Ok(Value::String(a + &b.to_string())),
            (a, Value::String(b)) => Ok(Value::String(a.to_string() + &b)),
            (Value::Array(mut a), Value::Array(b)) => {
                a.extend(b);
                Ok(Value::Array(a))
            }
            (Value::Tuple(mut a), Value::Tuple(b)) => {
                a.extend(b);
                Ok(Value::Tuple(a))
            }
            (a, b) => Err(InterpreterError::TypeError(format!(
                "operator + is not defined for {} and {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    pub(super) fn binary_numeric(&self, op: &str, left: Value, right: Value) -> InterpResult<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                "-" => Ok(Value::Int(a - b)),
                "*" => Ok(Value::Int(a * b)),
                "/" => {
                    if b == 0 {
                        return Err(InterpreterError::Runtime("division by zero".to_string()));
                    }
                    Ok(Value::Int(a / b))
                }
                "%" => {
                    if b == 0 {
                        return Err(InterpreterError::Runtime("modulo by zero".to_string()));
                    }
                    Ok(Value::Int(a % b))
                }
                _ => unreachable!(),
            },
            (Value::Int(a), Value::Float(b)) => self.binary_numeric_float(op, a as f64, b),
            (Value::Float(a), Value::Int(b)) => self.binary_numeric_float(op, a, b as f64),
            (Value::Float(a), Value::Float(b)) => self.binary_numeric_float(op, a, b),
            (a, b) => Err(InterpreterError::TypeError(format!(
                "operator {op} is not defined for {} and {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    pub(super) fn binary_numeric_float(&self, op: &str, left: f64, right: f64) -> InterpResult<Value> {
        match op {
            "-" => Ok(Value::Float(left - right)),
            "*" => Ok(Value::Float(left * right)),
            "/" => {
                if right == 0.0 {
                    return Err(InterpreterError::Runtime("division by zero".to_string()));
                }
                Ok(Value::Float(left / right))
            }
            "%" => {
                if right == 0.0 {
                    return Err(InterpreterError::Runtime("modulo by zero".to_string()));
                }
                Ok(Value::Float(left % right))
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn binary_compare(&self, op: &str, left: Value, right: Value) -> InterpResult<Value> {
        let out = match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                "<" => a < b,
                "<=" => a <= b,
                ">" => a > b,
                ">=" => a >= b,
                _ => unreachable!(),
            },
            (Value::Int(a), Value::Float(b)) => match op {
                "<" => (a as f64) < b,
                "<=" => (a as f64) <= b,
                ">" => (a as f64) > b,
                ">=" => (a as f64) >= b,
                _ => unreachable!(),
            },
            (Value::Float(a), Value::Int(b)) => match op {
                "<" => a < (b as f64),
                "<=" => a <= (b as f64),
                ">" => a > (b as f64),
                ">=" => a >= (b as f64),
                _ => unreachable!(),
            },
            (Value::Float(a), Value::Float(b)) => match op {
                "<" => a < b,
                "<=" => a <= b,
                ">" => a > b,
                ">=" => a >= b,
                _ => unreachable!(),
            },
            (Value::String(a), Value::String(b)) => match op {
                "<" => a < b,
                "<=" => a <= b,
                ">" => a > b,
                ">=" => a >= b,
                _ => unreachable!(),
            },
            (a, b) => {
                return Err(InterpreterError::TypeError(format!(
                    "operator {op} is not defined for {} and {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        };

        Ok(Value::Bool(out))
    }

    pub(super) fn binary_bitwise(&self, op: &str, left: Value, right: Value) -> InterpResult<Value> {
        let left = self.expect_int(left, "bitwise lhs")?;
        let right = self.expect_int(right, "bitwise rhs")?;
        let value = match op {
            "&" => left & right,
            "|" => left | right,
            "^" => left ^ right,
            "<<" => left << right,
            ">>" => left >> right,
            _ => unreachable!(),
        };
        Ok(Value::Int(value))
    }

    pub(super) fn binary_contains(
        &self,
        left: Value,
        right: Value,
        left_contains_right: bool,
    ) -> InterpResult<Value> {
        let result = if left_contains_right {
            self.contains(&left, &right)?
        } else {
            self.contains(&right, &left)?
        };
        Ok(Value::Bool(result))
    }

    pub(super) fn contains(&self, container: &Value, needle: &Value) -> InterpResult<bool> {
        match container {
            Value::Array(values) | Value::Tuple(values) => {
                Ok(values.iter().any(|value| value == needle))
            }
            Value::String(value) => Ok(value.contains(&needle.to_string())),
            Value::Map(entries) => Ok(entries.iter().any(|(key, _)| key == needle)),
            Value::Struct { fields, .. } => {
                if let Value::String(name) = needle {
                    Ok(fields.contains_key(name))
                } else {
                    Ok(fields.values().any(|value| value == needle))
                }
            }
            other => Err(InterpreterError::TypeError(format!(
                "contains is not defined for {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn expect_int(&self, value: Value, context: &str) -> InterpResult<i64> {
        match value {
            Value::Int(value) => Ok(value),
            other => Err(InterpreterError::TypeError(format!(
                "{context} expects Int, found {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn to_usize(&self, value: i64, context: &str) -> InterpResult<usize> {
        if value < 0 {
            return Err(InterpreterError::Runtime(format!(
                "{context} cannot be negative: {value}"
            )));
        }
        Ok(value as usize)
    }
}
