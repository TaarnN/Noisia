use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, GenericValue, IntValue, PointerValue,
};
use inkwell::{IntPredicate, OptimizationLevel};

use crate::ptypes::{Expr, FunctionDecl, Literal, PatternKind, Stmt};

use super::{InterpreterError, JitBackend, Value};

type JitResult<T> = Result<T, InterpreterError>;

#[derive(Debug, Default, Clone, Copy)]
pub struct InkwellJitBackend;

impl InkwellJitBackend {
    fn can_lower(function: &FunctionDecl, args: &[Value]) -> bool {
        if function.body.is_none() {
            return false;
        }

        if args.iter().any(|value| !matches!(value, Value::Int(_))) {
            return false;
        }

        if function.params.len() != args.len() {
            return false;
        }

        if !function
            .params
            .iter()
            .all(|param| matches!(param.pattern.kind, PatternKind::Identifier(_)) && param.default.is_none())
        {
            return false;
        }

        let body = function.body.as_ref().expect("checked above");

        // Keep the first JIT cut strict: explicit return at top-level end.
        if !matches!(body.stmts.last(), Some(Stmt::Return(_))) {
            return false;
        }

        body.stmts.iter().all(Self::supports_stmt)
    }

    fn supports_stmt(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Attributed { stmt, .. } => Self::supports_stmt(stmt),
            Stmt::Let { pattern, expr, .. } => {
                matches!(pattern.kind, PatternKind::Identifier(_) | PatternKind::Wildcard)
                    && expr.as_ref().map(Self::supports_expr).unwrap_or(true)
            }
            Stmt::Expr(expr) => Self::supports_expr(expr),
            Stmt::Return(expr) => expr.as_ref().map(Self::supports_expr).unwrap_or(true),
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                Self::supports_expr(cond)
                    && then_block.stmts.iter().all(Self::supports_stmt)
                    && else_block
                        .as_ref()
                        .map(|block| block.stmts.iter().all(Self::supports_stmt))
                        .unwrap_or(true)
            }
            Stmt::Scope { body } => body.stmts.iter().all(Self::supports_stmt),
            _ => false,
        }
    }

    fn supports_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::Int(_) | Literal::Bool(_)) => true,
            Expr::Ident(_) => true,
            Expr::Grouping(inner) => Self::supports_expr(inner),
            Expr::Unary { op, rhs } => {
                matches!(op.as_str(), "-" | "+" | "!" | "not") && Self::supports_expr(rhs)
            }
            Expr::Binary { left, op, right } => {
                let arithmetic = matches!(
                    op.as_str(),
                    "+" | "-"
                        | "*"
                        | "/"
                        | "%"
                        | "=="
                        | "!="
                        | "<"
                        | "<="
                        | ">"
                        | ">="
                        | "&&"
                        | "and"
                        | "||"
                        | "or"
                        | "&"
                        | "|"
                        | "^"
                        | "<<"
                        | ">>"
                );
                let assignment = matches!(
                    op.as_str(),
                    "="
                        | ":="
                        | "+="
                        | "-="
                        | "*="
                        | "/="
                        | "%="
                        | "&="
                        | "|="
                        | "^="
                        | "<<="
                        | ">>="
                ) && matches!(left.as_ref(), Expr::Ident(_));

                (arithmetic || assignment) && Self::supports_expr(left) && Self::supports_expr(right)
            }
            Expr::IfExpr {
                cond,
                then_branch,
                else_branch,
            } => {
                Self::supports_expr(cond)
                    && Self::supports_expr(then_branch)
                    && else_branch
                        .as_ref()
                        .map(|expr| Self::supports_expr(expr))
                        .unwrap_or(true)
            }
            _ => false,
        }
    }

    fn run_jit(function: &FunctionDecl, args: &[Value]) -> JitResult<Value> {
        Target::initialize_native(&InitializationConfig::default()).map_err(|err| {
            InterpreterError::Runtime(format!("JIT target initialization failed: {err}"))
        })?;

        let context = Context::create();
        let module = context.create_module("noisia_jit_module");
        let builder = context.create_builder();

        let mut compiler = AstToLlvmCompiler::new(&context, module, builder);
        let llvm_function = compiler.compile_function(function)?;
        compiler.verify_module()?;
        let (module, i64_type) = compiler.finish();

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Default)
            .map_err(|err| InterpreterError::Runtime(format!("JIT engine creation failed: {err}")))?;

        let generic_args: Vec<GenericValue<'_>> = args
            .iter()
            .map(|arg| {
                let value = match arg {
                    Value::Int(v) => *v,
                    _ => 0,
                };
                i64_type.create_generic_value(value as u64, true)
            })
            .collect();

        let generic_arg_refs: Vec<&GenericValue<'_>> = generic_args.iter().collect();
        let result = unsafe { execution_engine.run_function(llvm_function, &generic_arg_refs) };

        Ok(Value::Int(result.as_int(true) as i64))
    }
}

impl JitBackend for InkwellJitBackend {
    fn execute(&mut self, function: &FunctionDecl, args: &[Value]) -> JitResult<Option<Value>> {
        if !Self::can_lower(function, args) {
            return Ok(None);
        }

        // If JIT codegen fails unexpectedly, interpreter runtime remains the fallback path.
        match Self::run_jit(function, args) {
            Ok(value) => Ok(Some(value)),
            Err(_) => Ok(None),
        }
    }
}

struct AstToLlvmCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    i64_type: IntType<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    current_fn: Option<FunctionValue<'ctx>>,
}

impl<'ctx> AstToLlvmCompiler<'ctx> {
    fn new(context: &'ctx Context, module: Module<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            context,
            module,
            builder,
            i64_type: context.i64_type(),
            variables: HashMap::new(),
            current_fn: None,
        }
    }

    fn finish(self) -> (Module<'ctx>, IntType<'ctx>) {
        (self.module, self.i64_type)
    }

    fn verify_module(&self) -> JitResult<()> {
        self.module
            .verify()
            .map_err(|err| InterpreterError::Runtime(format!("JIT module verification failed: {err}")))
    }

    fn compile_function(&mut self, function: &FunctionDecl) -> JitResult<FunctionValue<'ctx>> {
        let body = function
            .body
            .as_ref()
            .ok_or_else(|| InterpreterError::Runtime("JIT expected function body".to_string()))?;

        let args: Vec<BasicMetadataTypeEnum<'ctx>> =
            vec![self.i64_type.into(); function.params.len()];
        let fn_type = self.i64_type.fn_type(&args, false);
        let llvm_function = self.module.add_function(&function.name, fn_type, None);

        self.current_fn = Some(llvm_function);
        self.variables.clear();

        let entry = self.context.append_basic_block(llvm_function, "entry");
        self.builder.position_at_end(entry);

        for (idx, param) in function.params.iter().enumerate() {
            let param_name = match &param.pattern.kind {
                PatternKind::Identifier(name) => name.as_str(),
                PatternKind::Wildcard => continue,
                _ => {
                    return Err(InterpreterError::Runtime(
                        "JIT only supports identifier/wildcard parameters".to_string(),
                    ))
                }
            };

            let ptr = self.create_entry_alloca(llvm_function, param_name)?;
            let value = llvm_function
                .get_nth_param(idx as u32)
                .ok_or_else(|| {
                    InterpreterError::Runtime(format!(
                        "JIT missing function argument at index {idx}"
                    ))
                })?
                .into_int_value();
            self.builder
                .build_store(ptr, value)
                .map_err(|err| Self::builder_err("store arg", err))?;
            self.variables.insert(param_name.to_string(), ptr);
        }

        self.compile_block(body)?;

        if !self.current_block_has_terminator() {
            let zero = self.i64_type.const_zero();
            self.builder
                .build_return(Some(&zero))
                .map_err(|err| Self::builder_err("implicit return", err))?;
        }

        self.current_fn = None;
        Ok(llvm_function)
    }

    fn create_entry_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
    ) -> JitResult<PointerValue<'ctx>> {
        let entry = function.get_first_basic_block().ok_or_else(|| {
            InterpreterError::Runtime("JIT function has no entry block".to_string())
        })?;

        let temp_builder = self.context.create_builder();
        if let Some(first_instruction) = entry.get_first_instruction() {
            temp_builder.position_before(&first_instruction);
        } else {
            temp_builder.position_at_end(entry);
        }

        temp_builder
            .build_alloca(self.i64_type, name)
            .map_err(|err| Self::builder_err("alloca", err))
    }

    fn compile_block(&mut self, block: &crate::ptypes::Block) -> JitResult<()> {
        for stmt in &block.stmts {
            if self.current_block_has_terminator() {
                break;
            }
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> JitResult<()> {
        match stmt {
            Stmt::Attributed { stmt, .. } => self.compile_stmt(stmt),
            Stmt::Let { pattern, expr, .. } => {
                let value = match expr {
                    Some(expr) => self.compile_expr(expr)?,
                    None => self.i64_type.const_zero(),
                };

                match &pattern.kind {
                    PatternKind::Identifier(name) => {
                        let function = self.current_function()?;
                        let ptr = self.create_entry_alloca(function, name)?;
                        self.builder
                            .build_store(ptr, value)
                            .map_err(|err| Self::builder_err("let store", err))?;
                        self.variables.insert(name.clone(), ptr);
                    }
                    PatternKind::Wildcard => {}
                    _ => {
                        return Err(InterpreterError::Runtime(
                            "JIT let pattern must be identifier/wildcard".to_string(),
                        ))
                    }
                }
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                Ok(())
            }
            Stmt::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.compile_expr(expr)?,
                    None => self.i64_type.const_zero(),
                };
                self.builder
                    .build_return(Some(&value))
                    .map_err(|err| Self::builder_err("return", err))?;
                Ok(())
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => self.compile_if_stmt(cond, then_block, else_block.as_ref()),
            Stmt::Scope { body } => self.compile_block(body),
            _ => Err(InterpreterError::Runtime(format!(
                "JIT unsupported statement: {stmt:?}"
            ))),
        }
    }

    fn compile_if_stmt(
        &mut self,
        cond: &Expr,
        then_block: &crate::ptypes::Block,
        else_block: Option<&crate::ptypes::Block>,
    ) -> JitResult<()> {
        let function = self.current_function()?;
        let then_bb = self.context.append_basic_block(function, "if.then");
        let else_bb = self.context.append_basic_block(function, "if.else");
        let cont_bb = self.context.append_basic_block(function, "if.cont");

        let cond_value = self.compile_expr(cond)?;
        let cond_bool = self.to_bool(cond_value, "if.cond")?;

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|err| Self::builder_err("if branch", err))?;

        self.builder.position_at_end(then_bb);
        self.compile_block(then_block)?;
        if !self.current_block_has_terminator() {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|err| Self::builder_err("if then -> cont", err))?;
        }

        self.builder.position_at_end(else_bb);
        if let Some(else_block) = else_block {
            self.compile_block(else_block)?;
        }
        if !self.current_block_has_terminator() {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|err| Self::builder_err("if else -> cont", err))?;
        }

        self.builder.position_at_end(cont_bb);
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> JitResult<IntValue<'ctx>> {
        match expr {
            Expr::Literal(Literal::Int(raw)) => {
                let value = raw.replace('_', "").parse::<i64>().map_err(|_| {
                    InterpreterError::Runtime(format!("JIT invalid integer literal `{raw}`"))
                })?;
                Ok(self.i64_type.const_int(value as u64, true))
            }
            Expr::Literal(Literal::Bool(value)) => {
                Ok(self.i64_type.const_int(if *value { 1 } else { 0 }, false))
            }
            Expr::Ident(name) => {
                let ptr = self.get_var_ptr(name)?;
                let loaded = self
                    .builder
                    .build_load(self.i64_type, ptr, "loadtmp")
                    .map_err(|err| Self::builder_err("load", err))?;
                Ok(loaded.into_int_value())
            }
            Expr::Grouping(inner) => self.compile_expr(inner),
            Expr::Unary { op, rhs } => {
                let rhs = self.compile_expr(rhs)?;
                match op.as_str() {
                    "-" => self
                        .builder
                        .build_int_neg(rhs, "negtmp")
                        .map_err(|err| Self::builder_err("int neg", err)),
                    "+" => Ok(rhs),
                    "!" | "not" => {
                        let rhs_bool = self.to_bool(rhs, "not.cond")?;
                        let negated = self
                            .builder
                            .build_not(rhs_bool, "nottmp")
                            .map_err(|err| Self::builder_err("not", err))?;
                        self.builder
                            .build_int_z_extend(negated, self.i64_type, "not.cast")
                            .map_err(|err| Self::builder_err("not cast", err))
                    }
                    _ => Err(InterpreterError::Runtime(format!(
                        "JIT unsupported unary op `{op}`"
                    ))),
                }
            }
            Expr::Binary { left, op, right } => self.compile_binary(left, op, right),
            Expr::IfExpr {
                cond,
                then_branch,
                else_branch,
            } => self.compile_if_expr(cond, then_branch, else_branch.as_deref()),
            _ => Err(InterpreterError::Runtime(format!(
                "JIT unsupported expression: {expr:?}"
            ))),
        }
    }

    fn compile_if_expr(
        &mut self,
        cond: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
    ) -> JitResult<IntValue<'ctx>> {
        let function = self.current_function()?;
        let then_bb = self.context.append_basic_block(function, "ifexpr.then");
        let else_bb = self.context.append_basic_block(function, "ifexpr.else");
        let merge_bb = self.context.append_basic_block(function, "ifexpr.merge");

        let cond_value = self.compile_expr(cond)?;
        let cond_bool = self.to_bool(cond_value, "ifexpr.cond")?;
        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|err| Self::builder_err("ifexpr branch", err))?;

        let mut incoming: Vec<(BasicValueEnum<'ctx>, BasicBlock<'ctx>)> = Vec::new();

        self.builder.position_at_end(then_bb);
        let then_value = self.compile_expr(then_branch)?;
        if !self.current_block_has_terminator() {
            self.builder
                .build_unconditional_branch(merge_bb)
                .map_err(|err| Self::builder_err("ifexpr then -> merge", err))?;
            let then_end = self.current_block()?;
            incoming.push((then_value.into(), then_end));
        }

        self.builder.position_at_end(else_bb);
        let else_value = match else_branch {
            Some(expr) => self.compile_expr(expr)?,
            None => self.i64_type.const_zero(),
        };
        if !self.current_block_has_terminator() {
            self.builder
                .build_unconditional_branch(merge_bb)
                .map_err(|err| Self::builder_err("ifexpr else -> merge", err))?;
            let else_end = self.current_block()?;
            incoming.push((else_value.into(), else_end));
        }

        self.builder.position_at_end(merge_bb);

        if incoming.is_empty() {
            return Ok(self.i64_type.const_zero());
        }

        let phi = self
            .builder
            .build_phi(self.i64_type, "ifexpr.phi")
            .map_err(|err| Self::builder_err("ifexpr phi", err))?;

        let incoming_refs: Vec<(&dyn BasicValue<'ctx>, BasicBlock<'ctx>)> = incoming
            .iter()
            .map(|(value, block)| (value as &dyn BasicValue<'ctx>, *block))
            .collect();
        phi.add_incoming(&incoming_refs);

        Ok(phi.as_basic_value().into_int_value())
    }

    fn compile_binary(&mut self, left: &Expr, op: &str, right: &Expr) -> JitResult<IntValue<'ctx>> {
        if Self::is_assignment_op(op) {
            return self.compile_assignment(left, op, right);
        }

        let left = self.compile_expr(left)?;
        let right = self.compile_expr(right)?;

        match op {
            "+" => self
                .builder
                .build_int_add(left, right, "addtmp")
                .map_err(|err| Self::builder_err("add", err)),
            "-" => self
                .builder
                .build_int_sub(left, right, "subtmp")
                .map_err(|err| Self::builder_err("sub", err)),
            "*" => self
                .builder
                .build_int_mul(left, right, "multmp")
                .map_err(|err| Self::builder_err("mul", err)),
            "/" => self
                .builder
                .build_int_signed_div(left, right, "divtmp")
                .map_err(|err| Self::builder_err("div", err)),
            "%" => self
                .builder
                .build_int_signed_rem(left, right, "modtmp")
                .map_err(|err| Self::builder_err("mod", err)),
            "&" => self
                .builder
                .build_and(left, right, "andtmp")
                .map_err(|err| Self::builder_err("bit and", err)),
            "|" => self
                .builder
                .build_or(left, right, "ortmp")
                .map_err(|err| Self::builder_err("bit or", err)),
            "^" => self
                .builder
                .build_xor(left, right, "xortmp")
                .map_err(|err| Self::builder_err("bit xor", err)),
            "<<" => self
                .builder
                .build_left_shift(left, right, "shltmp")
                .map_err(|err| Self::builder_err("shl", err)),
            ">>" => self
                .builder
                .build_right_shift(left, right, true, "shrtmp")
                .map_err(|err| Self::builder_err("shr", err)),
            "==" => self.compare_to_i64(IntPredicate::EQ, left, right, "eqtmp"),
            "!=" => self.compare_to_i64(IntPredicate::NE, left, right, "netmp"),
            "<" => self.compare_to_i64(IntPredicate::SLT, left, right, "lttmp"),
            "<=" => self.compare_to_i64(IntPredicate::SLE, left, right, "letmp"),
            ">" => self.compare_to_i64(IntPredicate::SGT, left, right, "gttmp"),
            ">=" => self.compare_to_i64(IntPredicate::SGE, left, right, "getmp"),
            "&&" | "and" => {
                let left_bool = self.to_bool(left, "land.l")?;
                let right_bool = self.to_bool(right, "land.r")?;
                let anded = self
                    .builder
                    .build_and(left_bool, right_bool, "land")
                    .map_err(|err| Self::builder_err("logical and", err))?;
                self.builder
                    .build_int_z_extend(anded, self.i64_type, "land.cast")
                    .map_err(|err| Self::builder_err("logical and cast", err))
            }
            "||" | "or" => {
                let left_bool = self.to_bool(left, "lor.l")?;
                let right_bool = self.to_bool(right, "lor.r")?;
                let ored = self
                    .builder
                    .build_or(left_bool, right_bool, "lor")
                    .map_err(|err| Self::builder_err("logical or", err))?;
                self.builder
                    .build_int_z_extend(ored, self.i64_type, "lor.cast")
                    .map_err(|err| Self::builder_err("logical or cast", err))
            }
            _ => Err(InterpreterError::Runtime(format!(
                "JIT unsupported binary op `{op}`"
            ))),
        }
    }

    fn compile_assignment(&mut self, left: &Expr, op: &str, right: &Expr) -> JitResult<IntValue<'ctx>> {
        let Expr::Ident(name) = left else {
            return Err(InterpreterError::Runtime(
                "JIT assignment target must be identifier".to_string(),
            ));
        };

        let right_value = self.compile_expr(right)?;

        let target_ptr = if let Some(ptr) = self.variables.get(name).copied() {
            ptr
        } else if op == ":=" {
            let function = self.current_function()?;
            let ptr = self.create_entry_alloca(function, name)?;
            self.variables.insert(name.clone(), ptr);
            ptr
        } else {
            return Err(InterpreterError::Runtime(format!(
                "JIT assignment to undefined variable `{name}`"
            )));
        };

        let value = match op {
            "=" | ":=" => right_value,
            "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => {
                let current = self
                    .builder
                    .build_load(self.i64_type, target_ptr, "assload")
                    .map_err(|err| Self::builder_err("assignment load", err))?
                    .into_int_value();
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
                self.compile_binary_from_values(base_op, current, right_value)?
            }
            _ => unreachable!(),
        };

        self.builder
            .build_store(target_ptr, value)
            .map_err(|err| Self::builder_err("assignment store", err))?;

        Ok(value)
    }

    fn compile_binary_from_values(
        &mut self,
        op: &str,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> JitResult<IntValue<'ctx>> {
        match op {
            "+" => self
                .builder
                .build_int_add(left, right, "addtmp")
                .map_err(|err| Self::builder_err("add", err)),
            "-" => self
                .builder
                .build_int_sub(left, right, "subtmp")
                .map_err(|err| Self::builder_err("sub", err)),
            "*" => self
                .builder
                .build_int_mul(left, right, "multmp")
                .map_err(|err| Self::builder_err("mul", err)),
            "/" => self
                .builder
                .build_int_signed_div(left, right, "divtmp")
                .map_err(|err| Self::builder_err("div", err)),
            "%" => self
                .builder
                .build_int_signed_rem(left, right, "modtmp")
                .map_err(|err| Self::builder_err("mod", err)),
            "&" => self
                .builder
                .build_and(left, right, "andtmp")
                .map_err(|err| Self::builder_err("and", err)),
            "|" => self
                .builder
                .build_or(left, right, "ortmp")
                .map_err(|err| Self::builder_err("or", err)),
            "^" => self
                .builder
                .build_xor(left, right, "xortmp")
                .map_err(|err| Self::builder_err("xor", err)),
            "<<" => self
                .builder
                .build_left_shift(left, right, "shltmp")
                .map_err(|err| Self::builder_err("shl", err)),
            ">>" => self
                .builder
                .build_right_shift(left, right, true, "shrtmp")
                .map_err(|err| Self::builder_err("shr", err)),
            _ => Err(InterpreterError::Runtime(format!(
                "JIT unsupported binary op `{op}`"
            ))),
        }
    }

    fn compare_to_i64(
        &mut self,
        predicate: IntPredicate,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
        name: &str,
    ) -> JitResult<IntValue<'ctx>> {
        let cmp = self
            .builder
            .build_int_compare(predicate, left, right, name)
            .map_err(|err| Self::builder_err("compare", err))?;
        self.builder
            .build_int_z_extend(cmp, self.i64_type, "cmp.cast")
            .map_err(|err| Self::builder_err("compare cast", err))
    }

    fn to_bool(&self, value: IntValue<'ctx>, name: &str) -> JitResult<IntValue<'ctx>> {
        self.builder
            .build_int_compare(IntPredicate::NE, value, self.i64_type.const_zero(), name)
            .map_err(|err| Self::builder_err("to bool", err))
    }

    fn get_var_ptr(&self, name: &str) -> JitResult<PointerValue<'ctx>> {
        self.variables.get(name).copied().ok_or_else(|| {
            InterpreterError::Runtime(format!("JIT unknown variable `{name}`"))
        })
    }

    fn current_function(&self) -> JitResult<FunctionValue<'ctx>> {
        self.current_fn
            .ok_or_else(|| InterpreterError::Runtime("JIT current function not set".to_string()))
    }

    fn current_block(&self) -> JitResult<BasicBlock<'ctx>> {
        self.builder.get_insert_block().ok_or_else(|| {
            InterpreterError::Runtime("JIT builder has no insert block".to_string())
        })
    }

    fn current_block_has_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .and_then(|block| block.get_terminator())
            .is_some()
    }

    fn is_assignment_op(op: &str) -> bool {
        matches!(
            op,
            "="
                | ":="
                | "+="
                | "-="
                | "*="
                | "/="
                | "%="
                | "&="
                | "|="
                | "^="
                | "<<="
                | ">>="
        )
    }

    fn builder_err(context: &str, err: BuilderError) -> InterpreterError {
        InterpreterError::Runtime(format!("JIT builder error ({context}): {err}"))
    }
}
