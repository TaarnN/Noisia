use super::*;
use crate::ptypes::{Expr, Literal, MatchArm, Pattern, PatternKind};

impl<B: JitBackend> Interpreter<B> {
    pub(super) fn bind_pattern(&mut self, pattern: &Pattern, value: Value, mutable: bool) -> InterpResult<()> {
        let mut bindings = Vec::new();
        if !Self::collect_pattern_bindings(pattern, &value, &mut bindings) {
            return Err(InterpreterError::PatternMismatch(format!(
                "value `{value}` does not match pattern"
            )));
        }

        for (name, value) in bindings {
            self.define_var(name, value, mutable);
        }

        Ok(())
    }

    pub(super) fn collect_pattern_bindings(
        pattern: &Pattern,
        value: &Value,
        bindings: &mut Vec<(String, Value)>,
    ) -> bool {
        match &pattern.kind {
            PatternKind::Identifier(name) => {
                bindings.push((name.clone(), value.clone()));
                true
            }
            PatternKind::Ref(inner) => Self::collect_pattern_bindings(inner, value, bindings),
            PatternKind::Wildcard => true,
            PatternKind::Literal(literal) => Self::literal_matches(literal, value),
            PatternKind::Struct { fields, .. } => {
                if let Value::Struct { fields: values, .. } = value {
                    for (field_name, field_pattern) in fields {
                        let Some(field_value) = values.get(field_name) else {
                            return false;
                        };
                        if !Self::collect_pattern_bindings(field_pattern, field_value, bindings) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            PatternKind::Tuple(patterns) => {
                let values = match value {
                    Value::Tuple(values) => values,
                    Value::Array(values) => values,
                    _ => return false,
                };

                if patterns.len() != values.len() {
                    return false;
                }

                for (pattern, value) in patterns.iter().zip(values.iter()) {
                    if !Self::collect_pattern_bindings(pattern, value, bindings) {
                        return false;
                    }
                }

                true
            }
            PatternKind::Or(patterns) => {
                for candidate in patterns {
                    let mut candidate_bindings = Vec::new();
                    if Self::collect_pattern_bindings(candidate, value, &mut candidate_bindings) {
                        bindings.extend(candidate_bindings);
                        return true;
                    }
                }
                false
            }
            PatternKind::SomeVariant(inner) => {
                if let Some(inner_value) = Self::extract_variant_payload(value, "Some") {
                    Self::collect_pattern_bindings(inner, inner_value, bindings)
                } else {
                    false
                }
            }
            PatternKind::NoneVariant => match value {
                Value::EnumCase(name) => name == "None",
                Value::Struct { name, .. } => name == "None",
                Value::Unit => true,
                _ => false,
            },
            PatternKind::OkVariant(inner) => {
                if let Some(inner_value) = Self::extract_variant_payload(value, "Ok") {
                    Self::collect_pattern_bindings(inner, inner_value, bindings)
                } else {
                    false
                }
            }
            PatternKind::ErrVariant(inner) => {
                if let Some(inner_value) = Self::extract_variant_payload(value, "Err") {
                    Self::collect_pattern_bindings(inner, inner_value, bindings)
                } else {
                    false
                }
            }
            PatternKind::EnumVariant {
                variant_name,
                inner_pattern,
            } => {
                if let Some(inner_pattern) = inner_pattern {
                    if let Some(inner_value) = Self::extract_variant_payload(value, variant_name) {
                        Self::collect_pattern_bindings(inner_pattern, inner_value, bindings)
                    } else {
                        false
                    }
                } else {
                    match value {
                        Value::EnumCase(name) => name == variant_name,
                        Value::Struct { name, .. } => name == variant_name,
                        _ => false,
                    }
                }
            }
            PatternKind::Array(patterns) => {
                let Value::Array(values) = value else {
                    return false;
                };
                if patterns.len() != values.len() {
                    return false;
                }
                for (pattern, value) in patterns.iter().zip(values.iter()) {
                    if !Self::collect_pattern_bindings(pattern, value, bindings) {
                        return false;
                    }
                }
                true
            }
            PatternKind::Nil => matches!(value, Value::Unit),
        }
    }

    pub(super) fn extract_variant_payload<'a>(value: &'a Value, variant: &str) -> Option<&'a Value> {
        let Value::Struct { name, fields } = value else {
            return None;
        };

        if name != variant {
            return None;
        }

        if let Some(value) = fields.get("value") {
            return Some(value);
        }
        if let Some(value) = fields.get("0") {
            return Some(value);
        }
        fields.values().next()
    }

    pub(super) fn literal_matches(literal: &Literal, value: &Value) -> bool {
        match literal {
            Literal::Int(raw) => raw
                .replace('_', "")
                .parse::<i64>()
                .ok()
                .map(|expected| matches!(value, Value::Int(actual) if *actual == expected))
                .unwrap_or(false),
            Literal::Float(raw) => raw
                .replace('_', "")
                .parse::<f64>()
                .ok()
                .map(|expected| matches!(value, Value::Float(actual) if (*actual - expected).abs() < f64::EPSILON))
                .unwrap_or(false),
            Literal::String(expected) => matches!(value, Value::String(actual) if actual == expected),
            Literal::Bool(expected) => matches!(value, Value::Bool(actual) if actual == expected),
            _ => false,
        }
    }

    pub(super) fn eval_match_arms(
        &mut self,
        target: &Value,
        arms: &[MatchArm],
    ) -> InterpResult<Option<Value>> {
        for arm in arms {
            let mut bindings = Vec::new();
            if !Self::collect_pattern_bindings(&arm.pattern, target, &mut bindings) {
                continue;
            }

            self.push_scope();
            for (name, value) in bindings {
                self.define_var(name, value, false);
            }

            let guard_pass = match &arm.guard {
                Some(guard) => self.eval_expr(guard)?.is_truthy(),
                None => true,
            };

            if !guard_pass {
                self.pop_scope();
                continue;
            }

            let value = self.eval_expr(&arm.body);
            self.pop_scope();
            return value.map(Some);
        }

        Ok(None)
    }

    pub(super) fn into_iterable(&self, value: Value) -> InterpResult<Vec<Value>> {
        match value {
            Value::Array(values) => Ok(values),
            Value::Tuple(values) => Ok(values),
            Value::Map(entries) => Ok(entries.into_iter().map(|(_, value)| value).collect()),
            Value::String(value) => Ok(value
                .chars()
                .map(|ch| Value::String(ch.to_string()))
                .collect()),
            other => Err(InterpreterError::TypeError(format!(
                "value of type {} is not iterable",
                other.type_name()
            ))),
        }
    }

    pub(super) fn delete_binding(&mut self, expr: &Expr) -> InterpResult<()> {
        let Expr::Ident(name) = expr else {
            return Err(InterpreterError::TypeError(
                "delete expects an identifier target".to_string(),
            ));
        };

        for scope in self.scopes.iter_mut().rev() {
            if scope.values.remove(name).is_some() {
                return Ok(());
            }
        }

        Err(InterpreterError::UndefinedVariable(name.clone()))
    }
}
