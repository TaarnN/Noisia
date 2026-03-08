use super::super::*;

impl Parser {
    pub(in crate::parser) fn attach_trailing_closure_arg(expr: Expr, closure: Expr) -> Expr {
        match expr {
            Expr::Call {
                callee,
                generics,
                mut args,
            } => {
                args.push(closure);
                Expr::Call {
                    callee,
                    generics,
                    args,
                }
            }
            Expr::MethodCall {
                object,
                method,
                generics,
                mut args,
            } => {
                args.push(closure);
                Expr::MethodCall {
                    object,
                    method,
                    generics,
                    args,
                }
            }
            Expr::OptionalCall {
                callee,
                generics,
                mut args,
            } => {
                args.push(closure);
                Expr::OptionalCall {
                    callee,
                    generics,
                    args,
                }
            }
            Expr::FieldAccess { object, field } => Expr::MethodCall {
                object,
                method: field,
                generics: Vec::new(),
                args: vec![closure],
            },
            Expr::OptionalFieldAccess { object, field } => {
                let callee = Expr::FieldAccess { object, field };
                Expr::OptionalCall {
                    callee: Box::new(callee),
                    generics: Vec::new(),
                    args: vec![closure],
                }
            }
            other => Expr::Call {
                callee: Box::new(other),
                generics: Vec::new(),
                args: vec![closure],
            },
        }
    }


}
