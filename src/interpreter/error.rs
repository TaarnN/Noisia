use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    EntryPointNotFound,
    FunctionNotFound(String),
    InvalidArgumentCount {
        function: String,
        expected: usize,
        found: usize,
    },
    UnsupportedStatement(String),
    UnsupportedExpression(String),
    UndefinedVariable(String),
    ImmutableBinding(String),
    TypeError(String),
    PatternMismatch(String),
    ReplayTargetNotFound(String),
    ReplayMutationOutsideReplay,
    Runtime(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::EntryPointNotFound => {
                write!(
                    f,
                    "no entry point found: mark a function with @entry or define fn main()"
                )
            }
            InterpreterError::FunctionNotFound(name) => {
                write!(f, "function not found: {name}")
            }
            InterpreterError::InvalidArgumentCount {
                function,
                expected,
                found,
            } => {
                write!(
                    f,
                    "invalid argument count for {function}: expected {expected}, found {found}"
                )
            }
            InterpreterError::UnsupportedStatement(kind) => {
                write!(f, "unsupported statement in interpreter v1: {kind}")
            }
            InterpreterError::UnsupportedExpression(kind) => {
                write!(f, "unsupported expression in interpreter v1: {kind}")
            }
            InterpreterError::UndefinedVariable(name) => {
                write!(f, "undefined variable: {name}")
            }
            InterpreterError::ImmutableBinding(name) => {
                write!(f, "cannot assign to immutable binding: {name}")
            }
            InterpreterError::TypeError(msg) => write!(f, "type error: {msg}"),
            InterpreterError::PatternMismatch(msg) => write!(f, "pattern mismatch: {msg}"),
            InterpreterError::ReplayTargetNotFound(target) => {
                write!(f, "replay target checkpoint not found: {target}")
            }
            InterpreterError::ReplayMutationOutsideReplay => {
                write!(f, "replay modify can only be used inside replay {{ ... }}")
            }
            InterpreterError::Runtime(msg) => write!(f, "runtime error: {msg}"),
        }
    }
}

impl std::error::Error for InterpreterError {}
