use crate::ptypes::{FunctionDecl, Program};

use super::{InterpreterError, Value};

pub trait JitBackend {
    fn warmup(&mut self, _program: &Program) -> Result<(), InterpreterError> {
        Ok(())
    }

    fn execute(
        &mut self,
        _function: &FunctionDecl,
        _args: &[Value],
    ) -> Result<Option<Value>, InterpreterError> {
        Ok(None)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NoopJitBackend;

impl JitBackend for NoopJitBackend {}
