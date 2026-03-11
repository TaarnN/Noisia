mod error;
mod expressions;
#[cfg(feature = "inkwell-jit")]
mod inkwell_backend;
mod jit;
mod patterns;
mod runtime;
mod statements;
mod temporal;
mod value;

pub use error::InterpreterError;
#[cfg(feature = "inkwell-jit")]
pub use inkwell_backend::InkwellJitBackend;
pub use jit::{JitBackend, NoopJitBackend};
pub use value::Value;

use crate::ptypes::FunctionDecl;
use std::collections::HashMap;

const MAX_LOOP_ITERATIONS: usize = 100_000;

type InterpResult<T> = Result<T, InterpreterError>;

#[derive(Debug, Clone)]
struct Binding {
    value: Value,
    mutable: bool,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    values: HashMap<String, Binding>,
}

#[derive(Debug, Clone)]
pub struct TemporalCheckpoint {
    pub id: usize,
    pub name: Option<String>,
    pub metadata: Option<Value>,
}

#[derive(Debug, Clone)]
struct TemporalSnapshot {
    checkpoint: TemporalCheckpoint,
    scopes: Vec<Scope>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemporalEvent {
    CheckpointCreated {
        id: usize,
        name: Option<String>,
    },
    Rewind {
        target: String,
    },
    ReplayStarted {
        from: String,
    },
    ReplayFinished {
        from: String,
    },
    ReplayPaused {
        each: bool,
        checkpoint: Option<String>,
    },
    ReplayModified {
        target: String,
    },
    Gc,
}

#[derive(Debug, Clone)]
struct ReplayContext {
    active_label: String,
}

#[derive(Debug, Clone, Default)]
pub struct TemporalState {
    checkpoints: Vec<TemporalSnapshot>,
    events: Vec<TemporalEvent>,
    next_id: usize,
    replay: Option<ReplayContext>,
}

impl TemporalState {
    pub fn checkpoints(&self) -> Vec<TemporalCheckpoint> {
        self.checkpoints
            .iter()
            .map(|snapshot| snapshot.checkpoint.clone())
            .collect()
    }

    pub fn events(&self) -> &[TemporalEvent] {
        &self.events
    }
}

#[derive(Debug)]
enum ExecSignal {
    None,
    Return(Value),
    Break,
    Continue,
}

#[derive(Debug)]
struct StmtOutcome {
    signal: ExecSignal,
    expr_value: Option<Value>,
}

impl StmtOutcome {
    fn none() -> Self {
        Self {
            signal: ExecSignal::None,
            expr_value: None,
        }
    }

    fn expr(value: Value) -> Self {
        Self {
            signal: ExecSignal::None,
            expr_value: Some(value),
        }
    }
}

#[derive(Debug)]
struct BlockOutcome {
    signal: ExecSignal,
    last_value: Value,
}

#[derive(Debug, Clone, Copy)]
struct AssignOptions {
    allow_define: bool,
    ignore_mutability: bool,
}

impl AssignOptions {
    const DEFAULT: Self = Self {
        allow_define: false,
        ignore_mutability: false,
    };
}

pub struct Interpreter<B: JitBackend = NoopJitBackend> {
    backend: B,
    functions: HashMap<String, FunctionDecl>,
    scopes: Vec<Scope>,
    temporal: TemporalState,
}

impl Default for Interpreter<NoopJitBackend> {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter<NoopJitBackend> {
    pub fn new() -> Self {
        Self::with_backend(NoopJitBackend)
    }
}
