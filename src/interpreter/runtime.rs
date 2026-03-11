use super::*;
use crate::ptypes::{Block, FunctionDecl, Item, Program};

impl<B: JitBackend> Interpreter<B> {
    pub fn with_backend(backend: B) -> Self {
        Self {
            backend,
            functions: HashMap::new(),
            scopes: vec![Scope::default()],
            temporal: TemporalState::default(),
        }
    }

    pub fn temporal_state(&self) -> &TemporalState {
        &self.temporal
    }

    pub fn run_program(&mut self, program: &Program) -> InterpResult<Value> {
        self.reset_runtime();
        self.register_program(program)?;
        let entry =
            Self::resolve_entry_name(program).ok_or(InterpreterError::EntryPointNotFound)?;
        self.call_function(&entry, Vec::new())
    }

    pub fn run_entrypoint(&mut self, program: &Program, entry: &str) -> InterpResult<Value> {
        self.reset_runtime();
        self.register_program(program)?;
        self.call_function(entry, Vec::new())
    }

    pub(super) fn reset_runtime(&mut self) {
        self.functions.clear();
        self.scopes.clear();
        self.scopes.push(Scope::default());
        self.temporal = TemporalState::default();
    }

    pub(super) fn register_program(&mut self, program: &Program) -> InterpResult<()> {
        self.backend.warmup(program)?;
        for item in &program.items {
            if let Item::Function(function) = item {
                self.functions
                    .insert(function.name.clone(), function.clone());
            }
        }
        Ok(())
    }

    pub(super) fn resolve_entry_name(program: &Program) -> Option<String> {
        for item in &program.items {
            if let Item::Function(function) = item {
                if function
                    .attributes
                    .iter()
                    .any(|attr| attr == "@entry" || attr.starts_with("@entry("))
                {
                    return Some(function.name.clone());
                }
            }
        }

        program.items.iter().find_map(|item| {
            if let Item::Function(function) = item {
                if function.name == "main" {
                    return Some(function.name.clone());
                }
            }
            None
        })
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(Scope::default());
        }
    }

    pub(super) fn define_var(&mut self, name: String, value: Value, mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.values.insert(name, Binding { value, mutable });
        }
    }

    pub(super) fn get_var(&self, name: &str) -> InterpResult<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.values.get(name) {
                return Ok(binding.value.clone());
            }
        }
        Err(InterpreterError::UndefinedVariable(name.to_string()))
    }

    pub(super) fn assign_var(
        &mut self,
        name: &str,
        value: Value,
        allow_define: bool,
        ignore_mutability: bool,
    ) -> InterpResult<()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.values.get_mut(name) {
                if !binding.mutable && !ignore_mutability {
                    return Err(InterpreterError::ImmutableBinding(name.to_string()));
                }
                binding.value = value;
                return Ok(());
            }
        }

        if allow_define {
            self.define_var(name.to_string(), value, true);
            return Ok(());
        }

        Err(InterpreterError::UndefinedVariable(name.to_string()))
    }

    pub(super) fn call_function(&mut self, name: &str, args: Vec<Value>) -> InterpResult<Value> {
        if let Some(value) = self.call_builtin(name, &args)? {
            return Ok(value);
        }

        let function = self
            .functions
            .get(name)
            .cloned()
            .ok_or_else(|| InterpreterError::FunctionNotFound(name.to_string()))?;

        if let Some(jit_value) = self.backend.execute(&function, &args)? {
            return Ok(jit_value);
        }

        self.invoke_function(function, args)
    }

    pub(super) fn invoke_function(&mut self, function: FunctionDecl, args: Vec<Value>) -> InterpResult<Value> {
        if args.len() > function.params.len() {
            return Err(InterpreterError::InvalidArgumentCount {
                function: function.name,
                expected: function.params.len(),
                found: args.len(),
            });
        }

        self.push_scope();
        let result = (|| {
            for (idx, param) in function.params.iter().enumerate() {
                let arg_value = if let Some(value) = args.get(idx) {
                    value.clone()
                } else if let Some(default_expr) = &param.default {
                    self.eval_expr(default_expr)?
                } else {
                    return Err(InterpreterError::InvalidArgumentCount {
                        function: function.name.clone(),
                        expected: function.params.len(),
                        found: args.len(),
                    });
                };

                self.bind_pattern(&param.pattern, arg_value, false)?;
            }

            if let Some(body) = &function.body {
                let outcome = self.exec_block(body, false)?;
                match outcome.signal {
                    ExecSignal::None => Ok(outcome.last_value),
                    ExecSignal::Return(value) => Ok(value),
                    ExecSignal::Break => Err(InterpreterError::Runtime(
                        "break used outside loop".to_string(),
                    )),
                    ExecSignal::Continue => Err(InterpreterError::Runtime(
                        "continue used outside loop".to_string(),
                    )),
                }
            } else {
                Ok(Value::Unit)
            }
        })();
        self.pop_scope();
        result
    }

    pub(super) fn call_builtin(&mut self, name: &str, args: &[Value]) -> InterpResult<Option<Value>> {
        match name {
            "print" => {
                let rendered = args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ");
                print!("{rendered}");
                Ok(Some(Value::Unit))
            }
            "println" => {
                let rendered = args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ");
                println!("{rendered}");
                Ok(Some(Value::Unit))
            }
            "len" => {
                if args.len() != 1 {
                    return Err(InterpreterError::InvalidArgumentCount {
                        function: "len".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let len = match &args[0] {
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
                Ok(Some(Value::Int(len as i64)))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn exec_block(&mut self, block: &Block, scoped: bool) -> InterpResult<BlockOutcome> {
        if scoped {
            self.push_scope();
        }

        let result = (|| {
            let mut last_value = Value::Unit;

            for stmt in &block.stmts {
                let outcome = self.exec_stmt(stmt)?;
                if let Some(value) = outcome.expr_value {
                    last_value = value;
                }

                match outcome.signal {
                    ExecSignal::None => {}
                    signal => {
                        return Ok(BlockOutcome { signal, last_value });
                    }
                }
            }

            Ok(BlockOutcome {
                signal: ExecSignal::None,
                last_value,
            })
        })();

        if scoped {
            self.pop_scope();
        }

        result
    }
}
