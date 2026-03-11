use super::*;
use crate::ptypes::{Block, Expr};

impl<B: JitBackend> Interpreter<B> {
    pub(super) fn execute_replay(&mut self, recording: &Expr, body: &Block) -> InterpResult<StmtOutcome> {
        let recording_value = self.eval_expr(recording)?;
        let checkpoint_index = self.resolve_checkpoint_index(Some(&recording_value))?;
        let snapshot = self
            .temporal
            .checkpoints
            .get(checkpoint_index)
            .cloned()
            .ok_or_else(|| {
                InterpreterError::ReplayTargetNotFound(
                    self.value_to_checkpoint_target(&recording_value),
                )
            })?;

        let label = self.snapshot_label(&snapshot);
        self.temporal.events.push(TemporalEvent::ReplayStarted {
            from: label.clone(),
        });

        let previous_scopes = self.scopes.clone();
        let previous_replay = self.temporal.replay.clone();
        self.scopes = snapshot.scopes.clone();
        self.temporal.replay = Some(ReplayContext {
            active_label: label.clone(),
        });

        let replay_result = self.exec_block(body, true);

        self.scopes = previous_scopes;
        self.temporal.replay = previous_replay;
        self.temporal
            .events
            .push(TemporalEvent::ReplayFinished { from: label });

        let outcome = replay_result?;
        Ok(StmtOutcome {
            signal: outcome.signal,
            expr_value: Some(outcome.last_value),
        })
    }

    pub(super) fn execute_rewind(
        &mut self,
        subject: &Option<Expr>,
        target: &Option<Expr>,
        condition: &Option<Expr>,
        query: &Option<Block>,
    ) -> InterpResult<bool> {
        if let Some(subject) = subject {
            self.eval_expr(subject)?;
        }

        if let Some(condition) = condition {
            if !self.eval_expr(condition)?.is_truthy() {
                return Ok(false);
            }
        }

        if let Some(query) = query {
            let query_result = self.eval_block_expr(query)?;
            if !query_result.is_truthy() {
                return Ok(false);
            }
        }

        let target_value = target
            .as_ref()
            .map(|expr| self.eval_expr(expr))
            .transpose()?;

        self.rewind_to(target_value.as_ref())
    }

    pub(super) fn rewind_to(&mut self, target: Option<&Value>) -> InterpResult<bool> {
        if self.temporal.checkpoints.is_empty() {
            return Ok(false);
        }

        let checkpoint_index = self.resolve_checkpoint_index(target)?;
        let snapshot = self.temporal.checkpoints[checkpoint_index].clone();
        let label = self.snapshot_label(&snapshot);
        self.scopes = snapshot.scopes;

        if let Some(replay) = self.temporal.replay.as_mut() {
            replay.active_label = label.clone();
        }

        self.temporal
            .events
            .push(TemporalEvent::Rewind { target: label });
        Ok(true)
    }

    pub(super) fn create_checkpoint(&mut self, name: Option<String>, metadata: Option<Value>) {
        let id = self.temporal.next_id;
        self.temporal.next_id += 1;

        let checkpoint = TemporalCheckpoint {
            id,
            name: name.clone(),
            metadata,
        };

        let snapshot = TemporalSnapshot {
            checkpoint,
            scopes: self.scopes.clone(),
        };

        let label = self.snapshot_label(&snapshot);
        self.temporal.events.push(TemporalEvent::CheckpointCreated {
            id,
            name: name.clone(),
        });

        if let Some(replay) = self.temporal.replay.as_mut() {
            replay.active_label = label;
        }

        self.temporal.checkpoints.push(snapshot);
    }

    pub(super) fn resolve_checkpoint_index(&self, target: Option<&Value>) -> InterpResult<usize> {
        if self.temporal.checkpoints.is_empty() {
            return Err(InterpreterError::ReplayTargetNotFound("<none>".to_string()));
        }

        match target {
            None => Ok(self.temporal.checkpoints.len() - 1),
            Some(value) => match value {
                Value::Int(id) => self
                    .temporal
                    .checkpoints
                    .iter()
                    .position(|snapshot| snapshot.checkpoint.id == *id as usize)
                    .ok_or_else(|| {
                        InterpreterError::ReplayTargetNotFound(format!("checkpoint id {id}"))
                    }),
                Value::String(name) | Value::Phrase(name) | Value::EnumCase(name) => self
                    .temporal
                    .checkpoints
                    .iter()
                    .rposition(|snapshot| {
                        snapshot
                            .checkpoint
                            .name
                            .as_ref()
                            .map(|snapshot_name| snapshot_name == name)
                            .unwrap_or_else(|| self.snapshot_label(snapshot) == *name)
                    })
                    .ok_or_else(|| InterpreterError::ReplayTargetNotFound(name.clone())),
                other => Err(InterpreterError::TypeError(format!(
                    "invalid checkpoint selector type: {}",
                    other.type_name()
                ))),
            },
        }
    }

    pub(super) fn snapshot_label(&self, snapshot: &TemporalSnapshot) -> String {
        snapshot
            .checkpoint
            .name
            .clone()
            .unwrap_or_else(|| format!("#{}", snapshot.checkpoint.id))
    }

    pub(super) fn checkpoint_selector_to_label(&self, value: &Value) -> InterpResult<String> {
        match value {
            Value::Int(id) => {
                let snapshot = self
                    .temporal
                    .checkpoints
                    .iter()
                    .find(|snapshot| snapshot.checkpoint.id == *id as usize)
                    .ok_or_else(|| {
                        InterpreterError::ReplayTargetNotFound(format!("checkpoint id {id}"))
                    })?;
                Ok(self.snapshot_label(snapshot))
            }
            Value::String(name) | Value::Phrase(name) | Value::EnumCase(name) => Ok(name.clone()),
            other => Err(InterpreterError::TypeError(format!(
                "invalid checkpoint selector type: {}",
                other.type_name()
            ))),
        }
    }

    pub(super) fn value_to_checkpoint_target(&self, value: &Value) -> String {
        match value {
            Value::String(name) | Value::Phrase(name) | Value::EnumCase(name) => name.clone(),
            Value::Int(id) => format!("checkpoint id {id}"),
            _ => value.to_string(),
        }
    }
}
