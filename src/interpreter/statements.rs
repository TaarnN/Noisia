use super::*;
use crate::ptypes::Stmt;

impl<B: JitBackend> Interpreter<B> {
    pub(super) fn exec_stmt(&mut self, stmt: &Stmt) -> InterpResult<StmtOutcome> {
        match stmt {
            Stmt::Attributed { stmt, .. } => self.exec_stmt(stmt),
            Stmt::Using { target } => {
                self.eval_expr(target)?;
                Ok(StmtOutcome::none())
            }
            Stmt::Let {
                pattern,
                mutable,
                expr,
                ..
            } => {
                let value = match expr {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Unit,
                };
                self.bind_pattern(pattern, value, *mutable)?;
                Ok(StmtOutcome::none())
            }
            Stmt::Expr(expr) => {
                let value = self.eval_expr(expr)?;
                Ok(StmtOutcome::expr(value))
            }
            Stmt::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Unit,
                };
                Ok(StmtOutcome {
                    signal: ExecSignal::Return(value),
                    expr_value: None,
                })
            }
            Stmt::Continue => Ok(StmtOutcome {
                signal: ExecSignal::Continue,
                expr_value: None,
            }),
            Stmt::Break => Ok(StmtOutcome {
                signal: ExecSignal::Break,
                expr_value: None,
            }),
            Stmt::TryCatch {
                try_block,
                catch_name,
                catch_block,
            } => {
                let try_result = self.exec_block(try_block, true);
                match try_result {
                    Ok(outcome) => Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    }),
                    Err(err) => {
                        self.push_scope();
                        self.define_var(catch_name.clone(), Value::String(err.to_string()), false);
                        let catch_result = self.exec_block(catch_block, false);
                        self.pop_scope();

                        let catch_outcome = catch_result?;
                        Ok(StmtOutcome {
                            signal: catch_outcome.signal,
                            expr_value: Some(catch_outcome.last_value),
                        })
                    }
                }
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                if self.eval_expr(cond)?.is_truthy() {
                    let outcome = self.exec_block(then_block, true)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                } else if let Some(else_block) = else_block {
                    let outcome = self.exec_block(else_block, true)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                } else {
                    Ok(StmtOutcome::none())
                }
            }
            Stmt::While { cond, body } => {
                let mut iterations = 0usize;
                loop {
                    if iterations >= MAX_LOOP_ITERATIONS {
                        return Err(InterpreterError::Runtime(format!(
                            "while loop exceeded {} iterations",
                            MAX_LOOP_ITERATIONS
                        )));
                    }
                    iterations += 1;

                    if !self.eval_expr(cond)?.is_truthy() {
                        break;
                    }

                    let outcome = self.exec_block(body, true)?;
                    match outcome.signal {
                        ExecSignal::None => {}
                        ExecSignal::Continue => continue,
                        ExecSignal::Break => break,
                        ExecSignal::Return(value) => {
                            return Ok(StmtOutcome {
                                signal: ExecSignal::Return(value),
                                expr_value: None,
                            })
                        }
                    }
                }

                Ok(StmtOutcome::none())
            }
            Stmt::For {
                pattern,
                iter,
                body,
            } => {
                let iterable = self.eval_expr(iter)?;
                let values = self.into_iterable(iterable)?;

                for item in values {
                    self.push_scope();
                    self.bind_pattern(pattern, item, false)?;
                    let outcome = self.exec_block(body, false);
                    self.pop_scope();

                    let outcome = outcome?;
                    match outcome.signal {
                        ExecSignal::None => {}
                        ExecSignal::Continue => continue,
                        ExecSignal::Break => break,
                        ExecSignal::Return(value) => {
                            return Ok(StmtOutcome {
                                signal: ExecSignal::Return(value),
                                expr_value: None,
                            })
                        }
                    }
                }

                Ok(StmtOutcome::none())
            }
            Stmt::Loop { body } => {
                let mut iterations = 0usize;
                loop {
                    if iterations >= MAX_LOOP_ITERATIONS {
                        return Err(InterpreterError::Runtime(format!(
                            "loop exceeded {} iterations",
                            MAX_LOOP_ITERATIONS
                        )));
                    }
                    iterations += 1;

                    let outcome = self.exec_block(body, true)?;
                    match outcome.signal {
                        ExecSignal::None | ExecSignal::Continue => continue,
                        ExecSignal::Break => break,
                        ExecSignal::Return(value) => {
                            return Ok(StmtOutcome {
                                signal: ExecSignal::Return(value),
                                expr_value: None,
                            })
                        }
                    }
                }

                Ok(StmtOutcome::none())
            }
            Stmt::Match { expr, arms } => {
                let target = self.eval_expr(expr)?;
                let value = self.eval_match_arms(&target, arms)?;
                Ok(StmtOutcome::expr(value.unwrap_or(Value::Unit)))
            }
            Stmt::Watch { variables, clauses } => {
                for variable in variables {
                    self.eval_expr(variable)?;
                }

                for clause in clauses {
                    if self.eval_expr(&clause.condition)?.is_truthy() {
                        let outcome = self.exec_block(&clause.body, true)?;
                        return Ok(StmtOutcome {
                            signal: outcome.signal,
                            expr_value: Some(outcome.last_value),
                        });
                    }
                }

                Ok(StmtOutcome::none())
            }
            Stmt::Converge { body, until, .. } => {
                let mut iterations = 0usize;
                loop {
                    if iterations >= MAX_LOOP_ITERATIONS {
                        return Err(InterpreterError::Runtime(format!(
                            "converge exceeded {} iterations",
                            MAX_LOOP_ITERATIONS
                        )));
                    }
                    iterations += 1;

                    let outcome = self.exec_block(body, true)?;
                    match outcome.signal {
                        ExecSignal::None => {}
                        signal => {
                            return Ok(StmtOutcome {
                                signal,
                                expr_value: Some(outcome.last_value),
                            })
                        }
                    }

                    if self.eval_expr(until)?.is_truthy() {
                        break;
                    }
                }

                Ok(StmtOutcome::none())
            }
            Stmt::InContext { target, arms } => {
                let target_value = self.eval_expr(target)?;
                for arm in arms {
                    let arm_value = self.eval_expr(&arm.value)?;
                    if arm_value == target_value {
                        let outcome = self.exec_block(&arm.body, true)?;
                        return Ok(StmtOutcome {
                            signal: outcome.signal,
                            expr_value: Some(outcome.last_value),
                        });
                    }
                }
                Ok(StmtOutcome::none())
            }
            Stmt::Within {
                condition,
                body,
                fallback,
                ..
            } => {
                if let Some(condition) = condition {
                    if !self.eval_expr(condition)?.is_truthy() {
                        if let Some(fallback) = fallback {
                            return Ok(StmtOutcome::expr(self.eval_expr(fallback)?));
                        }
                        return Ok(StmtOutcome::none());
                    }
                }

                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Atomically { body } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Trap {
                error_condition,
                body,
            } => {
                if self.eval_expr(error_condition)?.is_truthy() {
                    let outcome = self.exec_block(body, true)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                } else {
                    Ok(StmtOutcome::none())
                }
            }
            Stmt::Guard {
                condition,
                then_block,
                else_block,
            } => {
                if self.eval_expr(condition)?.is_truthy() {
                    let outcome = self.exec_block(then_block, true)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                } else if let Some(else_block) = else_block {
                    let outcome = self.exec_block(else_block, true)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                } else {
                    Ok(StmtOutcome::none())
                }
            }
            Stmt::Poll { body, .. } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Delete { expr } => {
                self.delete_binding(expr)?;
                Ok(StmtOutcome::none())
            }
            Stmt::Joins { decls, body } => {
                self.push_scope();
                let result = (|| {
                    for decl in decls {
                        let outcome = self.exec_stmt(decl)?;
                        match outcome.signal {
                            ExecSignal::None => {}
                            signal => {
                                return Ok(StmtOutcome {
                                    signal,
                                    expr_value: outcome.expr_value,
                                })
                            }
                        }
                    }
                    let outcome = self.exec_block(body, false)?;
                    Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    })
                })();
                self.pop_scope();
                result
            }
            Stmt::MatchCond { arms } => {
                for arm in arms {
                    if self.eval_expr(&arm.condition)?.is_truthy() {
                        let outcome = self.exec_block(&arm.body, true)?;
                        return Ok(StmtOutcome {
                            signal: outcome.signal,
                            expr_value: Some(outcome.last_value),
                        });
                    }
                }
                Ok(StmtOutcome::none())
            }
            Stmt::OnSequence { body, .. } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::PanicUnless { condition } => {
                if !self.eval_expr(condition)?.is_truthy() {
                    return Err(InterpreterError::Runtime(
                        "panic unless condition failed".to_string(),
                    ));
                }
                Ok(StmtOutcome::none())
            }
            Stmt::Scope { body } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Defer { expr } => {
                self.eval_expr(expr)?;
                Ok(StmtOutcome::none())
            }
            Stmt::Checkpoint {
                name,
                metadata,
                body,
                preserve,
            } => {
                let outcome = self.exec_block(body, true)?;
                if let Some(preserve) = preserve {
                    self.exec_block(preserve, true)?;
                }
                let metadata_value = metadata
                    .as_ref()
                    .map(|expr| self.eval_expr(expr))
                    .transpose()?;
                self.create_checkpoint(name.clone(), metadata_value);
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Rewind {
                subject,
                target,
                condition,
                query,
            } => {
                let rewound = self.execute_rewind(subject, target, condition, query)?;
                Ok(StmtOutcome::expr(Value::Bool(rewound)))
            }
            Stmt::Inspect {
                target,
                filter,
                body,
            } => {
                if let Some(target) = target {
                    self.eval_expr(target)?;
                }
                if let Some(filter) = filter {
                    self.exec_block(filter, true)?;
                }
                if let Some(body) = body {
                    let outcome = self.exec_block(body, true)?;
                    return Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    });
                }
                Ok(StmtOutcome::none())
            }
            Stmt::TemporalScope { body, .. }
            | Stmt::TemporalTest { body, .. }
            | Stmt::TemporalMemory { body, .. }
            | Stmt::BatchTemporal { body, .. }
            | Stmt::CompileTimeBlock { body }
            | Stmt::Cleanup { body } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::TemporalTransaction {
                body,
                catch_name,
                catch_block,
                ..
            } => {
                let run_body = self.exec_block(body, true);
                match run_body {
                    Ok(outcome) => Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    }),
                    Err(err) => {
                        if let Some(catch_block) = catch_block {
                            self.push_scope();
                            if let Some(name) = catch_name {
                                self.define_var(
                                    name.clone(),
                                    Value::String(err.to_string()),
                                    false,
                                );
                            }
                            let catch_outcome = self.exec_block(catch_block, false);
                            self.pop_scope();

                            let catch_outcome = catch_outcome?;
                            Ok(StmtOutcome {
                                signal: catch_outcome.signal,
                                expr_value: Some(catch_outcome.last_value),
                            })
                        } else {
                            Err(err)
                        }
                    }
                }
            }
            Stmt::Replay { recording, body } => self.execute_replay(recording, body),
            Stmt::ReplayPause { each, checkpoint } => {
                let checkpoint = checkpoint
                    .as_ref()
                    .map(|expr| self.eval_expr(expr))
                    .transpose()?
                    .map(|value| self.checkpoint_selector_to_label(&value))
                    .transpose()?;

                self.temporal.events.push(TemporalEvent::ReplayPaused {
                    each: *each,
                    checkpoint,
                });
                Ok(StmtOutcome::none())
            }
            Stmt::ReplayOnCheckpoint { checkpoint, body } => {
                let Some(active_label) = self
                    .temporal
                    .replay
                    .as_ref()
                    .map(|replay| replay.active_label.clone())
                else {
                    return Ok(StmtOutcome::none());
                };
                let checkpoint_value = self.eval_expr(checkpoint)?;
                let checkpoint_label = self.checkpoint_selector_to_label(&checkpoint_value)?;
                if checkpoint_label == active_label {
                    let outcome = self.exec_block(body, true)?;
                    return Ok(StmtOutcome {
                        signal: outcome.signal,
                        expr_value: Some(outcome.last_value),
                    });
                }
                Ok(StmtOutcome::none())
            }
            Stmt::ReplayModify { target, value } => {
                if self.temporal.replay.is_none() {
                    return Err(InterpreterError::ReplayMutationOutsideReplay);
                }

                let value = self.eval_expr(value)?;
                self.assign_target(
                    target,
                    value,
                    AssignOptions {
                        allow_define: true,
                        ignore_mutability: true,
                    },
                )?;

                self.temporal.events.push(TemporalEvent::ReplayModified {
                    target: self.assignment_target_name(target),
                });
                Ok(StmtOutcome::none())
            }
            Stmt::Snapshot { name, metadata } => {
                let metadata_value = metadata
                    .as_ref()
                    .map(|expr| self.eval_expr(expr))
                    .transpose()?;
                self.create_checkpoint(name.clone(), metadata_value);
                Ok(StmtOutcome::none())
            }
            Stmt::Rollback {
                subject,
                target,
                condition,
                query,
                ..
            } => {
                let rewound = self.execute_rewind(subject, target, condition, query)?;
                Ok(StmtOutcome::expr(Value::Bool(rewound)))
            }
            Stmt::MergeBranch { branch, target } => {
                self.eval_expr(branch)?;
                self.eval_expr(target)?;
                Ok(StmtOutcome::none())
            }
            Stmt::TemporalRetry { max_times, body } => {
                let max_times_value = self.eval_expr(max_times)?;
                let max_times = self.expect_int(max_times_value, "retry count")?;
                if max_times <= 0 {
                    return Ok(StmtOutcome::none());
                }

                let mut last_error = None;
                for _ in 0..max_times {
                    match self.exec_block(body, true) {
                        Ok(outcome) => {
                            return Ok(StmtOutcome {
                                signal: outcome.signal,
                                expr_value: Some(outcome.last_value),
                            })
                        }
                        Err(err) => {
                            last_error = Some(err);
                        }
                    }
                }

                Err(last_error.unwrap_or_else(|| {
                    InterpreterError::Runtime("retry failed without error detail".to_string())
                }))
            }
            Stmt::AutoCheckpoint { body } => {
                self.create_checkpoint(None, None);
                let outcome = self.exec_block(body, true)?;
                self.create_checkpoint(None, None);
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Emit { target, body } => {
                self.eval_expr(target)?;
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::GcTemporal { filter } => {
                if let Some(filter) = filter {
                    self.exec_block(filter, true)?;
                } else {
                    self.temporal.checkpoints.clear();
                    self.temporal.events.push(TemporalEvent::Gc);
                }
                Ok(StmtOutcome::none())
            }
            Stmt::AssertTemporal { body, kind } => {
                let outcome = self.exec_block(body, true)?;
                if !outcome.last_value.is_truthy() {
                    return Err(InterpreterError::Runtime(format!(
                        "assert temporal {kind} failed"
                    )));
                }
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::Commit { metadata } => {
                let metadata_value = metadata
                    .as_ref()
                    .map(|expr| self.eval_expr(expr))
                    .transpose()?;
                self.create_checkpoint(Some("commit".to_string()), metadata_value);
                Ok(StmtOutcome::none())
            }
            Stmt::DebugTemporal { .. } => Ok(StmtOutcome::none()),
            Stmt::TemporalHandle { body, .. } => {
                let outcome = self.exec_block(body, true)?;
                Ok(StmtOutcome {
                    signal: outcome.signal,
                    expr_value: Some(outcome.last_value),
                })
            }
            Stmt::TemporalMatch { target, .. } => {
                self.eval_expr(target)?;
                Ok(StmtOutcome::none())
            }
        }
    }
}
