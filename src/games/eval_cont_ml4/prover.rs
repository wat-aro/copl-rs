use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalContML4BinOp, EvalContML4Binding, EvalContML4ContFrame, EvalContML4Continuation,
    EvalContML4Derivation, EvalContML4Env, EvalContML4Expr, EvalContML4Judgment, EvalContML4Value,
};

pub(super) fn prove_judgment(
    judgment: EvalContML4Judgment,
) -> Result<EvalContML4Derivation, CheckError> {
    match &judgment {
        EvalContML4Judgment::EvalTo {
            env,
            expr,
            continuation,
            value: expected_value,
            ..
        } => {
            let Some(actual) = prove_eval_to(env, expr, continuation.clone()) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };
            let Some(actual_value) = as_eval_to_value(&actual.judgment) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };

            if actual_value == expected_value {
                Ok(actual)
            } else {
                Err(non_derivable_judgment_error(
                    &judgment,
                    Some(actual.judgment.clone()),
                ))
            }
        }
        EvalContML4Judgment::ContEvalTo {
            input,
            continuation,
            value: expected_value,
        } => {
            let Some(actual) = prove_cont_eval(input.clone(), continuation.clone()) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };
            let Some(actual_value) = as_cont_eval_to_value(&actual.judgment) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };

            if actual_value == expected_value {
                Ok(actual)
            } else {
                Err(non_derivable_judgment_error(
                    &judgment,
                    Some(actual.judgment.clone()),
                ))
            }
        }
        EvalContML4Judgment::PlusIs {
            left,
            right,
            result,
        } => prove_checked_int_binary_judgment(
            judgment.clone(),
            "B-Plus",
            left,
            right,
            *result,
            i64::checked_add,
            |left, right, result| EvalContML4Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalContML4Judgment::MinusIs {
            left,
            right,
            result,
        } => prove_checked_int_binary_judgment(
            judgment.clone(),
            "B-Minus",
            left,
            right,
            *result,
            i64::checked_sub,
            |left, right, result| EvalContML4Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalContML4Judgment::TimesIs {
            left,
            right,
            result,
        } => prove_checked_int_binary_judgment(
            judgment.clone(),
            "B-Times",
            left,
            right,
            *result,
            i64::checked_mul,
            |left, right, result| EvalContML4Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalContML4Judgment::LessThanIs {
            left,
            right,
            result,
        } => {
            let expected_result = left < right;
            if expected_result == *result {
                Ok(derivation(judgment, "B-Lt", Vec::new()))
            } else {
                Err(non_derivable_judgment_error(
                    &judgment,
                    Some(EvalContML4Judgment::LessThanIs {
                        left: *left,
                        right: *right,
                        result: expected_result,
                    }),
                ))
            }
        }
    }
}

fn prove_checked_int_binary_judgment(
    actual_judgment: EvalContML4Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalContML4Judgment,
) -> Result<EvalContML4Derivation, CheckError> {
    let Some(expected_result) = compute(*left, *right) else {
        return Err(non_derivable_judgment_error(&actual_judgment, None));
    };

    if expected_result == actual_result {
        Ok(derivation(actual_judgment, rule_name, Vec::new()))
    } else {
        Err(non_derivable_judgment_error(
            &actual_judgment,
            Some(build_judgment(*left, *right, expected_result)),
        ))
    }
}

fn prove_eval_to(
    env: &EvalContML4Env,
    expr: &EvalContML4Expr,
    continuation: EvalContML4Continuation,
) -> Option<EvalContML4Derivation> {
    match expr {
        EvalContML4Expr::Int(value) => {
            let premise = prove_cont_eval(EvalContML4Value::Int(*value), continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Int",
                vec![premise],
            ))
        }
        EvalContML4Expr::Bool(value) => {
            let premise = prove_cont_eval(EvalContML4Value::Bool(*value), continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Bool",
                vec![premise],
            ))
        }
        EvalContML4Expr::Var(name) => prove_var(env, name, continuation),
        EvalContML4Expr::Fun { param, body } => {
            let closure = EvalContML4Value::Closure {
                env: env.clone(),
                param: param.clone(),
                body: body.as_ref().clone(),
            };
            let premise = prove_cont_eval(closure, continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Fun",
                vec![premise],
            ))
        }
        EvalContML4Expr::Let {
            name,
            bound_expr,
            body,
        } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::LetBody {
                    env: Some(env.clone()),
                    name: name.clone(),
                    body: body.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, bound_expr.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Let",
                vec![premise],
            ))
        }
        EvalContML4Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let recursive = EvalContML4Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let extended_env = push_binding(env, name, &recursive);
            let premise = prove_eval_to(&extended_env, body.as_ref(), continuation.clone())?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-LetRec",
                vec![premise],
            ))
        }
        EvalContML4Expr::LetCc { name, body } => {
            let captured = EvalContML4Value::Continuation(continuation.clone());
            let extended_env = push_binding(env, name, &captured);
            let premise = prove_eval_to(&extended_env, body.as_ref(), continuation.clone())?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-LetCc",
                vec![premise],
            ))
        }
        EvalContML4Expr::BinOp { op, left, right } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::EvalR {
                    env: Some(env.clone()),
                    op: *op,
                    right: right.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, left.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-BinOp",
                vec![premise],
            ))
        }
        EvalContML4Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::If {
                    env: Some(env.clone()),
                    then_branch: then_branch.as_ref().clone(),
                    else_branch: else_branch.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, condition.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-If",
                vec![premise],
            ))
        }
        EvalContML4Expr::App { func, arg } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::EvalArg {
                    env: Some(env.clone()),
                    arg: arg.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, func.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-App",
                vec![premise],
            ))
        }
        EvalContML4Expr::Nil => {
            let premise = prove_cont_eval(EvalContML4Value::Nil, continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), EvalContML4Expr::Nil, continuation, value),
                "E-Nil",
                vec![premise],
            ))
        }
        EvalContML4Expr::Cons { head, tail } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::EvalConsR {
                    env: Some(env.clone()),
                    tail_expr: tail.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, head.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Cons",
                vec![premise],
            ))
        }
        EvalContML4Expr::Match {
            scrutinee,
            nil_case,
            head_name,
            tail_name,
            cons_case,
        } => {
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::Match {
                    env: Some(env.clone()),
                    nil_case: nil_case.as_ref().clone(),
                    head_name: head_name.clone(),
                    tail_name: tail_name.clone(),
                    cons_case: cons_case.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(env, scrutinee.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(env.clone(), expr.clone(), continuation, value),
                "E-Match",
                vec![premise],
            ))
        }
    }
}

fn prove_var(
    env: &EvalContML4Env,
    name: &str,
    continuation: EvalContML4Continuation,
) -> Option<EvalContML4Derivation> {
    let bound = lookup_env(env, name)?.clone();
    let premise = prove_cont_eval(bound, continuation.clone())?;
    let value = as_cont_eval_to_value(&premise.judgment)?.clone();
    Some(derivation(
        eval_to_judgment(
            env.clone(),
            EvalContML4Expr::Var(name.to_string()),
            continuation,
            value,
        ),
        "E-Var",
        vec![premise],
    ))
}

fn prove_cont_eval(
    input: EvalContML4Value,
    continuation: EvalContML4Continuation,
) -> Option<EvalContML4Derivation> {
    if continuation.frames.is_empty() {
        return Some(derivation(
            cont_eval_to_judgment(input.clone(), EvalContML4Continuation::hole(), input),
            "C-Ret",
            Vec::new(),
        ));
    }

    let (head, tail) = split_continuation(&continuation)?;
    match head {
        EvalContML4ContFrame::EvalR { env, op, right } => {
            let left = as_int(&input)?;
            let next_frame = match op {
                EvalContML4BinOp::Plus => EvalContML4ContFrame::Plus { left },
                EvalContML4BinOp::Minus => EvalContML4ContFrame::Minus { left },
                EvalContML4BinOp::Times => EvalContML4ContFrame::Times { left },
                EvalContML4BinOp::Lt => EvalContML4ContFrame::Lt { left },
            };
            let next_continuation = prepend_frame(next_frame, &tail);
            let eval_env = env.clone().unwrap_or_default();
            let premise = prove_eval_to(&eval_env, right, next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Int(left), continuation, value),
                "C-EvalR",
                vec![premise],
            ))
        }
        EvalContML4ContFrame::Plus { left } => {
            let right = as_int(&input)?;
            let result = left.checked_add(right)?;
            let first = derivation(
                EvalContML4Judgment::PlusIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML4Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Int(right), continuation, value),
                "C-Plus",
                vec![first, second],
            ))
        }
        EvalContML4ContFrame::Minus { left } => {
            let right = as_int(&input)?;
            let result = left.checked_sub(right)?;
            let first = derivation(
                EvalContML4Judgment::MinusIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML4Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Int(right), continuation, value),
                "C-Minus",
                vec![first, second],
            ))
        }
        EvalContML4ContFrame::Times { left } => {
            let right = as_int(&input)?;
            let result = left.checked_mul(right)?;
            let first = derivation(
                EvalContML4Judgment::TimesIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML4Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Int(right), continuation, value),
                "C-Times",
                vec![first, second],
            ))
        }
        EvalContML4ContFrame::Lt { left } => {
            let right = as_int(&input)?;
            let result = *left < right;
            let first = derivation(
                EvalContML4Judgment::LessThanIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML4Value::Bool(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Int(right), continuation, value),
                "C-Lt",
                vec![first, second],
            ))
        }
        EvalContML4ContFrame::If {
            env,
            then_branch,
            else_branch,
        } => {
            let condition = as_bool(&input)?;
            let eval_env = env.clone().unwrap_or_default();
            let (rule_name, premise) = if condition {
                ("C-IfT", prove_eval_to(&eval_env, then_branch, tail)?)
            } else {
                ("C-IfF", prove_eval_to(&eval_env, else_branch, tail)?)
            };
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML4Value::Bool(condition), continuation, value),
                rule_name,
                vec![premise],
            ))
        }
        EvalContML4ContFrame::LetBody { env, name, body } => {
            let eval_env = env.clone().unwrap_or_default();
            let extended_env = push_binding(&eval_env, name, &input);
            let premise = prove_eval_to(&extended_env, body, tail)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(input, continuation, value),
                "C-LetBody",
                vec![premise],
            ))
        }
        EvalContML4ContFrame::EvalArg { env, arg } => {
            let eval_env = env.clone().unwrap_or_default();
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::EvalFun {
                    func: input.clone(),
                },
                &tail,
            );
            let premise = prove_eval_to(&eval_env, arg, next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(input, continuation, value),
                "C-EvalArg",
                vec![premise],
            ))
        }
        EvalContML4ContFrame::EvalFun { func } => match func {
            EvalContML4Value::Closure { env, param, body } => {
                let extended_env = push_binding(env, param, &input);
                let premise = prove_eval_to(&extended_env, body, tail)?;
                let value = as_eval_to_value(&premise.judgment)?.clone();
                Some(derivation(
                    cont_eval_to_judgment(input, continuation, value),
                    "C-EvalFun",
                    vec![premise],
                ))
            }
            EvalContML4Value::RecClosure {
                env,
                name,
                param,
                body,
            } => {
                let with_rec = push_binding(env, name, func);
                let extended_env = push_binding(&with_rec, param, &input);
                let premise = prove_eval_to(&extended_env, body, tail)?;
                let value = as_eval_to_value(&premise.judgment)?.clone();
                Some(derivation(
                    cont_eval_to_judgment(input, continuation, value),
                    "C-EvalFunR",
                    vec![premise],
                ))
            }
            EvalContML4Value::Continuation(captured) => {
                let premise = prove_cont_eval(input.clone(), captured.clone())?;
                let value = as_cont_eval_to_value(&premise.judgment)?.clone();
                Some(derivation(
                    cont_eval_to_judgment(input, continuation, value),
                    "C-EvalFunC",
                    vec![premise],
                ))
            }
            _ => None,
        },
        EvalContML4ContFrame::EvalConsR { env, tail_expr } => {
            let eval_env = env.clone().unwrap_or_default();
            let next_continuation = prepend_frame(
                EvalContML4ContFrame::Cons {
                    head: input.clone(),
                },
                &tail,
            );
            let premise = prove_eval_to(&eval_env, tail_expr, next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(input, continuation, value),
                "C-EvalConsR",
                vec![premise],
            ))
        }
        EvalContML4ContFrame::Cons { head } => {
            let cons_value = EvalContML4Value::Cons {
                head: Box::new(head.clone()),
                tail: Box::new(input.clone()),
            };
            let premise = prove_cont_eval(cons_value, tail)?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(input, continuation, value),
                "C-Cons",
                vec![premise],
            ))
        }
        EvalContML4ContFrame::Match {
            env,
            nil_case,
            head_name,
            tail_name,
            cons_case,
        } => {
            let eval_env = env.clone().unwrap_or_default();
            match input.clone() {
                EvalContML4Value::Nil => {
                    let premise = prove_eval_to(&eval_env, nil_case, tail)?;
                    let value = as_eval_to_value(&premise.judgment)?.clone();
                    Some(derivation(
                        cont_eval_to_judgment(EvalContML4Value::Nil, continuation, value),
                        "C-MatchNil",
                        vec![premise],
                    ))
                }
                EvalContML4Value::Cons {
                    head: cons_head,
                    tail: cons_tail,
                } => {
                    let with_head = push_binding(&eval_env, head_name, cons_head.as_ref());
                    let with_both = push_binding(&with_head, tail_name, cons_tail.as_ref());
                    let premise = prove_eval_to(&with_both, cons_case, tail)?;
                    let value = as_eval_to_value(&premise.judgment)?.clone();
                    Some(derivation(
                        cont_eval_to_judgment(
                            EvalContML4Value::Cons {
                                head: cons_head,
                                tail: cons_tail,
                            },
                            continuation,
                            value,
                        ),
                        "C-MatchCons",
                        vec![premise],
                    ))
                }
                _ => None,
            }
        }
    }
}

fn split_continuation(
    continuation: &EvalContML4Continuation,
) -> Option<(&EvalContML4ContFrame, EvalContML4Continuation)> {
    let head = continuation.frames.first()?;
    let tail = EvalContML4Continuation {
        frames: continuation.frames.iter().skip(1).cloned().collect(),
        explicit_ret: continuation.explicit_ret,
    };
    Some((head, tail))
}

fn prepend_frame(
    frame: EvalContML4ContFrame,
    continuation: &EvalContML4Continuation,
) -> EvalContML4Continuation {
    let mut frames = vec![frame];
    frames.extend(continuation.frames.iter().cloned());
    EvalContML4Continuation {
        frames,
        explicit_ret: continuation.explicit_ret,
    }
}

fn eval_to_judgment(
    env: EvalContML4Env,
    expr: EvalContML4Expr,
    continuation: EvalContML4Continuation,
    value: EvalContML4Value,
) -> EvalContML4Judgment {
    let has_continuation = !continuation.frames.is_empty() || continuation.explicit_ret;
    EvalContML4Judgment::EvalTo {
        env,
        expr,
        continuation,
        value,
        has_continuation,
    }
}

fn cont_eval_to_judgment(
    input: EvalContML4Value,
    continuation: EvalContML4Continuation,
    value: EvalContML4Value,
) -> EvalContML4Judgment {
    EvalContML4Judgment::ContEvalTo {
        input,
        continuation,
        value,
    }
}

fn as_eval_to_value(judgment: &EvalContML4Judgment) -> Option<&EvalContML4Value> {
    let EvalContML4Judgment::EvalTo { value, .. } = judgment else {
        return None;
    };
    Some(value)
}

fn as_cont_eval_to_value(judgment: &EvalContML4Judgment) -> Option<&EvalContML4Value> {
    let EvalContML4Judgment::ContEvalTo { value, .. } = judgment else {
        return None;
    };
    Some(value)
}

fn as_int(value: &EvalContML4Value) -> Option<i64> {
    let EvalContML4Value::Int(int) = value else {
        return None;
    };
    Some(*int)
}

fn as_bool(value: &EvalContML4Value) -> Option<bool> {
    let EvalContML4Value::Bool(boolean) = value else {
        return None;
    };
    Some(*boolean)
}

fn lookup_env<'a>(env: &'a EvalContML4Env, name: &str) -> Option<&'a EvalContML4Value> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.value)
}

fn push_binding(env: &EvalContML4Env, name: &str, value: &EvalContML4Value) -> EvalContML4Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalContML4Binding {
        name: name.to_string(),
        value: value.clone(),
    });
    EvalContML4Env(bindings)
}

fn non_derivable_judgment_error(
    actual: &EvalContML4Judgment,
    expected: Option<EvalContML4Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalContML4 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalContML4 (actual: {actual}; fix: check environment/continuation shape, operand/value types, and operator constraints)"
        )),
    }
}

fn fix_message(actual: &EvalContML4Judgment, expected: &EvalContML4Judgment) -> String {
    match (actual, expected) {
        (
            EvalContML4Judgment::EvalTo {
                value: actual_value,
                ..
            },
            EvalContML4Judgment::EvalTo {
                value: expected_value,
                ..
            },
        )
        | (
            EvalContML4Judgment::ContEvalTo {
                value: actual_value,
                ..
            },
            EvalContML4Judgment::ContEvalTo {
                value: expected_value,
                ..
            },
        ) => {
            if actual_value == expected_value {
                "fix: check environment/continuation structure and expression/value forms"
                    .to_string()
            } else {
                format!("fix: replace value with {expected_value}")
            }
        }
        (
            EvalContML4Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalContML4Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalContML4Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalContML4Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalContML4Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalContML4Judgment::TimesIs {
                result: expected_result,
                ..
            },
        ) => {
            if actual_result == expected_result {
                "fix: check operands and operator".to_string()
            } else {
                format!("fix: replace result term with {expected_result}")
            }
        }
        (
            EvalContML4Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalContML4Judgment::LessThanIs {
                result: expected_result,
                ..
            },
        ) => {
            if actual_result == expected_result {
                "fix: check operands and operator".to_string()
            } else {
                format!("fix: replace result term with {expected_result}")
            }
        }
        _ => "fix: check the judgment terms and operator".to_string(),
    }
}

fn derivation(
    judgment: EvalContML4Judgment,
    rule_name: &str,
    subderivations: Vec<EvalContML4Derivation>,
) -> EvalContML4Derivation {
    EvalContML4Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::{parse_judgment_source, parse_source};
    use super::{prove_judgment, EvalContML4Derivation};
    use crate::games::eval_cont_ml4::syntax::{
        EvalContML4BinOp, EvalContML4Binding, EvalContML4ContFrame, EvalContML4Continuation,
        EvalContML4Env, EvalContML4Expr, EvalContML4Judgment, EvalContML4Value,
    };

    #[test]
    fn proves_eval_judgment_with_e_let_cc() {
        let derivation = prove_judgment(EvalContML4Judgment::EvalTo {
            env: EvalContML4Env::default(),
            expr: EvalContML4Expr::LetCc {
                name: "k".to_string(),
                body: Box::new(EvalContML4Expr::App {
                    func: Box::new(EvalContML4Expr::Var("k".to_string())),
                    arg: Box::new(EvalContML4Expr::Int(2)),
                }),
            },
            continuation: EvalContML4Continuation {
                frames: vec![EvalContML4ContFrame::Plus { left: 3 }],
                explicit_ret: false,
            },
            value: EvalContML4Value::Int(5),
            has_continuation: true,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-LetCc");
        assert!(contains_rule(&derivation, "C-EvalFunC"));
    }

    #[test]
    fn proves_var_with_nearest_binding_by_e_var() {
        let derivation = prove_judgment(EvalContML4Judgment::EvalTo {
            env: EvalContML4Env(vec![
                EvalContML4Binding {
                    name: "x".to_string(),
                    value: EvalContML4Value::Int(1),
                },
                EvalContML4Binding {
                    name: "x".to_string(),
                    value: EvalContML4Value::Int(2),
                },
            ]),
            expr: EvalContML4Expr::Var("x".to_string()),
            continuation: EvalContML4Continuation::implicit_hole(),
            value: EvalContML4Value::Int(2),
            has_continuation: false,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Var");
        assert_eq!(derivation.subderivations.len(), 1);
    }

    #[test]
    fn proves_continuation_judgment_with_c_eval_fun_c() {
        let derivation = prove_judgment(EvalContML4Judgment::ContEvalTo {
            input: EvalContML4Value::Int(2),
            continuation: EvalContML4Continuation {
                frames: vec![EvalContML4ContFrame::EvalFun {
                    func: EvalContML4Value::Continuation(EvalContML4Continuation {
                        frames: vec![EvalContML4ContFrame::Plus { left: 3 }],
                        explicit_ret: false,
                    }),
                }],
                explicit_ret: false,
            },
            value: EvalContML4Value::Int(5),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "C-EvalFunC");
        assert_eq!(derivation.subderivations.len(), 1);
    }

    #[test]
    fn proves_builtin_less_than_judgment_with_b_lt() {
        let derivation = prove_judgment(EvalContML4Judgment::LessThanIs {
            left: -3,
            right: -2,
            result: true,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "B-Lt");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn rejects_non_derivable_eval_judgment() {
        let err = prove_judgment(EvalContML4Judgment::EvalTo {
            env: EvalContML4Env::default(),
            expr: EvalContML4Expr::BinOp {
                op: EvalContML4BinOp::Plus,
                left: Box::new(EvalContML4Expr::Int(1)),
                right: Box::new(EvalContML4Expr::Int(2)),
            },
            continuation: EvalContML4Continuation::implicit_hole(),
            value: EvalContML4Value::Int(2),
            has_continuation: false,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalContML4"));
        assert!(err
            .message()
            .contains("expected: |- 1 + 2 evalto 3, actual: |- 1 + 2 evalto 2"));
        assert!(err.message().contains("fix: replace value with 3"));
    }

    #[test]
    fn rejects_non_derivable_continuation_judgment() {
        let err = prove_judgment(EvalContML4Judgment::ContEvalTo {
            input: EvalContML4Value::Int(2),
            continuation: EvalContML4Continuation {
                frames: vec![EvalContML4ContFrame::Plus { left: 3 }],
                explicit_ret: false,
            },
            value: EvalContML4Value::Int(6),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalContML4"));
        assert!(err
            .message()
            .contains("expected: 2 => {3 + _} evalto 5, actual: 2 => {3 + _} evalto 6"));
        assert!(err.message().contains("fix: replace value with 5"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_134() {
        let expected =
            parse_source(include_str!("../../../copl/134.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    #[test]
    fn keeps_negative_application_arguments_parenthesized_in_output() {
        let judgment = parse_judgment_source(include_str!("../../../copl/137.exam.copl"))
            .expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be provable");
        let text = derivation.to_string();

        assert!(text.contains("f (-2) k1 k2"));
        assert!(text.contains("|- _ (-2)"));
        assert!(!text.contains("f -2 k1 k2"));
        assert!(!text.contains("|- _ -2"));
    }

    fn contains_rule(derivation: &EvalContML4Derivation, rule_name: &str) -> bool {
        derivation.rule_name == rule_name
            || derivation
                .subderivations
                .iter()
                .any(|sub| contains_rule(sub, rule_name))
    }

    fn assert_same_shape(actual: &EvalContML4Derivation, expected: &EvalContML4Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
