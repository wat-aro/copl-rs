use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalML5BinOp, EvalML5Binding, EvalML5Derivation, EvalML5Env, EvalML5Expr, EvalML5Judgment,
    EvalML5Pattern, EvalML5Value,
};

pub(super) fn prove_judgment(judgment: EvalML5Judgment) -> Result<EvalML5Derivation, CheckError> {
    match &judgment {
        EvalML5Judgment::EvalTo {
            env,
            expr,
            value: expected_value,
        } => {
            let Some(actual) = prove_expr(env, expr) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };
            let Some((_, _, actual_value)) = as_eval_to(&actual.judgment) else {
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
        EvalML5Judgment::PlusIs {
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
            |left, right, result| EvalML5Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalML5Judgment::MinusIs {
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
            |left, right, result| EvalML5Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalML5Judgment::TimesIs {
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
            |left, right, result| EvalML5Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalML5Judgment::LessThanIs {
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
                    Some(EvalML5Judgment::LessThanIs {
                        left: *left,
                        right: *right,
                        result: expected_result,
                    }),
                ))
            }
        }
        EvalML5Judgment::Matches {
            pattern,
            value,
            bindings: expected_bindings,
        } => {
            let Some(actual) = prove_match(pattern, value) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };
            let Some((_, _, actual_bindings)) = as_matches(&actual.judgment) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };

            if actual_bindings == expected_bindings {
                Ok(actual)
            } else {
                Err(non_derivable_judgment_error(
                    &judgment,
                    Some(actual.judgment.clone()),
                ))
            }
        }
        EvalML5Judgment::NotMatch { pattern, value } => {
            let Some(actual) = prove_not_match(pattern, value) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };

            if actual.judgment == judgment {
                Ok(actual)
            } else {
                Err(non_derivable_judgment_error(
                    &judgment,
                    Some(actual.judgment.clone()),
                ))
            }
        }
    }
}

fn prove_checked_int_binary_judgment(
    actual_judgment: EvalML5Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalML5Judgment,
) -> Result<EvalML5Derivation, CheckError> {
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

fn prove_expr(env: &EvalML5Env, expr: &EvalML5Expr) -> Option<EvalML5Derivation> {
    match expr {
        EvalML5Expr::Int(value) => Some(derivation(
            EvalML5Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML5Value::Int(*value),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalML5Expr::Bool(value) => Some(derivation(
            EvalML5Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML5Value::Bool(*value),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalML5Expr::Var(name) => prove_var(env, name),
        EvalML5Expr::Nil => Some(derivation(
            EvalML5Judgment::EvalTo {
                env: env.clone(),
                expr: EvalML5Expr::Nil,
                value: EvalML5Value::Nil,
            },
            "E-Nil",
            Vec::new(),
        )),
        EvalML5Expr::Cons { head, tail } => {
            let first = prove_expr(env, head)?;
            let second = prove_expr(env, tail)?;
            let first_value = as_eval_to_value(&first.judgment)?.clone();
            let second_value = as_eval_to_value(&second.judgment)?.clone();

            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML5Value::Cons {
                        head: Box::new(first_value),
                        tail: Box::new(second_value),
                    },
                },
                "E-Cons",
                vec![first, second],
            ))
        }
        EvalML5Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_derivation = prove_expr(env, condition)?;
            let condition_value = as_eval_to_bool(&condition_derivation.judgment)?;
            if condition_value {
                let branch_derivation = prove_expr(env, then_branch)?;
                let value = as_eval_to_value(&branch_derivation.judgment)?.clone();
                Some(derivation(
                    EvalML5Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfT",
                    vec![condition_derivation, branch_derivation],
                ))
            } else {
                let branch_derivation = prove_expr(env, else_branch)?;
                let value = as_eval_to_value(&branch_derivation.judgment)?.clone();
                Some(derivation(
                    EvalML5Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfF",
                    vec![condition_derivation, branch_derivation],
                ))
            }
        }
        EvalML5Expr::Let {
            name,
            bound_expr,
            body,
        } => {
            let first = prove_expr(env, bound_expr)?;
            let first_value = as_eval_to_value(&first.judgment)?.clone();
            let extended_env = push_binding(env, name, &first_value);
            let second = prove_expr(&extended_env, body)?;
            let value = as_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-Let",
                vec![first, second],
            ))
        }
        EvalML5Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let recursive_value = EvalML5Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let extended_env = push_binding(env, name, &recursive_value);
            let first = prove_expr(&extended_env, body)?;
            let value = as_eval_to_value(&first.judgment)?.clone();
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-LetRec",
                vec![first],
            ))
        }
        EvalML5Expr::Fun { param, body } => Some(derivation(
            EvalML5Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML5Value::Closure {
                    env: env.clone(),
                    param: param.clone(),
                    body: body.as_ref().clone(),
                },
            },
            "E-Fun",
            Vec::new(),
        )),
        EvalML5Expr::App { func, arg } => {
            let first = prove_expr(env, func)?;
            let second = prove_expr(env, arg)?;
            let second_value = as_eval_to_value(&second.judgment)?;
            let first_value = as_eval_to_value(&first.judgment)?;

            match first_value {
                EvalML5Value::Closure {
                    env: closure_env,
                    param,
                    body,
                } => {
                    let third_env = push_binding(closure_env, param, second_value);
                    let third = prove_expr(&third_env, body)?;
                    let value = as_eval_to_value(&third.judgment)?.clone();
                    Some(derivation(
                        EvalML5Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            value,
                        },
                        "E-App",
                        vec![first, second, third],
                    ))
                }
                EvalML5Value::RecClosure {
                    env: closure_env,
                    name,
                    param,
                    body,
                } => {
                    let recursive_value = first_value.clone();
                    let third_env = push_binding(
                        &push_binding(closure_env, name, &recursive_value),
                        param,
                        second_value,
                    );
                    let third = prove_expr(&third_env, body)?;
                    let value = as_eval_to_value(&third.judgment)?.clone();
                    Some(derivation(
                        EvalML5Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            value,
                        },
                        "E-AppRec",
                        vec![first, second, third],
                    ))
                }
                _ => None,
            }
        }
        EvalML5Expr::Match { scrutinee, clauses } => {
            prove_match_expr(env, expr, scrutinee, clauses)
        }
        EvalML5Expr::BinOp { op, left, right } => prove_binop(env, expr, *op, left, right),
    }
}

fn prove_var(env: &EvalML5Env, name: &str) -> Option<EvalML5Derivation> {
    let value = env
        .0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| binding.value.clone())?;

    Some(derivation(
        EvalML5Judgment::EvalTo {
            env: env.clone(),
            expr: EvalML5Expr::Var(name.to_string()),
            value,
        },
        "E-Var",
        Vec::new(),
    ))
}

fn prove_match_expr(
    env: &EvalML5Env,
    expr: &EvalML5Expr,
    scrutinee: &EvalML5Expr,
    clauses: &[super::syntax::EvalML5MatchClause],
) -> Option<EvalML5Derivation> {
    let first_clause = clauses.first()?;
    let first = prove_expr(env, scrutinee)?;
    let scrutinee_value = as_eval_to_value(&first.judgment)?.clone();

    if let Some(second) = prove_match(&first_clause.pattern, &scrutinee_value) {
        let (_, _, delta) = as_matches(&second.judgment)?;
        let third_env = append_env(env, delta);
        let third = prove_expr(&third_env, &first_clause.body)?;
        let value = as_eval_to_value(&third.judgment)?.clone();

        let rule_name = if clauses.len() == 1 {
            "E-MatchM1"
        } else {
            "E-MatchM2"
        };

        return Some(derivation(
            EvalML5Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value,
            },
            rule_name,
            vec![first, second, third],
        ));
    }

    if clauses.len() == 1 {
        return None;
    }

    let second = prove_not_match(&first_clause.pattern, &scrutinee_value)?;
    let rest_expr = EvalML5Expr::Match {
        scrutinee: Box::new(scrutinee.clone()),
        clauses: clauses[1..].to_vec(),
    };
    let third = prove_expr(env, &rest_expr)?;
    let value = as_eval_to_value(&third.judgment)?.clone();

    Some(derivation(
        EvalML5Judgment::EvalTo {
            env: env.clone(),
            expr: expr.clone(),
            value,
        },
        "E-MatchN",
        vec![first, second, third],
    ))
}

fn prove_match(pattern: &EvalML5Pattern, value: &EvalML5Value) -> Option<EvalML5Derivation> {
    match (pattern, value) {
        (EvalML5Pattern::Var(name), value) => Some(derivation(
            EvalML5Judgment::Matches {
                pattern: EvalML5Pattern::Var(name.clone()),
                value: value.clone(),
                bindings: EvalML5Env(vec![EvalML5Binding {
                    name: name.clone(),
                    value: value.clone(),
                }]),
            },
            "M-Var",
            Vec::new(),
        )),
        (EvalML5Pattern::Wildcard, value) => Some(derivation(
            EvalML5Judgment::Matches {
                pattern: EvalML5Pattern::Wildcard,
                value: value.clone(),
                bindings: EvalML5Env::default(),
            },
            "M-Wild",
            Vec::new(),
        )),
        (EvalML5Pattern::Nil, EvalML5Value::Nil) => Some(derivation(
            EvalML5Judgment::Matches {
                pattern: EvalML5Pattern::Nil,
                value: EvalML5Value::Nil,
                bindings: EvalML5Env::default(),
            },
            "M-Nil",
            Vec::new(),
        )),
        (
            EvalML5Pattern::Cons { head, tail },
            EvalML5Value::Cons {
                head: head_value,
                tail: tail_value,
            },
        ) => {
            let first = prove_match(head, head_value)?;
            let second = prove_match(tail, tail_value)?;
            let first_bindings = as_match_bindings(&first.judgment)?;
            let second_bindings = as_match_bindings(&second.judgment)?;

            Some(derivation(
                EvalML5Judgment::Matches {
                    pattern: pattern.clone(),
                    value: value.clone(),
                    bindings: append_env(first_bindings, second_bindings),
                },
                "M-Cons",
                vec![first, second],
            ))
        }
        _ => None,
    }
}

fn prove_not_match(pattern: &EvalML5Pattern, value: &EvalML5Value) -> Option<EvalML5Derivation> {
    match (pattern, value) {
        (EvalML5Pattern::Nil, EvalML5Value::Cons { .. }) => Some(derivation(
            EvalML5Judgment::NotMatch {
                pattern: EvalML5Pattern::Nil,
                value: value.clone(),
            },
            "NM-ConsNil",
            Vec::new(),
        )),
        (
            EvalML5Pattern::Cons { head, tail },
            EvalML5Value::Cons {
                head: head_value,
                tail: tail_value,
            },
        ) => {
            if let Some(first) = prove_not_match(head, head_value) {
                return Some(derivation(
                    EvalML5Judgment::NotMatch {
                        pattern: pattern.clone(),
                        value: value.clone(),
                    },
                    "NM-ConsConsL",
                    vec![first],
                ));
            }

            if let Some(first) = prove_not_match(tail, tail_value) {
                return Some(derivation(
                    EvalML5Judgment::NotMatch {
                        pattern: pattern.clone(),
                        value: value.clone(),
                    },
                    "NM-ConsConsR",
                    vec![first],
                ));
            }

            None
        }
        _ => None,
    }
}

fn prove_binop(
    env: &EvalML5Env,
    expr: &EvalML5Expr,
    op: EvalML5BinOp,
    left: &EvalML5Expr,
    right: &EvalML5Expr,
) -> Option<EvalML5Derivation> {
    let first = prove_expr(env, left)?;
    let second = prove_expr(env, right)?;
    let left_int = as_eval_to_int(&first.judgment)?;
    let right_int = as_eval_to_int(&second.judgment)?;

    match op {
        EvalML5BinOp::Plus => {
            let result = left_int.checked_add(right_int)?;
            let third = derivation(
                EvalML5Judgment::PlusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML5Value::Int(result),
                },
                "E-Plus",
                vec![first, second, third],
            ))
        }
        EvalML5BinOp::Minus => {
            let result = left_int.checked_sub(right_int)?;
            let third = derivation(
                EvalML5Judgment::MinusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML5Value::Int(result),
                },
                "E-Minus",
                vec![first, second, third],
            ))
        }
        EvalML5BinOp::Times => {
            let result = left_int.checked_mul(right_int)?;
            let third = derivation(
                EvalML5Judgment::TimesIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML5Value::Int(result),
                },
                "E-Times",
                vec![first, second, third],
            ))
        }
        EvalML5BinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalML5Judgment::LessThanIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML5Value::Bool(result),
                },
                "E-Lt",
                vec![first, second, third],
            ))
        }
    }
}

fn push_binding(env: &EvalML5Env, name: &str, value: &EvalML5Value) -> EvalML5Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalML5Binding {
        name: name.to_string(),
        value: value.clone(),
    });
    EvalML5Env(bindings)
}

fn append_env(left: &EvalML5Env, right: &EvalML5Env) -> EvalML5Env {
    let mut bindings = left.0.clone();
    bindings.extend(right.0.iter().cloned());
    EvalML5Env(bindings)
}

fn as_eval_to(judgment: &EvalML5Judgment) -> Option<(&EvalML5Env, &EvalML5Expr, &EvalML5Value)> {
    let EvalML5Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_value(judgment: &EvalML5Judgment) -> Option<&EvalML5Value> {
    let (_, _, value) = as_eval_to(judgment)?;
    Some(value)
}

fn as_eval_to_int(judgment: &EvalML5Judgment) -> Option<i64> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalML5Value::Int(value_int) = value else {
        return None;
    };
    Some(*value_int)
}

fn as_eval_to_bool(judgment: &EvalML5Judgment) -> Option<bool> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalML5Value::Bool(value_bool) = value else {
        return None;
    };
    Some(*value_bool)
}

fn as_matches(judgment: &EvalML5Judgment) -> Option<(&EvalML5Pattern, &EvalML5Value, &EvalML5Env)> {
    let EvalML5Judgment::Matches {
        pattern,
        value,
        bindings,
    } = judgment
    else {
        return None;
    };
    Some((pattern, value, bindings))
}

fn as_match_bindings(judgment: &EvalML5Judgment) -> Option<&EvalML5Env> {
    let (_, _, bindings) = as_matches(judgment)?;
    Some(bindings)
}

fn non_derivable_judgment_error(
    actual: &EvalML5Judgment,
    expected: Option<EvalML5Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML5 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML5 (actual: {actual}; fix: check environment bindings, operand/value types, operator constraints, and pattern-matching rules)"
        )),
    }
}

fn fix_message(actual: &EvalML5Judgment, expected: &EvalML5Judgment) -> String {
    match (actual, expected) {
        (
            EvalML5Judgment::EvalTo {
                expr: _,
                env: _,
                value: actual_value,
            },
            EvalML5Judgment::EvalTo {
                expr: _,
                env: _,
                value: expected_value,
            },
        ) => {
            if actual_value == expected_value {
                "fix: check environment and expression/value forms".to_string()
            } else {
                format!("fix: replace value with {expected_value}")
            }
        }
        (
            EvalML5Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalML5Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML5Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalML5Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML5Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalML5Judgment::TimesIs {
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
            EvalML5Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalML5Judgment::LessThanIs {
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
            EvalML5Judgment::Matches {
                bindings: actual_bindings,
                ..
            },
            EvalML5Judgment::Matches {
                bindings: expected_bindings,
                ..
            },
        ) => {
            if actual_bindings == expected_bindings {
                "fix: check pattern and value forms".to_string()
            } else if expected_bindings.0.is_empty() {
                "fix: replace bindings with ()".to_string()
            } else {
                format!("fix: replace bindings with ({expected_bindings})")
            }
        }
        _ => "fix: check the judgment terms and operator".to_string(),
    }
}

fn derivation(
    judgment: EvalML5Judgment,
    rule_name: &str,
    subderivations: Vec<EvalML5Derivation>,
) -> EvalML5Derivation {
    EvalML5Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, EvalML5Derivation};
    use crate::games::eval_ml5::syntax::{
        EvalML5BinOp, EvalML5Binding, EvalML5Env, EvalML5Expr, EvalML5Judgment, EvalML5MatchClause,
        EvalML5Pattern, EvalML5Value,
    };

    #[test]
    fn proves_eval_int_judgment_with_e_int() {
        let derivation = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env::default(),
            expr: EvalML5Expr::Int(3),
            value: EvalML5Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_let_rec_judgment_with_e_let_rec() {
        let derivation = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env::default(),
            expr: EvalML5Expr::LetRec {
                name: "f".to_string(),
                param: "x".to_string(),
                fun_body: Box::new(EvalML5Expr::BinOp {
                    op: EvalML5BinOp::Plus,
                    left: Box::new(EvalML5Expr::Var("x".to_string())),
                    right: Box::new(EvalML5Expr::Int(1)),
                }),
                body: Box::new(EvalML5Expr::App {
                    func: Box::new(EvalML5Expr::Var("f".to_string())),
                    arg: Box::new(EvalML5Expr::Int(2)),
                }),
            },
            value: EvalML5Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-LetRec");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "E-AppRec");
    }

    #[test]
    fn proves_eval_match_judgment_with_e_match_n() {
        let derivation = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env::default(),
            expr: EvalML5Expr::Match {
                scrutinee: Box::new(EvalML5Expr::Cons {
                    head: Box::new(EvalML5Expr::Int(1)),
                    tail: Box::new(EvalML5Expr::Nil),
                }),
                clauses: vec![
                    EvalML5MatchClause {
                        pattern: EvalML5Pattern::Nil,
                        body: EvalML5Expr::Int(0),
                    },
                    EvalML5MatchClause {
                        pattern: EvalML5Pattern::Cons {
                            head: Box::new(EvalML5Pattern::Var("x".to_string())),
                            tail: Box::new(EvalML5Pattern::Var("xs".to_string())),
                        },
                        body: EvalML5Expr::Var("x".to_string()),
                    },
                ],
            },
            value: EvalML5Value::Int(1),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-MatchN");
        assert_eq!(derivation.subderivations.len(), 3);
    }

    #[test]
    fn proves_pattern_match_judgment_with_m_cons() {
        let derivation = prove_judgment(EvalML5Judgment::Matches {
            pattern: EvalML5Pattern::Cons {
                head: Box::new(EvalML5Pattern::Var("x".to_string())),
                tail: Box::new(EvalML5Pattern::Wildcard),
            },
            value: EvalML5Value::Cons {
                head: Box::new(EvalML5Value::Int(1)),
                tail: Box::new(EvalML5Value::Nil),
            },
            bindings: EvalML5Env(vec![EvalML5Binding {
                name: "x".to_string(),
                value: EvalML5Value::Int(1),
            }]),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "M-Cons");
        assert_eq!(derivation.subderivations.len(), 2);
    }

    #[test]
    fn proves_pattern_not_match_judgment_with_nm_cons_cons_l() {
        let derivation = prove_judgment(EvalML5Judgment::NotMatch {
            pattern: EvalML5Pattern::Cons {
                head: Box::new(EvalML5Pattern::Nil),
                tail: Box::new(EvalML5Pattern::Var("xs".to_string())),
            },
            value: EvalML5Value::Cons {
                head: Box::new(EvalML5Value::Cons {
                    head: Box::new(EvalML5Value::Int(1)),
                    tail: Box::new(EvalML5Value::Nil),
                }),
                tail: Box::new(EvalML5Value::Nil),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "NM-ConsConsL");
        assert_eq!(derivation.subderivations.len(), 1);
    }

    #[test]
    fn proves_var_with_nearest_binding_by_e_var() {
        let derivation = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env(vec![
                EvalML5Binding {
                    name: "x".to_string(),
                    value: EvalML5Value::Int(1),
                },
                EvalML5Binding {
                    name: "x".to_string(),
                    value: EvalML5Value::Int(2),
                },
            ]),
            expr: EvalML5Expr::Var("x".to_string()),
            value: EvalML5Value::Int(2),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Var");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_builtin_plus_judgment_with_b_plus() {
        let derivation = prove_judgment(EvalML5Judgment::PlusIs {
            left: 3,
            right: 5,
            result: 8,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "B-Plus");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn rejects_non_derivable_eval_judgment() {
        let err = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env::default(),
            expr: EvalML5Expr::BinOp {
                op: EvalML5BinOp::Plus,
                left: Box::new(EvalML5Expr::Int(3)),
                right: Box::new(EvalML5Expr::Int(5)),
            },
            value: EvalML5Value::Int(7),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML5"));
        assert!(err
            .message()
            .contains("expected: |- 3 + 5 evalto 8, actual: |- 3 + 5 evalto 7"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn rejects_ill_typed_eval_judgment() {
        let err = prove_judgment(EvalML5Judgment::EvalTo {
            env: EvalML5Env::default(),
            expr: EvalML5Expr::BinOp {
                op: EvalML5BinOp::Plus,
                left: Box::new(EvalML5Expr::Bool(true)),
                right: Box::new(EvalML5Expr::Int(1)),
            },
            value: EvalML5Value::Int(2),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML5"));
        assert!(err.message().contains(
            "fix: check environment bindings, operand/value types, operator constraints, and pattern-matching rules"
        ));
    }

    #[test]
    fn rejects_non_derivable_builtin_judgment() {
        let err = prove_judgment(EvalML5Judgment::TimesIs {
            left: 2,
            right: 3,
            result: 5,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML5"));
        assert!(err
            .message()
            .contains("expected: 2 times 3 is 6, actual: 2 times 3 is 5"));
        assert!(err.message().contains("fix: replace result term with 6"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_078() {
        let expected =
            parse_source(include_str!("../../../copl/078.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalML5Derivation, expected: &EvalML5Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
