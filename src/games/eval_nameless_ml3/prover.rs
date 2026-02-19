use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalNamelessML3BinOp, EvalNamelessML3Derivation, EvalNamelessML3Env, EvalNamelessML3Expr,
    EvalNamelessML3Judgment, EvalNamelessML3Value,
};

pub(super) fn prove_judgment(
    judgment: EvalNamelessML3Judgment,
) -> Result<EvalNamelessML3Derivation, CheckError> {
    match &judgment {
        EvalNamelessML3Judgment::EvalTo {
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
        EvalNamelessML3Judgment::PlusIs {
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
            |left, right, result| EvalNamelessML3Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalNamelessML3Judgment::MinusIs {
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
            |left, right, result| EvalNamelessML3Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalNamelessML3Judgment::TimesIs {
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
            |left, right, result| EvalNamelessML3Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalNamelessML3Judgment::LessThanIs {
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
                    Some(EvalNamelessML3Judgment::LessThanIs {
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
    actual_judgment: EvalNamelessML3Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalNamelessML3Judgment,
) -> Result<EvalNamelessML3Derivation, CheckError> {
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

fn prove_expr(
    env: &EvalNamelessML3Env,
    expr: &EvalNamelessML3Expr,
) -> Option<EvalNamelessML3Derivation> {
    match expr {
        EvalNamelessML3Expr::Int(value) => Some(derivation(
            EvalNamelessML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalNamelessML3Value::Int(*value),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalNamelessML3Expr::Bool(value) => Some(derivation(
            EvalNamelessML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalNamelessML3Value::Bool(*value),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalNamelessML3Expr::Index(index) => prove_var(env, *index),
        EvalNamelessML3Expr::If {
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
                    EvalNamelessML3Judgment::EvalTo {
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
                    EvalNamelessML3Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfF",
                    vec![condition_derivation, branch_derivation],
                ))
            }
        }
        EvalNamelessML3Expr::Let { bound_expr, body } => {
            let first = prove_expr(env, bound_expr)?;
            let first_value = as_eval_to_value(&first.judgment)?.clone();
            let extended_env = push_value(env, &first_value);
            let second = prove_expr(&extended_env, body)?;
            let value = as_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-Let",
                vec![first, second],
            ))
        }
        EvalNamelessML3Expr::LetRec { fun_body, body } => {
            let recursive_value = EvalNamelessML3Value::RecClosure {
                env: env.clone(),
                body: fun_body.as_ref().clone(),
            };
            let extended_env = push_value(env, &recursive_value);
            let first = prove_expr(&extended_env, body)?;
            let value = as_eval_to_value(&first.judgment)?.clone();
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-LetRec",
                vec![first],
            ))
        }
        EvalNamelessML3Expr::Fun { body } => Some(derivation(
            EvalNamelessML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalNamelessML3Value::Closure {
                    env: env.clone(),
                    body: body.as_ref().clone(),
                },
            },
            "E-Fun",
            Vec::new(),
        )),
        EvalNamelessML3Expr::App { func, arg } => {
            let first = prove_expr(env, func)?;
            let second = prove_expr(env, arg)?;
            let second_value = as_eval_to_value(&second.judgment)?;
            let first_value = as_eval_to_value(&first.judgment)?;

            match first_value {
                EvalNamelessML3Value::Closure {
                    env: closure_env,
                    body,
                } => {
                    let third_env = push_value(closure_env, second_value);
                    let third = prove_expr(&third_env, body)?;
                    let value = as_eval_to_value(&third.judgment)?.clone();
                    Some(derivation(
                        EvalNamelessML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            value,
                        },
                        "E-App",
                        vec![first, second, third],
                    ))
                }
                EvalNamelessML3Value::RecClosure {
                    env: closure_env,
                    body,
                } => {
                    let recursive_value = first_value.clone();
                    let third_env =
                        push_value(&push_value(closure_env, &recursive_value), second_value);
                    let third = prove_expr(&third_env, body)?;
                    let value = as_eval_to_value(&third.judgment)?.clone();
                    Some(derivation(
                        EvalNamelessML3Judgment::EvalTo {
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
        EvalNamelessML3Expr::BinOp { op, left, right } => prove_binop(env, expr, *op, left, right),
    }
}

fn prove_var(env: &EvalNamelessML3Env, index: usize) -> Option<EvalNamelessML3Derivation> {
    let value = resolve_index(env, index)?;
    Some(derivation(
        EvalNamelessML3Judgment::EvalTo {
            env: env.clone(),
            expr: EvalNamelessML3Expr::Index(index),
            value,
        },
        "E-Var",
        Vec::new(),
    ))
}

fn resolve_index(env: &EvalNamelessML3Env, index: usize) -> Option<EvalNamelessML3Value> {
    if index == 0 || index > env.0.len() {
        return None;
    }

    Some(env.0[env.0.len() - index].clone())
}

fn prove_binop(
    env: &EvalNamelessML3Env,
    expr: &EvalNamelessML3Expr,
    op: EvalNamelessML3BinOp,
    left: &EvalNamelessML3Expr,
    right: &EvalNamelessML3Expr,
) -> Option<EvalNamelessML3Derivation> {
    let first = prove_expr(env, left)?;
    let second = prove_expr(env, right)?;
    let left_int = as_eval_to_int(&first.judgment)?;
    let right_int = as_eval_to_int(&second.judgment)?;

    match op {
        EvalNamelessML3BinOp::Plus => {
            let result = left_int.checked_add(right_int)?;
            let third = derivation(
                EvalNamelessML3Judgment::PlusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalNamelessML3Value::Int(result),
                },
                "E-Plus",
                vec![first, second, third],
            ))
        }
        EvalNamelessML3BinOp::Minus => {
            let result = left_int.checked_sub(right_int)?;
            let third = derivation(
                EvalNamelessML3Judgment::MinusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalNamelessML3Value::Int(result),
                },
                "E-Minus",
                vec![first, second, third],
            ))
        }
        EvalNamelessML3BinOp::Times => {
            let result = left_int.checked_mul(right_int)?;
            let third = derivation(
                EvalNamelessML3Judgment::TimesIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalNamelessML3Value::Int(result),
                },
                "E-Times",
                vec![first, second, third],
            ))
        }
        EvalNamelessML3BinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalNamelessML3Judgment::LessThanIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalNamelessML3Value::Bool(result),
                },
                "E-Lt",
                vec![first, second, third],
            ))
        }
    }
}

fn push_value(env: &EvalNamelessML3Env, value: &EvalNamelessML3Value) -> EvalNamelessML3Env {
    let mut values = env.0.clone();
    values.push(value.clone());
    EvalNamelessML3Env(values)
}

fn as_eval_to(
    judgment: &EvalNamelessML3Judgment,
) -> Option<(
    &EvalNamelessML3Env,
    &EvalNamelessML3Expr,
    &EvalNamelessML3Value,
)> {
    let EvalNamelessML3Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_value(judgment: &EvalNamelessML3Judgment) -> Option<&EvalNamelessML3Value> {
    let (_, _, value) = as_eval_to(judgment)?;
    Some(value)
}

fn as_eval_to_int(judgment: &EvalNamelessML3Judgment) -> Option<i64> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalNamelessML3Value::Int(value_int) = value else {
        return None;
    };
    Some(*value_int)
}

fn as_eval_to_bool(judgment: &EvalNamelessML3Judgment) -> Option<bool> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalNamelessML3Value::Bool(value_bool) = value else {
        return None;
    };
    Some(*value_bool)
}

fn non_derivable_judgment_error(
    actual: &EvalNamelessML3Judgment,
    expected: Option<EvalNamelessML3Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalNamelessML3 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalNamelessML3 (actual: {actual}; fix: check environment bindings, operand/value types, and operator constraints)"
        )),
    }
}

fn fix_message(actual: &EvalNamelessML3Judgment, expected: &EvalNamelessML3Judgment) -> String {
    match (actual, expected) {
        (
            EvalNamelessML3Judgment::EvalTo {
                expr: _,
                env: _,
                value: actual_value,
            },
            EvalNamelessML3Judgment::EvalTo {
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
            EvalNamelessML3Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalNamelessML3Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalNamelessML3Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalNamelessML3Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalNamelessML3Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalNamelessML3Judgment::TimesIs {
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
            EvalNamelessML3Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalNamelessML3Judgment::LessThanIs {
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
    judgment: EvalNamelessML3Judgment,
    rule_name: &str,
    subderivations: Vec<EvalNamelessML3Derivation>,
) -> EvalNamelessML3Derivation {
    EvalNamelessML3Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, EvalNamelessML3Derivation};
    use crate::games::eval_nameless_ml3::syntax::{
        EvalNamelessML3BinOp, EvalNamelessML3Env, EvalNamelessML3Expr, EvalNamelessML3Judgment,
        EvalNamelessML3Value,
    };

    #[test]
    fn proves_eval_int_judgment_with_e_int() {
        let derivation = prove_judgment(EvalNamelessML3Judgment::EvalTo {
            env: EvalNamelessML3Env::default(),
            expr: EvalNamelessML3Expr::Int(3),
            value: EvalNamelessML3Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_let_rec_judgment_with_e_let_rec() {
        let derivation = prove_judgment(EvalNamelessML3Judgment::EvalTo {
            env: EvalNamelessML3Env::default(),
            expr: EvalNamelessML3Expr::LetRec {
                fun_body: Box::new(EvalNamelessML3Expr::BinOp {
                    op: EvalNamelessML3BinOp::Plus,
                    left: Box::new(EvalNamelessML3Expr::Index(1)),
                    right: Box::new(EvalNamelessML3Expr::Int(1)),
                }),
                body: Box::new(EvalNamelessML3Expr::App {
                    func: Box::new(EvalNamelessML3Expr::Index(1)),
                    arg: Box::new(EvalNamelessML3Expr::Int(2)),
                }),
            },
            value: EvalNamelessML3Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-LetRec");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "E-AppRec");
    }

    #[test]
    fn proves_builtin_plus_judgment_with_b_plus() {
        let derivation = prove_judgment(EvalNamelessML3Judgment::PlusIs {
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
        let err = prove_judgment(EvalNamelessML3Judgment::EvalTo {
            env: EvalNamelessML3Env::default(),
            expr: EvalNamelessML3Expr::BinOp {
                op: EvalNamelessML3BinOp::Plus,
                left: Box::new(EvalNamelessML3Expr::Int(3)),
                right: Box::new(EvalNamelessML3Expr::Int(5)),
            },
            value: EvalNamelessML3Value::Int(7),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalNamelessML3"));
        assert!(err
            .message()
            .contains("expected: |- 3 + 5 evalto 8, actual: |- 3 + 5 evalto 7"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn rejects_ill_typed_eval_judgment() {
        let err = prove_judgment(EvalNamelessML3Judgment::EvalTo {
            env: EvalNamelessML3Env::default(),
            expr: EvalNamelessML3Expr::BinOp {
                op: EvalNamelessML3BinOp::Plus,
                left: Box::new(EvalNamelessML3Expr::Bool(true)),
                right: Box::new(EvalNamelessML3Expr::Int(1)),
            },
            value: EvalNamelessML3Value::Int(2),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalNamelessML3"));
        assert!(err.message().contains(
            "fix: check environment bindings, operand/value types, and operator constraints"
        ));
    }

    #[test]
    fn rejects_non_derivable_builtin_judgment() {
        let err = prove_judgment(EvalNamelessML3Judgment::TimesIs {
            left: 2,
            right: 3,
            result: 5,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalNamelessML3"));
        assert!(err
            .message()
            .contains("expected: 2 times 3 is 6, actual: 2 times 3 is 5"));
        assert!(err.message().contains("fix: replace result term with 6"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_057() {
        let expected =
            parse_source(include_str!("../../../copl/057.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalNamelessML3Derivation, expected: &EvalNamelessML3Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
