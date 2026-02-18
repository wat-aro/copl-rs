use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalML3BinOp, EvalML3Binding, EvalML3Derivation, EvalML3Env, EvalML3Expr, EvalML3Judgment,
    EvalML3Value,
};

pub(super) fn prove_judgment(judgment: EvalML3Judgment) -> Result<EvalML3Derivation, CheckError> {
    match &judgment {
        EvalML3Judgment::EvalTo {
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
        EvalML3Judgment::PlusIs {
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
            |left, right, result| EvalML3Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalML3Judgment::MinusIs {
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
            |left, right, result| EvalML3Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalML3Judgment::TimesIs {
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
            |left, right, result| EvalML3Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalML3Judgment::LessThanIs {
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
                    Some(EvalML3Judgment::LessThanIs {
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
    actual_judgment: EvalML3Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalML3Judgment,
) -> Result<EvalML3Derivation, CheckError> {
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

fn prove_expr(env: &EvalML3Env, expr: &EvalML3Expr) -> Option<EvalML3Derivation> {
    match expr {
        EvalML3Expr::Int(value) => Some(derivation(
            EvalML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML3Value::Int(*value),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalML3Expr::Bool(value) => Some(derivation(
            EvalML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML3Value::Bool(*value),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalML3Expr::Var(name) => prove_var(env, name),
        EvalML3Expr::If {
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
                    EvalML3Judgment::EvalTo {
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
                    EvalML3Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfF",
                    vec![condition_derivation, branch_derivation],
                ))
            }
        }
        EvalML3Expr::Let {
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
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-Let",
                vec![first, second],
            ))
        }
        EvalML3Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let recursive_value = EvalML3Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let extended_env = push_binding(env, name, &recursive_value);
            let first = prove_expr(&extended_env, body)?;
            let value = as_eval_to_value(&first.judgment)?.clone();
            Some(derivation(
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value,
                },
                "E-LetRec",
                vec![first],
            ))
        }
        EvalML3Expr::Fun { param, body } => Some(derivation(
            EvalML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                value: EvalML3Value::Closure {
                    env: env.clone(),
                    param: param.clone(),
                    body: body.as_ref().clone(),
                },
            },
            "E-Fun",
            Vec::new(),
        )),
        EvalML3Expr::App { func, arg } => {
            let first = prove_expr(env, func)?;
            let second = prove_expr(env, arg)?;
            let second_value = as_eval_to_value(&second.judgment)?;
            let first_value = as_eval_to_value(&first.judgment)?;

            match first_value {
                EvalML3Value::Closure {
                    env: closure_env,
                    param,
                    body,
                } => {
                    let third_env = push_binding(closure_env, param, second_value);
                    let third = prove_expr(&third_env, body)?;
                    let value = as_eval_to_value(&third.judgment)?.clone();
                    Some(derivation(
                        EvalML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            value,
                        },
                        "E-App",
                        vec![first, second, third],
                    ))
                }
                EvalML3Value::RecClosure {
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
                        EvalML3Judgment::EvalTo {
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
        EvalML3Expr::BinOp { op, left, right } => prove_binop(env, expr, *op, left, right),
    }
}

fn prove_var(env: &EvalML3Env, name: &str) -> Option<EvalML3Derivation> {
    let (prefix, last) = split_last_binding(env)?;
    if last.name == name {
        return Some(derivation(
            EvalML3Judgment::EvalTo {
                env: env.clone(),
                expr: EvalML3Expr::Var(name.to_string()),
                value: last.value.clone(),
            },
            "E-Var1",
            Vec::new(),
        ));
    }

    let prefix_env = EvalML3Env(prefix.to_vec());
    let first = prove_var(&prefix_env, name)?;
    let value = as_eval_to_value(&first.judgment)?.clone();
    Some(derivation(
        EvalML3Judgment::EvalTo {
            env: env.clone(),
            expr: EvalML3Expr::Var(name.to_string()),
            value,
        },
        "E-Var2",
        vec![first],
    ))
}

fn prove_binop(
    env: &EvalML3Env,
    expr: &EvalML3Expr,
    op: EvalML3BinOp,
    left: &EvalML3Expr,
    right: &EvalML3Expr,
) -> Option<EvalML3Derivation> {
    let first = prove_expr(env, left)?;
    let second = prove_expr(env, right)?;
    let left_int = as_eval_to_int(&first.judgment)?;
    let right_int = as_eval_to_int(&second.judgment)?;

    match op {
        EvalML3BinOp::Plus => {
            let result = left_int.checked_add(right_int)?;
            let third = derivation(
                EvalML3Judgment::PlusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML3Value::Int(result),
                },
                "E-Plus",
                vec![first, second, third],
            ))
        }
        EvalML3BinOp::Minus => {
            let result = left_int.checked_sub(right_int)?;
            let third = derivation(
                EvalML3Judgment::MinusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML3Value::Int(result),
                },
                "E-Minus",
                vec![first, second, third],
            ))
        }
        EvalML3BinOp::Times => {
            let result = left_int.checked_mul(right_int)?;
            let third = derivation(
                EvalML3Judgment::TimesIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML3Value::Int(result),
                },
                "E-Times",
                vec![first, second, third],
            ))
        }
        EvalML3BinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalML3Judgment::LessThanIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    value: EvalML3Value::Bool(result),
                },
                "E-Lt",
                vec![first, second, third],
            ))
        }
    }
}

fn split_last_binding(env: &EvalML3Env) -> Option<(&[EvalML3Binding], &EvalML3Binding)> {
    let (last, prefix) = env.0.split_last()?;
    Some((prefix, last))
}

fn push_binding(env: &EvalML3Env, name: &str, value: &EvalML3Value) -> EvalML3Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalML3Binding {
        name: name.to_string(),
        value: value.clone(),
    });
    EvalML3Env(bindings)
}

fn as_eval_to(judgment: &EvalML3Judgment) -> Option<(&EvalML3Env, &EvalML3Expr, &EvalML3Value)> {
    let EvalML3Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_value(judgment: &EvalML3Judgment) -> Option<&EvalML3Value> {
    let (_, _, value) = as_eval_to(judgment)?;
    Some(value)
}

fn as_eval_to_int(judgment: &EvalML3Judgment) -> Option<i64> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalML3Value::Int(value_int) = value else {
        return None;
    };
    Some(*value_int)
}

fn as_eval_to_bool(judgment: &EvalML3Judgment) -> Option<bool> {
    let (_, _, value) = as_eval_to(judgment)?;
    let EvalML3Value::Bool(value_bool) = value else {
        return None;
    };
    Some(*value_bool)
}

fn non_derivable_judgment_error(
    actual: &EvalML3Judgment,
    expected: Option<EvalML3Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML3 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML3 (actual: {actual}; fix: check environment bindings, operand/value types, and operator constraints)"
        )),
    }
}

fn fix_message(actual: &EvalML3Judgment, expected: &EvalML3Judgment) -> String {
    match (actual, expected) {
        (
            EvalML3Judgment::EvalTo {
                expr: _,
                env: _,
                value: actual_value,
            },
            EvalML3Judgment::EvalTo {
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
            EvalML3Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalML3Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML3Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalML3Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML3Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalML3Judgment::TimesIs {
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
            EvalML3Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalML3Judgment::LessThanIs {
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
    judgment: EvalML3Judgment,
    rule_name: &str,
    subderivations: Vec<EvalML3Derivation>,
) -> EvalML3Derivation {
    EvalML3Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, EvalML3Derivation};
    use crate::games::eval_ml3::syntax::{
        EvalML3BinOp, EvalML3Env, EvalML3Expr, EvalML3Judgment, EvalML3Value,
    };

    #[test]
    fn proves_eval_int_judgment_with_e_int() {
        let derivation = prove_judgment(EvalML3Judgment::EvalTo {
            env: EvalML3Env::default(),
            expr: EvalML3Expr::Int(3),
            value: EvalML3Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_let_rec_judgment_with_e_let_rec() {
        let derivation = prove_judgment(EvalML3Judgment::EvalTo {
            env: EvalML3Env::default(),
            expr: EvalML3Expr::LetRec {
                name: "f".to_string(),
                param: "x".to_string(),
                fun_body: Box::new(EvalML3Expr::BinOp {
                    op: EvalML3BinOp::Plus,
                    left: Box::new(EvalML3Expr::Var("x".to_string())),
                    right: Box::new(EvalML3Expr::Int(1)),
                }),
                body: Box::new(EvalML3Expr::App {
                    func: Box::new(EvalML3Expr::Var("f".to_string())),
                    arg: Box::new(EvalML3Expr::Int(2)),
                }),
            },
            value: EvalML3Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-LetRec");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "E-AppRec");
    }

    #[test]
    fn proves_builtin_plus_judgment_with_b_plus() {
        let derivation = prove_judgment(EvalML3Judgment::PlusIs {
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
        let err = prove_judgment(EvalML3Judgment::EvalTo {
            env: EvalML3Env::default(),
            expr: EvalML3Expr::BinOp {
                op: EvalML3BinOp::Plus,
                left: Box::new(EvalML3Expr::Int(3)),
                right: Box::new(EvalML3Expr::Int(5)),
            },
            value: EvalML3Value::Int(7),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML3"));
        assert!(err
            .message()
            .contains("expected: |- 3 + 5 evalto 8, actual: |- 3 + 5 evalto 7"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn rejects_ill_typed_eval_judgment() {
        let err = prove_judgment(EvalML3Judgment::EvalTo {
            env: EvalML3Env::default(),
            expr: EvalML3Expr::BinOp {
                op: EvalML3BinOp::Plus,
                left: Box::new(EvalML3Expr::Bool(true)),
                right: Box::new(EvalML3Expr::Int(1)),
            },
            value: EvalML3Value::Int(2),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML3"));
        assert!(err.message().contains(
            "fix: check environment bindings, operand/value types, and operator constraints"
        ));
    }

    #[test]
    fn rejects_non_derivable_builtin_judgment() {
        let err = prove_judgment(EvalML3Judgment::TimesIs {
            left: 2,
            right: 3,
            result: 5,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML3"));
        assert!(err
            .message()
            .contains("expected: 2 times 3 is 6, actual: 2 times 3 is 5"));
        assert!(err.message().contains("fix: replace result term with 6"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_042() {
        let expected =
            parse_source(include_str!("../../../copl/042.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalML3Derivation, expected: &EvalML3Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
