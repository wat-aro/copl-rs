use crate::core::{CheckError, SourceSpan};

use super::syntax::{EvalML1BinOp, EvalML1Derivation, EvalML1Expr, EvalML1Judgment, EvalML1Value};

pub(super) fn prove_judgment(judgment: EvalML1Judgment) -> Result<EvalML1Derivation, CheckError> {
    match &judgment {
        EvalML1Judgment::EvalTo {
            expr,
            value: expected_value,
        } => {
            let Some(actual) = prove_expr(expr) else {
                return Err(non_derivable_judgment_error(&judgment, None));
            };
            let Some((_, actual_value)) = as_eval_to(&actual.judgment) else {
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
        EvalML1Judgment::PlusIs {
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
            |left, right, result| EvalML1Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalML1Judgment::MinusIs {
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
            |left, right, result| EvalML1Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalML1Judgment::TimesIs {
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
            |left, right, result| EvalML1Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalML1Judgment::LessThanIs {
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
                    Some(EvalML1Judgment::LessThanIs {
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
    actual_judgment: EvalML1Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalML1Judgment,
) -> Result<EvalML1Derivation, CheckError> {
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

fn prove_expr(expr: &EvalML1Expr) -> Option<EvalML1Derivation> {
    match expr {
        EvalML1Expr::Int(value) => Some(derivation(
            EvalML1Judgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1Value::Int(*value),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalML1Expr::Bool(value) => Some(derivation(
            EvalML1Judgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1Value::Bool(*value),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalML1Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_derivation = prove_expr(condition)?;
            let condition_value = as_eval_to_bool(&condition_derivation.judgment)?;
            if condition_value {
                let branch_derivation = prove_expr(then_branch)?;
                let value = as_eval_to_value(&branch_derivation.judgment)?.clone();
                Some(derivation(
                    EvalML1Judgment::EvalTo {
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfT",
                    vec![condition_derivation, branch_derivation],
                ))
            } else {
                let branch_derivation = prove_expr(else_branch)?;
                let value = as_eval_to_value(&branch_derivation.judgment)?.clone();
                Some(derivation(
                    EvalML1Judgment::EvalTo {
                        expr: expr.clone(),
                        value,
                    },
                    "E-IfF",
                    vec![condition_derivation, branch_derivation],
                ))
            }
        }
        EvalML1Expr::BinOp { op, left, right } => prove_binop(expr, *op, left, right),
    }
}

fn prove_binop(
    expr: &EvalML1Expr,
    op: EvalML1BinOp,
    left: &EvalML1Expr,
    right: &EvalML1Expr,
) -> Option<EvalML1Derivation> {
    let left_derivation = prove_expr(left)?;
    let right_derivation = prove_expr(right)?;
    let left_int = as_eval_to_int(&left_derivation.judgment)?;
    let right_int = as_eval_to_int(&right_derivation.judgment)?;

    match op {
        EvalML1BinOp::Plus => {
            let result = left_int.checked_add(right_int)?;
            let third = derivation(
                EvalML1Judgment::PlusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalML1Judgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1Value::Int(result),
                },
                "E-Plus",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1BinOp::Minus => {
            let result = left_int.checked_sub(right_int)?;
            let third = derivation(
                EvalML1Judgment::MinusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalML1Judgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1Value::Int(result),
                },
                "E-Minus",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1BinOp::Times => {
            let result = left_int.checked_mul(right_int)?;
            let third = derivation(
                EvalML1Judgment::TimesIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalML1Judgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1Value::Int(result),
                },
                "E-Times",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1BinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalML1Judgment::LessThanIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalML1Judgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1Value::Bool(result),
                },
                "E-Lt",
                vec![left_derivation, right_derivation, third],
            ))
        }
    }
}

fn as_eval_to(judgment: &EvalML1Judgment) -> Option<(&EvalML1Expr, &EvalML1Value)> {
    let EvalML1Judgment::EvalTo { expr, value } = judgment else {
        return None;
    };
    Some((expr, value))
}

fn as_eval_to_value(judgment: &EvalML1Judgment) -> Option<&EvalML1Value> {
    let (_, value) = as_eval_to(judgment)?;
    Some(value)
}

fn as_eval_to_int(judgment: &EvalML1Judgment) -> Option<i64> {
    let (_, value) = as_eval_to(judgment)?;
    let EvalML1Value::Int(value_int) = value else {
        return None;
    };
    Some(*value_int)
}

fn as_eval_to_bool(judgment: &EvalML1Judgment) -> Option<bool> {
    let (_, value) = as_eval_to(judgment)?;
    let EvalML1Value::Bool(value_bool) = value else {
        return None;
    };
    Some(*value_bool)
}

fn non_derivable_judgment_error(
    actual: &EvalML1Judgment,
    expected: Option<EvalML1Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML1 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML1 (actual: {actual}; fix: check operand/value types and operator constraints)"
        )),
    }
}

fn fix_message(actual: &EvalML1Judgment, expected: &EvalML1Judgment) -> String {
    match (actual, expected) {
        (
            EvalML1Judgment::EvalTo {
                expr: _,
                value: actual_value,
            },
            EvalML1Judgment::EvalTo {
                expr: _,
                value: expected_value,
            },
        ) => {
            if actual_value == expected_value {
                "fix: check the expression and value forms".to_string()
            } else {
                format!("fix: replace value with {expected_value}")
            }
        }
        (
            EvalML1Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalML1Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML1Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalML1Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML1Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalML1Judgment::TimesIs {
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
            EvalML1Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalML1Judgment::LessThanIs {
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
    judgment: EvalML1Judgment,
    rule_name: &str,
    subderivations: Vec<EvalML1Derivation>,
) -> EvalML1Derivation {
    EvalML1Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, EvalML1Derivation};
    use crate::games::eval_ml1::syntax::{
        EvalML1BinOp, EvalML1Expr, EvalML1Judgment, EvalML1Value,
    };

    #[test]
    fn proves_eval_int_judgment_with_e_int() {
        let derivation = prove_judgment(EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::Int(3),
            value: EvalML1Value::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_if_judgment_with_e_if_t() {
        let derivation = prove_judgment(EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::If {
                condition: Box::new(EvalML1Expr::BinOp {
                    op: EvalML1BinOp::Lt,
                    left: Box::new(EvalML1Expr::Int(4)),
                    right: Box::new(EvalML1Expr::Int(5)),
                }),
                then_branch: Box::new(EvalML1Expr::BinOp {
                    op: EvalML1BinOp::Plus,
                    left: Box::new(EvalML1Expr::Int(2)),
                    right: Box::new(EvalML1Expr::Int(3)),
                }),
                else_branch: Box::new(EvalML1Expr::BinOp {
                    op: EvalML1BinOp::Times,
                    left: Box::new(EvalML1Expr::Int(8)),
                    right: Box::new(EvalML1Expr::Int(8)),
                }),
            },
            value: EvalML1Value::Int(5),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-IfT");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "E-Lt");
        assert_eq!(derivation.subderivations[1].rule_name, "E-Plus");
    }

    #[test]
    fn proves_builtin_plus_judgment_with_b_plus() {
        let derivation = prove_judgment(EvalML1Judgment::PlusIs {
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
        let err = prove_judgment(EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::BinOp {
                op: EvalML1BinOp::Plus,
                left: Box::new(EvalML1Expr::Int(3)),
                right: Box::new(EvalML1Expr::Int(5)),
            },
            value: EvalML1Value::Int(7),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1"));
        assert!(err
            .message()
            .contains("expected: 3 + 5 evalto 8, actual: 3 + 5 evalto 7"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn rejects_ill_typed_eval_judgment() {
        let err = prove_judgment(EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::BinOp {
                op: EvalML1BinOp::Plus,
                left: Box::new(EvalML1Expr::Bool(true)),
                right: Box::new(EvalML1Expr::Int(1)),
            },
            value: EvalML1Value::Int(2),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1"));
        assert!(err
            .message()
            .contains("fix: check operand/value types and operator constraints"));
    }

    #[test]
    fn rejects_non_derivable_builtin_judgment() {
        let err = prove_judgment(EvalML1Judgment::TimesIs {
            left: 2,
            right: 3,
            result: 5,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1"));
        assert!(err
            .message()
            .contains("expected: 2 times 3 is 6, actual: 2 times 3 is 5"));
        assert!(err.message().contains("fix: replace result term with 6"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_029() {
        let expected =
            parse_source(include_str!("../../../copl/029.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalML1Derivation, expected: &EvalML1Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
