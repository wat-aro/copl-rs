use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalML1ErrBinOp, EvalML1ErrDerivation, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
};

pub(super) fn prove_judgment(
    judgment: EvalML1ErrJudgment,
) -> Result<EvalML1ErrDerivation, CheckError> {
    match &judgment {
        EvalML1ErrJudgment::EvalTo {
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
        EvalML1ErrJudgment::PlusIs {
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
            |left, right, result| EvalML1ErrJudgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalML1ErrJudgment::MinusIs {
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
            |left, right, result| EvalML1ErrJudgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalML1ErrJudgment::TimesIs {
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
            |left, right, result| EvalML1ErrJudgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalML1ErrJudgment::LessThanIs {
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
                    Some(EvalML1ErrJudgment::LessThanIs {
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
    actual_judgment: EvalML1ErrJudgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalML1ErrJudgment,
) -> Result<EvalML1ErrDerivation, CheckError> {
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

fn prove_expr(expr: &EvalML1ErrExpr) -> Option<EvalML1ErrDerivation> {
    match expr {
        EvalML1ErrExpr::Int(value) => Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Int(*value),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalML1ErrExpr::Bool(value) => Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Bool(*value),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalML1ErrExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_derivation = prove_expr(condition)?;
            match as_eval_to_value(&condition_derivation.judgment)? {
                EvalML1ErrValue::Bool(true) => {
                    let branch_derivation = prove_expr(then_branch)?;
                    match as_eval_to_value(&branch_derivation.judgment)? {
                        EvalML1ErrValue::Error => Some(derivation(
                            EvalML1ErrJudgment::EvalTo {
                                expr: expr.clone(),
                                value: EvalML1ErrValue::Error,
                            },
                            "E-IfTError",
                            vec![condition_derivation, branch_derivation],
                        )),
                        value => Some(derivation(
                            EvalML1ErrJudgment::EvalTo {
                                expr: expr.clone(),
                                value: value.clone(),
                            },
                            "E-IfT",
                            vec![condition_derivation, branch_derivation],
                        )),
                    }
                }
                EvalML1ErrValue::Bool(false) => {
                    let branch_derivation = prove_expr(else_branch)?;
                    match as_eval_to_value(&branch_derivation.judgment)? {
                        EvalML1ErrValue::Error => Some(derivation(
                            EvalML1ErrJudgment::EvalTo {
                                expr: expr.clone(),
                                value: EvalML1ErrValue::Error,
                            },
                            "E-IfFError",
                            vec![condition_derivation, branch_derivation],
                        )),
                        value => Some(derivation(
                            EvalML1ErrJudgment::EvalTo {
                                expr: expr.clone(),
                                value: value.clone(),
                            },
                            "E-IfF",
                            vec![condition_derivation, branch_derivation],
                        )),
                    }
                }
                EvalML1ErrValue::Int(_) => Some(derivation(
                    EvalML1ErrJudgment::EvalTo {
                        expr: expr.clone(),
                        value: EvalML1ErrValue::Error,
                    },
                    "E-IfInt",
                    vec![condition_derivation],
                )),
                EvalML1ErrValue::Error => None,
            }
        }
        EvalML1ErrExpr::BinOp { op, left, right } => prove_binop(expr, *op, left, right),
    }
}

fn prove_binop(
    expr: &EvalML1ErrExpr,
    op: EvalML1ErrBinOp,
    left: &EvalML1ErrExpr,
    right: &EvalML1ErrExpr,
) -> Option<EvalML1ErrDerivation> {
    let left_derivation = prove_expr(left)?;
    if as_eval_to_error(&left_derivation.judgment).is_some() {
        return Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Error,
            },
            error_l_rule_name(op),
            vec![left_derivation],
        ));
    }
    if as_eval_to_bool(&left_derivation.judgment).is_some() {
        return Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Error,
            },
            bool_l_rule_name(op),
            vec![left_derivation],
        ));
    }
    let left_int = as_eval_to_int(&left_derivation.judgment)?;

    let right_derivation = prove_expr(right)?;
    if as_eval_to_error(&right_derivation.judgment).is_some() {
        return Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Error,
            },
            error_r_rule_name(op),
            vec![right_derivation],
        ));
    }
    if as_eval_to_bool(&right_derivation.judgment).is_some() {
        return Some(derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: expr.clone(),
                value: EvalML1ErrValue::Error,
            },
            bool_r_rule_name(op),
            vec![right_derivation],
        ));
    }
    let right_int = as_eval_to_int(&right_derivation.judgment)?;

    match op {
        EvalML1ErrBinOp::Plus => {
            let result = left_int.checked_add(right_int)?;
            let third = derivation(
                EvalML1ErrJudgment::PlusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalML1ErrJudgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1ErrValue::Int(result),
                },
                "E-Plus",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1ErrBinOp::Minus => {
            let result = left_int.checked_sub(right_int)?;
            let third = derivation(
                EvalML1ErrJudgment::MinusIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalML1ErrJudgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1ErrValue::Int(result),
                },
                "E-Minus",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1ErrBinOp::Times => {
            let result = left_int.checked_mul(right_int)?;
            let third = derivation(
                EvalML1ErrJudgment::TimesIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalML1ErrJudgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1ErrValue::Int(result),
                },
                "E-Times",
                vec![left_derivation, right_derivation, third],
            ))
        }
        EvalML1ErrBinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalML1ErrJudgment::LessThanIs {
                    left: left_int,
                    right: right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalML1ErrJudgment::EvalTo {
                    expr: expr.clone(),
                    value: EvalML1ErrValue::Bool(result),
                },
                "E-Lt",
                vec![left_derivation, right_derivation, third],
            ))
        }
    }
}

fn bool_l_rule_name(op: EvalML1ErrBinOp) -> &'static str {
    match op {
        EvalML1ErrBinOp::Plus => "E-PlusBoolL",
        EvalML1ErrBinOp::Minus => "E-MinusBoolL",
        EvalML1ErrBinOp::Times => "E-TimesBoolL",
        EvalML1ErrBinOp::Lt => "E-LtBoolL",
    }
}

fn bool_r_rule_name(op: EvalML1ErrBinOp) -> &'static str {
    match op {
        EvalML1ErrBinOp::Plus => "E-PlusBoolR",
        EvalML1ErrBinOp::Minus => "E-MinusBoolR",
        EvalML1ErrBinOp::Times => "E-TimesBoolR",
        EvalML1ErrBinOp::Lt => "E-LtBoolR",
    }
}

fn error_l_rule_name(op: EvalML1ErrBinOp) -> &'static str {
    match op {
        EvalML1ErrBinOp::Plus => "E-PlusErrorL",
        EvalML1ErrBinOp::Minus => "E-MinusErrorL",
        EvalML1ErrBinOp::Times => "E-TimesErrorL",
        EvalML1ErrBinOp::Lt => "E-LtErrorL",
    }
}

fn error_r_rule_name(op: EvalML1ErrBinOp) -> &'static str {
    match op {
        EvalML1ErrBinOp::Plus => "E-PlusErrorR",
        EvalML1ErrBinOp::Minus => "E-MinusErrorR",
        EvalML1ErrBinOp::Times => "E-TimesErrorR",
        EvalML1ErrBinOp::Lt => "E-LtErrorR",
    }
}

fn as_eval_to(judgment: &EvalML1ErrJudgment) -> Option<(&EvalML1ErrExpr, &EvalML1ErrValue)> {
    let EvalML1ErrJudgment::EvalTo { expr, value } = judgment else {
        return None;
    };
    Some((expr, value))
}

fn as_eval_to_value(judgment: &EvalML1ErrJudgment) -> Option<&EvalML1ErrValue> {
    let (_, value) = as_eval_to(judgment)?;
    Some(value)
}

fn as_eval_to_int(judgment: &EvalML1ErrJudgment) -> Option<i64> {
    let (_, value) = as_eval_to(judgment)?;
    let EvalML1ErrValue::Int(value_int) = value else {
        return None;
    };
    Some(*value_int)
}

fn as_eval_to_bool(judgment: &EvalML1ErrJudgment) -> Option<bool> {
    let (_, value) = as_eval_to(judgment)?;
    let EvalML1ErrValue::Bool(value_bool) = value else {
        return None;
    };
    Some(*value_bool)
}

fn as_eval_to_error(judgment: &EvalML1ErrJudgment) -> Option<()> {
    let (_, value) = as_eval_to(judgment)?;
    if matches!(value, EvalML1ErrValue::Error) {
        Some(())
    } else {
        None
    }
}

fn non_derivable_judgment_error(
    actual: &EvalML1ErrJudgment,
    expected: Option<EvalML1ErrJudgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML1Err (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalML1Err (actual: {actual}; fix: check operand/value types and error-propagation constraints)"
        )),
    }
}

fn fix_message(actual: &EvalML1ErrJudgment, expected: &EvalML1ErrJudgment) -> String {
    match (actual, expected) {
        (
            EvalML1ErrJudgment::EvalTo {
                expr: _,
                value: actual_value,
            },
            EvalML1ErrJudgment::EvalTo {
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
            EvalML1ErrJudgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalML1ErrJudgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML1ErrJudgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalML1ErrJudgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalML1ErrJudgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalML1ErrJudgment::TimesIs {
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
            EvalML1ErrJudgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalML1ErrJudgment::LessThanIs {
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
    judgment: EvalML1ErrJudgment,
    rule_name: &str,
    subderivations: Vec<EvalML1ErrDerivation>,
) -> EvalML1ErrDerivation {
    EvalML1ErrDerivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, EvalML1ErrDerivation};
    use crate::games::eval_ml1_err::syntax::{
        EvalML1ErrBinOp, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
    };

    #[test]
    fn proves_eval_int_judgment_with_e_int() {
        let derivation = prove_judgment(EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::Int(3),
            value: EvalML1ErrValue::Int(3),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Int");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_if_error_judgment_with_e_if_t_error() {
        let derivation = prove_judgment(EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::If {
                condition: Box::new(EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Lt,
                    left: Box::new(EvalML1ErrExpr::Int(3)),
                    right: Box::new(EvalML1ErrExpr::Int(4)),
                }),
                then_branch: Box::new(EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Lt,
                    left: Box::new(EvalML1ErrExpr::Int(1)),
                    right: Box::new(EvalML1ErrExpr::Bool(true)),
                }),
                else_branch: Box::new(EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Minus,
                    left: Box::new(EvalML1ErrExpr::Int(3)),
                    right: Box::new(EvalML1ErrExpr::Bool(false)),
                }),
            },
            value: EvalML1ErrValue::Error,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-IfTError");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "E-Lt");
        assert_eq!(derivation.subderivations[1].rule_name, "E-LtBoolR");
    }

    #[test]
    fn proves_builtin_plus_judgment_with_b_plus() {
        let derivation = prove_judgment(EvalML1ErrJudgment::PlusIs {
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
        let err = prove_judgment(EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::BinOp {
                op: EvalML1ErrBinOp::Plus,
                left: Box::new(EvalML1ErrExpr::Int(1)),
                right: Box::new(EvalML1ErrExpr::Bool(true)),
            },
            value: EvalML1ErrValue::Int(1),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1Err"));
        assert!(err
            .message()
            .contains("expected: 1 + true evalto error, actual: 1 + true evalto 1"));
        assert!(err.message().contains("fix: replace value with error"));
    }

    #[test]
    fn rejects_non_derivable_if_with_error_condition() {
        let err = prove_judgment(EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::If {
                condition: Box::new(EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Plus,
                    left: Box::new(EvalML1ErrExpr::Int(1)),
                    right: Box::new(EvalML1ErrExpr::Bool(true)),
                }),
                then_branch: Box::new(EvalML1ErrExpr::Int(1)),
                else_branch: Box::new(EvalML1ErrExpr::Int(2)),
            },
            value: EvalML1ErrValue::Error,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1Err"));
        assert!(err
            .message()
            .contains("fix: check operand/value types and error-propagation constraints"));
    }

    #[test]
    fn rejects_non_derivable_builtin_judgment() {
        let err = prove_judgment(EvalML1ErrJudgment::TimesIs {
            left: 2,
            right: 3,
            result: 5,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalML1Err"));
        assert!(err
            .message()
            .contains("expected: 2 times 3 is 6, actual: 2 times 3 is 5"));
        assert!(err.message().contains("fix: replace result term with 6"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_033() {
        let expected =
            parse_source(include_str!("../../../copl/033.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalML1ErrDerivation, expected: &EvalML1ErrDerivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
