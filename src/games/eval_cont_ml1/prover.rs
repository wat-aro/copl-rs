use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalContML1BinOp, EvalContML1ContFrame, EvalContML1Continuation, EvalContML1Derivation,
    EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
};

pub(super) fn prove_judgment(
    judgment: EvalContML1Judgment,
) -> Result<EvalContML1Derivation, CheckError> {
    match &judgment {
        EvalContML1Judgment::EvalTo {
            expr,
            continuation,
            value: expected_value,
            ..
        } => {
            let Some(actual) = prove_eval_to(expr, continuation.clone()) else {
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
        EvalContML1Judgment::ContEvalTo {
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
        EvalContML1Judgment::PlusIs {
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
            |left, right, result| EvalContML1Judgment::PlusIs {
                left,
                right,
                result,
            },
        ),
        EvalContML1Judgment::MinusIs {
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
            |left, right, result| EvalContML1Judgment::MinusIs {
                left,
                right,
                result,
            },
        ),
        EvalContML1Judgment::TimesIs {
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
            |left, right, result| EvalContML1Judgment::TimesIs {
                left,
                right,
                result,
            },
        ),
        EvalContML1Judgment::LessThanIs {
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
                    Some(EvalContML1Judgment::LessThanIs {
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
    actual_judgment: EvalContML1Judgment,
    rule_name: &str,
    left: &i64,
    right: &i64,
    actual_result: i64,
    compute: impl Fn(i64, i64) -> Option<i64>,
    build_judgment: impl Fn(i64, i64, i64) -> EvalContML1Judgment,
) -> Result<EvalContML1Derivation, CheckError> {
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
    expr: &EvalContML1Expr,
    continuation: EvalContML1Continuation,
) -> Option<EvalContML1Derivation> {
    match expr {
        EvalContML1Expr::Int(value) => {
            let premise = prove_cont_eval(EvalContML1Value::Int(*value), continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(expr.clone(), continuation, value),
                "E-Int",
                vec![premise],
            ))
        }
        EvalContML1Expr::Bool(value) => {
            let premise = prove_cont_eval(EvalContML1Value::Bool(*value), continuation.clone())?;
            let value = as_cont_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(expr.clone(), continuation, value),
                "E-Bool",
                vec![premise],
            ))
        }
        EvalContML1Expr::BinOp { op, left, right } => {
            let next_continuation = prepend_frame(
                EvalContML1ContFrame::EvalR {
                    op: *op,
                    right: right.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(left.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(expr.clone(), continuation, value),
                "E-BinOp",
                vec![premise],
            ))
        }
        EvalContML1Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let next_continuation = prepend_frame(
                EvalContML1ContFrame::If {
                    then_branch: then_branch.as_ref().clone(),
                    else_branch: else_branch.as_ref().clone(),
                },
                &continuation,
            );
            let premise = prove_eval_to(condition.as_ref(), next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                eval_to_judgment(expr.clone(), continuation, value),
                "E-If",
                vec![premise],
            ))
        }
    }
}

fn prove_cont_eval(
    input: EvalContML1Value,
    continuation: EvalContML1Continuation,
) -> Option<EvalContML1Derivation> {
    if continuation.frames.is_empty() {
        return Some(derivation(
            cont_eval_to_judgment(input.clone(), EvalContML1Continuation::hole(), input),
            "C-Ret",
            Vec::new(),
        ));
    }

    let (head, tail) = split_continuation(&continuation)?;
    match head {
        EvalContML1ContFrame::EvalR { op, right } => {
            let left = as_int(&input)?;
            let next_frame = match op {
                EvalContML1BinOp::Plus => EvalContML1ContFrame::Plus { left },
                EvalContML1BinOp::Minus => EvalContML1ContFrame::Minus { left },
                EvalContML1BinOp::Times => EvalContML1ContFrame::Times { left },
                EvalContML1BinOp::Lt => EvalContML1ContFrame::Lt { left },
            };
            let next_continuation = prepend_frame(next_frame, &tail);
            let premise = prove_eval_to(right, next_continuation)?;
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Int(left), continuation, value),
                "C-EvalR",
                vec![premise],
            ))
        }
        EvalContML1ContFrame::Plus { left } => {
            let right = as_int(&input)?;
            let result = left.checked_add(right)?;
            let first = derivation(
                EvalContML1Judgment::PlusIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML1Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Int(right), continuation, value),
                "C-Plus",
                vec![first, second],
            ))
        }
        EvalContML1ContFrame::Minus { left } => {
            let right = as_int(&input)?;
            let result = left.checked_sub(right)?;
            let first = derivation(
                EvalContML1Judgment::MinusIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML1Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Int(right), continuation, value),
                "C-Minus",
                vec![first, second],
            ))
        }
        EvalContML1ContFrame::Times { left } => {
            let right = as_int(&input)?;
            let result = left.checked_mul(right)?;
            let first = derivation(
                EvalContML1Judgment::TimesIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML1Value::Int(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Int(right), continuation, value),
                "C-Times",
                vec![first, second],
            ))
        }
        EvalContML1ContFrame::Lt { left } => {
            let right = as_int(&input)?;
            let result = *left < right;
            let first = derivation(
                EvalContML1Judgment::LessThanIs {
                    left: *left,
                    right,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            let second = prove_cont_eval(EvalContML1Value::Bool(result), tail)?;
            let value = as_cont_eval_to_value(&second.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Int(right), continuation, value),
                "C-Lt",
                vec![first, second],
            ))
        }
        EvalContML1ContFrame::If {
            then_branch,
            else_branch,
        } => {
            let condition = as_bool(&input)?;
            let (rule_name, premise) = if condition {
                ("C-IfT", prove_eval_to(then_branch, tail)?)
            } else {
                ("C-IfF", prove_eval_to(else_branch, tail)?)
            };
            let value = as_eval_to_value(&premise.judgment)?.clone();
            Some(derivation(
                cont_eval_to_judgment(EvalContML1Value::Bool(condition), continuation, value),
                rule_name,
                vec![premise],
            ))
        }
    }
}

fn split_continuation(
    continuation: &EvalContML1Continuation,
) -> Option<(&EvalContML1ContFrame, EvalContML1Continuation)> {
    let head = continuation.frames.first()?;
    let tail = EvalContML1Continuation {
        frames: continuation.frames.iter().skip(1).cloned().collect(),
        explicit_ret: continuation.explicit_ret,
    };
    Some((head, tail))
}

fn prepend_frame(
    frame: EvalContML1ContFrame,
    continuation: &EvalContML1Continuation,
) -> EvalContML1Continuation {
    let mut frames = vec![frame];
    frames.extend(continuation.frames.iter().cloned());
    EvalContML1Continuation {
        frames,
        explicit_ret: continuation.explicit_ret,
    }
}

fn eval_to_judgment(
    expr: EvalContML1Expr,
    continuation: EvalContML1Continuation,
    value: EvalContML1Value,
) -> EvalContML1Judgment {
    let has_continuation = !continuation.frames.is_empty() || continuation.explicit_ret;
    EvalContML1Judgment::EvalTo {
        expr,
        continuation,
        value,
        has_continuation,
    }
}

fn cont_eval_to_judgment(
    input: EvalContML1Value,
    continuation: EvalContML1Continuation,
    value: EvalContML1Value,
) -> EvalContML1Judgment {
    EvalContML1Judgment::ContEvalTo {
        input,
        continuation,
        value,
    }
}

fn as_eval_to_value(judgment: &EvalContML1Judgment) -> Option<&EvalContML1Value> {
    let EvalContML1Judgment::EvalTo { value, .. } = judgment else {
        return None;
    };
    Some(value)
}

fn as_cont_eval_to_value(judgment: &EvalContML1Judgment) -> Option<&EvalContML1Value> {
    let EvalContML1Judgment::ContEvalTo { value, .. } = judgment else {
        return None;
    };
    Some(value)
}

fn as_int(value: &EvalContML1Value) -> Option<i64> {
    let EvalContML1Value::Int(int) = value else {
        return None;
    };
    Some(*int)
}

fn as_bool(value: &EvalContML1Value) -> Option<bool> {
    let EvalContML1Value::Bool(boolean) = value else {
        return None;
    };
    Some(*boolean)
}

fn non_derivable_judgment_error(
    actual: &EvalContML1Judgment,
    expected: Option<EvalContML1Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalContML1 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in EvalContML1 (actual: {actual}; fix: check continuation shape, operand/value types, and operator constraints)"
        )),
    }
}

fn fix_message(actual: &EvalContML1Judgment, expected: &EvalContML1Judgment) -> String {
    match (actual, expected) {
        (
            EvalContML1Judgment::EvalTo {
                value: actual_value,
                ..
            },
            EvalContML1Judgment::EvalTo {
                value: expected_value,
                ..
            },
        )
        | (
            EvalContML1Judgment::ContEvalTo {
                value: actual_value,
                ..
            },
            EvalContML1Judgment::ContEvalTo {
                value: expected_value,
                ..
            },
        ) => {
            if actual_value == expected_value {
                "fix: check continuation structure and expression/value forms".to_string()
            } else {
                format!("fix: replace value with {expected_value}")
            }
        }
        (
            EvalContML1Judgment::PlusIs {
                result: actual_result,
                ..
            },
            EvalContML1Judgment::PlusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalContML1Judgment::MinusIs {
                result: actual_result,
                ..
            },
            EvalContML1Judgment::MinusIs {
                result: expected_result,
                ..
            },
        )
        | (
            EvalContML1Judgment::TimesIs {
                result: actual_result,
                ..
            },
            EvalContML1Judgment::TimesIs {
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
            EvalContML1Judgment::LessThanIs {
                result: actual_result,
                ..
            },
            EvalContML1Judgment::LessThanIs {
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
    judgment: EvalContML1Judgment,
    rule_name: &str,
    subderivations: Vec<EvalContML1Derivation>,
) -> EvalContML1Derivation {
    EvalContML1Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::prove_judgment;
    use crate::games::eval_cont_ml1::syntax::{
        EvalContML1BinOp, EvalContML1ContFrame, EvalContML1Continuation, EvalContML1Derivation,
        EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
    };

    #[test]
    fn proves_eval_judgment_with_e_binop() {
        let derivation = prove_judgment(EvalContML1Judgment::EvalTo {
            expr: EvalContML1Expr::BinOp {
                op: EvalContML1BinOp::Plus,
                left: Box::new(EvalContML1Expr::Int(3)),
                right: Box::new(EvalContML1Expr::Int(5)),
            },
            continuation: EvalContML1Continuation::implicit_hole(),
            value: EvalContML1Value::Int(8),
            has_continuation: false,
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-BinOp");
        assert_eq!(derivation.subderivations.len(), 1);
    }

    #[test]
    fn proves_continuation_judgment_with_c_plus() {
        let derivation = prove_judgment(EvalContML1Judgment::ContEvalTo {
            input: EvalContML1Value::Int(5),
            continuation: EvalContML1Continuation {
                frames: vec![EvalContML1ContFrame::Plus { left: 3 }],
                explicit_ret: false,
            },
            value: EvalContML1Value::Int(8),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "C-Plus");
        assert_eq!(derivation.subderivations.len(), 2);
    }

    #[test]
    fn proves_builtin_less_than_judgment_with_b_lt() {
        let derivation = prove_judgment(EvalContML1Judgment::LessThanIs {
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
        let err = prove_judgment(EvalContML1Judgment::EvalTo {
            expr: EvalContML1Expr::BinOp {
                op: EvalContML1BinOp::Plus,
                left: Box::new(EvalContML1Expr::Int(3)),
                right: Box::new(EvalContML1Expr::Int(5)),
            },
            continuation: EvalContML1Continuation::implicit_hole(),
            value: EvalContML1Value::Int(7),
            has_continuation: false,
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalContML1"));
        assert!(err
            .message()
            .contains("expected: 3 + 5 evalto 8, actual: 3 + 5 evalto 7"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn rejects_non_derivable_continuation_judgment() {
        let err = prove_judgment(EvalContML1Judgment::ContEvalTo {
            input: EvalContML1Value::Int(5),
            continuation: EvalContML1Continuation {
                frames: vec![EvalContML1ContFrame::Plus { left: 3 }],
                explicit_ret: false,
            },
            value: EvalContML1Value::Int(9),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalContML1"));
        assert!(err
            .message()
            .contains("expected: 5 => {3 + _} evalto 8, actual: 5 => {3 + _} evalto 9"));
        assert!(err.message().contains("fix: replace value with 8"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_126() {
        let expected =
            parse_source(include_str!("../../../copl/126.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalContML1Derivation, expected: &EvalContML1Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
