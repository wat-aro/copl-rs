use crate::core::{CheckError, SourceSpan};

use super::syntax::{EvalNatExpDerivation, EvalNatExpExpr, EvalNatExpJudgment, NatTerm};

pub(super) fn prove_judgment(
    judgment: EvalNatExpJudgment,
) -> Result<EvalNatExpDerivation, CheckError> {
    let derivation = match &judgment {
        EvalNatExpJudgment::EvalTo {
            expr,
            value: expected_value,
        } => {
            let actual = prove_expr(expr);
            let Some(actual_value) = as_eval_to_nat(&actual.judgment) else {
                return Err(non_derivable_judgment_error(&judgment));
            };
            if actual_value == expected_value {
                actual
            } else {
                return Err(non_derivable_judgment_error(&judgment));
            }
        }
        EvalNatExpJudgment::PlusIs {
            left,
            right,
            result,
        } => {
            let Some(derivation) = prove_plus(left, right, result) else {
                return Err(non_derivable_judgment_error(&judgment));
            };
            derivation
        }
        EvalNatExpJudgment::TimesIs {
            left,
            right,
            result,
        } => {
            let Some(derivation) = prove_times(left, right, result) else {
                return Err(non_derivable_judgment_error(&judgment));
            };
            derivation
        }
    };

    Ok(derivation)
}

fn prove_expr(expr: &EvalNatExpExpr) -> EvalNatExpDerivation {
    match expr {
        EvalNatExpExpr::Nat(term) => derivation(
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Nat(term.clone()),
                value: term.clone(),
            },
            "E-Const",
            Vec::new(),
        ),
        EvalNatExpExpr::Plus(left, right) => {
            let first = prove_expr(left);
            let second = prove_expr(right);
            let first_value = as_eval_to_nat(&first.judgment)
                .expect("prove_expr(EvalTo) should always yield a Nat value")
                .clone();
            let second_value = as_eval_to_nat(&second.judgment)
                .expect("prove_expr(EvalTo) should always yield a Nat value")
                .clone();
            let result = eval_plus(&first_value, &second_value);
            let third = prove_plus(&first_value, &second_value, &result)
                .expect("eval_plus result must be derivable");
            derivation(
                EvalNatExpJudgment::EvalTo {
                    expr: expr.clone(),
                    value: result,
                },
                "E-Plus",
                vec![first, second, third],
            )
        }
        EvalNatExpExpr::Times(left, right) => {
            let first = prove_expr(left);
            let second = prove_expr(right);
            let first_value = as_eval_to_nat(&first.judgment)
                .expect("prove_expr(EvalTo) should always yield a Nat value")
                .clone();
            let second_value = as_eval_to_nat(&second.judgment)
                .expect("prove_expr(EvalTo) should always yield a Nat value")
                .clone();
            let result = eval_times(&first_value, &second_value);
            let third = prove_times(&first_value, &second_value, &result)
                .expect("eval_times result must be derivable");
            derivation(
                EvalNatExpJudgment::EvalTo {
                    expr: expr.clone(),
                    value: result,
                },
                "E-Times",
                vec![first, second, third],
            )
        }
    }
}

fn prove_plus(left: &NatTerm, right: &NatTerm, result: &NatTerm) -> Option<EvalNatExpDerivation> {
    match left {
        NatTerm::Z if right == result => Some(derivation(
            EvalNatExpJudgment::PlusIs {
                left: left.clone(),
                right: right.clone(),
                result: result.clone(),
            },
            "P-Zero",
            Vec::new(),
        )),
        NatTerm::S(left_inner) => {
            let NatTerm::S(result_inner) = result else {
                return None;
            };
            let premise = prove_plus(left_inner, right, result_inner)?;
            Some(derivation(
                EvalNatExpJudgment::PlusIs {
                    left: left.clone(),
                    right: right.clone(),
                    result: result.clone(),
                },
                "P-Succ",
                vec![premise],
            ))
        }
        _ => None,
    }
}

fn prove_times(left: &NatTerm, right: &NatTerm, result: &NatTerm) -> Option<EvalNatExpDerivation> {
    match left {
        NatTerm::Z if result == &NatTerm::Z => Some(derivation(
            EvalNatExpJudgment::TimesIs {
                left: left.clone(),
                right: right.clone(),
                result: result.clone(),
            },
            "T-Zero",
            Vec::new(),
        )),
        NatTerm::S(left_inner) => {
            let first_result = eval_times(left_inner, right);
            let first = prove_times(left_inner, right, &first_result)?;
            let second = prove_plus(right, &first_result, result)?;
            Some(derivation(
                EvalNatExpJudgment::TimesIs {
                    left: left.clone(),
                    right: right.clone(),
                    result: result.clone(),
                },
                "T-Succ",
                vec![first, second],
            ))
        }
        _ => None,
    }
}

fn eval_expr(expr: &EvalNatExpExpr) -> NatTerm {
    match expr {
        EvalNatExpExpr::Nat(term) => term.clone(),
        EvalNatExpExpr::Plus(left, right) => eval_plus(&eval_expr(left), &eval_expr(right)),
        EvalNatExpExpr::Times(left, right) => eval_times(&eval_expr(left), &eval_expr(right)),
    }
}

fn eval_plus(left: &NatTerm, right: &NatTerm) -> NatTerm {
    match left {
        NatTerm::Z => right.clone(),
        NatTerm::S(inner) => NatTerm::S(Box::new(eval_plus(inner, right))),
    }
}

fn eval_times(left: &NatTerm, right: &NatTerm) -> NatTerm {
    match left {
        NatTerm::Z => NatTerm::Z,
        NatTerm::S(inner) => {
            let partial = eval_times(inner, right);
            eval_plus(right, &partial)
        }
    }
}

fn as_eval_to_nat(judgment: &EvalNatExpJudgment) -> Option<&NatTerm> {
    let EvalNatExpJudgment::EvalTo { value, .. } = judgment else {
        return None;
    };
    Some(value)
}

fn non_derivable_judgment_error(judgment: &EvalNatExpJudgment) -> CheckError {
    let (expected, fix) = match judgment {
        EvalNatExpJudgment::EvalTo { expr, value } => {
            let expected_value = eval_expr(expr);
            let expected = EvalNatExpJudgment::EvalTo {
                expr: expr.clone(),
                value: expected_value.clone(),
            };
            let fix = value_fix_message(value, &expected_value);
            (expected, fix)
        }
        EvalNatExpJudgment::PlusIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_plus(left, right);
            let expected = EvalNatExpJudgment::PlusIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            (expected, fix)
        }
        EvalNatExpJudgment::TimesIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_times(left, right);
            let expected = EvalNatExpJudgment::TimesIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            (expected, fix)
        }
    };

    CheckError::rule_violation(format!(
        "judgment is not derivable in EvalNatExp (expected: {expected}, actual: {judgment}; {fix})"
    ))
}

fn value_fix_message(actual: &NatTerm, expected: &NatTerm) -> String {
    if actual == expected {
        "fix: check the expression and value forms".to_string()
    } else {
        format!("fix: replace value with {expected}")
    }
}

fn result_fix_message(actual: &NatTerm, expected: &NatTerm) -> String {
    if actual == expected {
        "fix: check the judgment terms and operator".to_string()
    } else {
        format!("fix: replace result term with {expected}")
    }
}

fn derivation(
    judgment: EvalNatExpJudgment,
    rule_name: &str,
    subderivations: Vec<EvalNatExpDerivation>,
) -> EvalNatExpDerivation {
    EvalNatExpDerivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::EvalNatExpDerivation;
    use super::prove_judgment;
    use crate::games::eval_nat_exp::syntax::{EvalNatExpExpr, EvalNatExpJudgment, NatTerm};

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    #[test]
    fn proves_eval_const_judgment() {
        let derivation = prove_judgment(EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Nat(s(z())),
            value: s(z()),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Const");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_eval_plus_judgment() {
        let derivation = prove_judgment(EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Plus(
                Box::new(EvalNatExpExpr::Nat(s(z()))),
                Box::new(EvalNatExpExpr::Nat(s(z()))),
            ),
            value: s(s(z())),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Plus");
        assert_eq!(derivation.subderivations.len(), 3);
        assert_eq!(derivation.subderivations[0].rule_name, "E-Const");
        assert_eq!(derivation.subderivations[1].rule_name, "E-Const");
        assert_eq!(derivation.subderivations[2].rule_name, "P-Succ");
    }

    #[test]
    fn proves_eval_times_judgment() {
        let derivation = prove_judgment(EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Times(
                Box::new(EvalNatExpExpr::Nat(s(s(z())))),
                Box::new(EvalNatExpExpr::Nat(s(z()))),
            ),
            value: s(s(z())),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "E-Times");
        assert_eq!(derivation.subderivations.len(), 3);
        assert_eq!(derivation.subderivations[2].rule_name, "T-Succ");
    }

    #[test]
    fn rejects_non_derivable_eval_judgment() {
        let err = prove_judgment(EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Plus(
                Box::new(EvalNatExpExpr::Nat(z())),
                Box::new(EvalNatExpExpr::Nat(s(z()))),
            ),
            value: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalNatExp"));
        assert!(err
            .message()
            .contains("expected: Z + S(Z) evalto S(Z), actual: Z + S(Z) evalto Z"));
        assert!(err.message().contains("fix: replace value with S(Z)"));
    }

    #[test]
    fn rejects_non_derivable_times_judgment() {
        let err = prove_judgment(EvalNatExpJudgment::TimesIs {
            left: s(z()),
            right: s(z()),
            result: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in EvalNatExp"));
        assert!(err
            .message()
            .contains("expected: S(Z) times S(Z) is S(Z), actual: S(Z) times S(Z) is Z"));
        assert!(err.message().contains("fix: replace result term with S(Z)"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_018() {
        let expected =
            parse_source(include_str!("../../../copl/018.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &EvalNatExpDerivation, expected: &EvalNatExpDerivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
