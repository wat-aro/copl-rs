use crate::core::{CheckError, SourceSpan};

use super::syntax::{NatDerivation, NatJudgment, NatTerm};

pub(super) fn prove_judgment(judgment: NatJudgment) -> Result<NatDerivation, CheckError> {
    let derivation = match &judgment {
        NatJudgment::PlusIs {
            left,
            right,
            result,
        } => prove_plus(left, right, result),
        NatJudgment::TimesIs {
            left,
            right,
            result,
        } => prove_times(left, right, result),
    };

    derivation.ok_or_else(|| non_derivable_judgment_error(&judgment))
}

fn prove_plus(left: &NatTerm, right: &NatTerm, result: &NatTerm) -> Option<NatDerivation> {
    match left {
        NatTerm::Z if right == result => Some(derivation(
            NatJudgment::PlusIs {
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
                NatJudgment::PlusIs {
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

fn prove_times(left: &NatTerm, right: &NatTerm, result: &NatTerm) -> Option<NatDerivation> {
    match left {
        NatTerm::Z if result == &NatTerm::Z => Some(derivation(
            NatJudgment::TimesIs {
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
                NatJudgment::TimesIs {
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

fn non_derivable_judgment_error(judgment: &NatJudgment) -> CheckError {
    let (expected, fix) = match judgment {
        NatJudgment::PlusIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_plus(left, right);
            let expected = NatJudgment::PlusIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            (expected, fix)
        }
        NatJudgment::TimesIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_times(left, right);
            let expected = NatJudgment::TimesIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            (expected, fix)
        }
    };

    CheckError::rule_violation(format!(
        "judgment is not derivable in Nat (expected: {expected}, actual: {judgment}; {fix})"
    ))
}

fn result_fix_message(actual_result: &NatTerm, expected_result: &NatTerm) -> String {
    if actual_result == expected_result {
        "fix: check the judgment terms and operator".to_string()
    } else {
        format!("fix: replace result term with {expected_result}")
    }
}

fn derivation(
    judgment: NatJudgment,
    rule_name: &str,
    subderivations: Vec<NatDerivation>,
) -> NatDerivation {
    NatDerivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::NatDerivation;
    use super::prove_judgment;
    use crate::games::nat::syntax::{NatJudgment, NatTerm};

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    #[test]
    fn proves_plus_judgment_with_p_zero() {
        let derivation = prove_judgment(NatJudgment::PlusIs {
            left: z(),
            right: s(z()),
            result: s(z()),
        })
        .expect("plus judgment should be derivable");

        assert_eq!(derivation.rule_name, "P-Zero");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_times_judgment_with_t_zero() {
        let derivation = prove_judgment(NatJudgment::TimesIs {
            left: z(),
            right: s(s(z())),
            result: z(),
        })
        .expect("times judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Zero");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_plus_judgment_with_p_succ_chain() {
        let derivation = prove_judgment(NatJudgment::PlusIs {
            left: s(s(z())),
            right: s(z()),
            result: s(s(s(z()))),
        })
        .expect("plus judgment should be derivable");

        assert_eq!(derivation.rule_name, "P-Succ");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "P-Succ");
        assert_eq!(derivation.subderivations[0].subderivations.len(), 1);
        assert_eq!(
            derivation.subderivations[0].subderivations[0].rule_name,
            "P-Zero"
        );
    }

    #[test]
    fn proves_times_judgment_with_t_succ_chain() {
        let derivation = prove_judgment(NatJudgment::TimesIs {
            left: s(s(z())),
            right: s(z()),
            result: s(s(z())),
        })
        .expect("times judgment should be derivable");

        assert_eq!(derivation.rule_name, "T-Succ");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "T-Succ");
        assert_eq!(derivation.subderivations[1].rule_name, "P-Succ");
    }

    #[test]
    fn rejects_non_derivable_plus_judgment() {
        let err = prove_judgment(NatJudgment::PlusIs {
            left: z(),
            right: s(z()),
            result: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err.message().contains("judgment is not derivable in Nat"));
        assert!(err
            .message()
            .contains("expected: Z plus S(Z) is S(Z), actual: Z plus S(Z) is Z"));
        assert!(err.message().contains("fix: replace result term with S(Z)"));
    }

    #[test]
    fn rejects_non_derivable_times_judgment() {
        let err = prove_judgment(NatJudgment::TimesIs {
            left: s(z()),
            right: s(z()),
            result: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err.message().contains("judgment is not derivable in Nat"));
        assert!(err
            .message()
            .contains("expected: S(Z) times S(Z) is S(Z), actual: S(Z) times S(Z) is Z"));
        assert!(err.message().contains("fix: replace result term with S(Z)"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_007() {
        let expected =
            parse_source(include_str!("../../../copl/007.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &NatDerivation, expected: &NatDerivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
