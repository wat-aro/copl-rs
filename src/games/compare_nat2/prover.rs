use crate::core::CheckError;
use crate::core::SourceSpan;

use super::syntax::{CompareNat2Derivation, CompareNat2Judgment, CompareNat2Term};

pub(super) fn prove_judgment(
    judgment: CompareNat2Judgment,
) -> Result<CompareNat2Derivation, CheckError> {
    prove(&judgment).ok_or_else(|| non_derivable_judgment_error(&judgment))
}

fn prove(judgment: &CompareNat2Judgment) -> Option<CompareNat2Derivation> {
    match (&judgment.left, &judgment.right) {
        (CompareNat2Term::Z, CompareNat2Term::S(_)) => {
            Some(derivation(judgment.clone(), "L-Zero", Vec::new()))
        }
        (CompareNat2Term::S(left_inner), CompareNat2Term::S(right_inner)) => {
            let premise = CompareNat2Judgment {
                left: left_inner.as_ref().clone(),
                right: right_inner.as_ref().clone(),
            };
            Some(derivation(
                judgment.clone(),
                "L-SuccSucc",
                vec![prove(&premise)?],
            ))
        }
        _ => None,
    }
}

fn non_derivable_judgment_error(judgment: &CompareNat2Judgment) -> CheckError {
    let expected_right = succ_term(judgment.left.clone());
    let expected = CompareNat2Judgment {
        left: judgment.left.clone(),
        right: expected_right.clone(),
    };
    CheckError::rule_violation(format!(
        "judgment is not derivable in CompareNat2 (expected: {expected}, actual: {judgment}; fix: replace right term with {expected_right})"
    ))
}

fn succ_term(term: CompareNat2Term) -> CompareNat2Term {
    CompareNat2Term::S(Box::new(term))
}

fn derivation(
    judgment: CompareNat2Judgment,
    rule_name: &str,
    subderivations: Vec<CompareNat2Derivation>,
) -> CompareNat2Derivation {
    CompareNat2Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::CompareNat2Derivation;
    use super::prove_judgment;
    use crate::games::compare_nat2::syntax::{CompareNat2Judgment, CompareNat2Term};

    fn z() -> CompareNat2Term {
        CompareNat2Term::Z
    }

    fn s(inner: CompareNat2Term) -> CompareNat2Term {
        CompareNat2Term::S(Box::new(inner))
    }

    #[test]
    fn proves_judgment_with_l_zero() {
        let derivation = prove_judgment(CompareNat2Judgment {
            left: z(),
            right: s(s(z())),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-Zero");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_judgment_with_l_succ_succ_chain() {
        let derivation = prove_judgment(CompareNat2Judgment {
            left: s(s(z())),
            right: s(s(s(s(s(z()))))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-SuccSucc");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "L-SuccSucc");
        assert_eq!(
            derivation.subderivations[0].subderivations[0].rule_name,
            "L-Zero"
        );
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(CompareNat2Judgment {
            left: s(z()),
            right: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in CompareNat2"));
        assert!(err
            .message()
            .contains("expected: S(Z) is less than S(S(Z)), actual: S(Z) is less than Z"));
        assert!(err
            .message()
            .contains("fix: replace right term with S(S(Z))"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_013() {
        let expected =
            parse_source(include_str!("../../../copl/013.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &CompareNat2Derivation, expected: &CompareNat2Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
