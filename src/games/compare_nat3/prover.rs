use crate::core::CheckError;
use crate::core::SourceSpan;

use super::syntax::{CompareNat3Derivation, CompareNat3Judgment, CompareNat3Term};

pub(super) fn prove_judgment(
    judgment: CompareNat3Judgment,
) -> Result<CompareNat3Derivation, CheckError> {
    prove(&judgment).ok_or_else(|| non_derivable_judgment_error(&judgment))
}

fn prove(judgment: &CompareNat3Judgment) -> Option<CompareNat3Derivation> {
    match &judgment.right {
        CompareNat3Term::S(right_inner) if right_inner.as_ref() == &judgment.left => {
            Some(derivation(judgment.clone(), "L-Succ", Vec::new()))
        }
        CompareNat3Term::S(right_inner) => {
            let premise = CompareNat3Judgment {
                left: judgment.left.clone(),
                right: right_inner.as_ref().clone(),
            };
            Some(derivation(
                judgment.clone(),
                "L-SuccR",
                vec![prove(&premise)?],
            ))
        }
        _ => None,
    }
}

fn non_derivable_judgment_error(judgment: &CompareNat3Judgment) -> CheckError {
    let expected_right = succ_term(judgment.left.clone());
    let expected = CompareNat3Judgment {
        left: judgment.left.clone(),
        right: expected_right.clone(),
    };
    CheckError::rule_violation(format!(
        "judgment is not derivable in CompareNat3 (expected: {expected}, actual: {judgment}; fix: replace right term with {expected_right})"
    ))
}

fn succ_term(term: CompareNat3Term) -> CompareNat3Term {
    CompareNat3Term::S(Box::new(term))
}

fn derivation(
    judgment: CompareNat3Judgment,
    rule_name: &str,
    subderivations: Vec<CompareNat3Derivation>,
) -> CompareNat3Derivation {
    CompareNat3Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::CompareNat3Derivation;
    use super::prove_judgment;
    use crate::games::compare_nat3::syntax::{CompareNat3Judgment, CompareNat3Term};

    fn z() -> CompareNat3Term {
        CompareNat3Term::Z
    }

    fn s(inner: CompareNat3Term) -> CompareNat3Term {
        CompareNat3Term::S(Box::new(inner))
    }

    #[test]
    fn proves_judgment_with_l_succ() {
        let derivation = prove_judgment(CompareNat3Judgment {
            left: s(s(z())),
            right: s(s(s(z()))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-Succ");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_judgment_with_l_succ_r_chain() {
        let derivation = prove_judgment(CompareNat3Judgment {
            left: s(s(z())),
            right: s(s(s(s(s(z()))))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-SuccR");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "L-SuccR");
        assert_eq!(
            derivation.subderivations[0].subderivations[0].rule_name,
            "L-Succ"
        );
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(CompareNat3Judgment {
            left: s(z()),
            right: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in CompareNat3"));
        assert!(err
            .message()
            .contains("expected: S(Z) is less than S(S(Z)), actual: S(Z) is less than Z"));
        assert!(err
            .message()
            .contains("fix: replace right term with S(S(Z))"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_014() {
        let expected =
            parse_source(include_str!("../../../copl/014.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &CompareNat3Derivation, expected: &CompareNat3Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
