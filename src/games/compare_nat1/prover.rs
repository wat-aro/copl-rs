use crate::core::CheckError;
use crate::core::SourceSpan;

use super::syntax::{CompareNat1Derivation, CompareNat1Judgment, CompareNat1Term};

pub(super) fn prove_judgment(
    judgment: CompareNat1Judgment,
) -> Result<CompareNat1Derivation, CheckError> {
    prove(&judgment).ok_or_else(|| non_derivable_judgment_error(&judgment))
}

fn prove(judgment: &CompareNat1Judgment) -> Option<CompareNat1Derivation> {
    if nat_value(&judgment.left) >= nat_value(&judgment.right) {
        return None;
    }

    if matches!(&judgment.right, CompareNat1Term::S(inner) if inner.as_ref() == &judgment.left) {
        return Some(derivation(judgment.clone(), "L-Succ", Vec::new()));
    }

    let middle = succ_term(judgment.left.clone());
    let first = CompareNat1Judgment {
        left: judgment.left.clone(),
        right: middle.clone(),
    };
    let second = CompareNat1Judgment {
        left: middle,
        right: judgment.right.clone(),
    };

    Some(derivation(
        judgment.clone(),
        "L-Trans",
        vec![prove(&first)?, prove(&second)?],
    ))
}

fn nat_value(term: &CompareNat1Term) -> usize {
    match term {
        CompareNat1Term::Z => 0,
        CompareNat1Term::S(inner) => 1 + nat_value(inner),
    }
}

fn succ_term(term: CompareNat1Term) -> CompareNat1Term {
    CompareNat1Term::S(Box::new(term))
}

fn non_derivable_judgment_error(judgment: &CompareNat1Judgment) -> CheckError {
    let expected_right = succ_term(judgment.left.clone());
    let expected = CompareNat1Judgment {
        left: judgment.left.clone(),
        right: expected_right.clone(),
    };
    CheckError::rule_violation(format!(
        "judgment is not derivable in CompareNat1 (expected: {expected}, actual: {judgment}; fix: replace right term with {expected_right})"
    ))
}

fn derivation(
    judgment: CompareNat1Judgment,
    rule_name: &str,
    subderivations: Vec<CompareNat1Derivation>,
) -> CompareNat1Derivation {
    CompareNat1Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::CompareNat1Derivation;
    use super::prove_judgment;
    use crate::games::compare_nat1::syntax::{CompareNat1Judgment, CompareNat1Term};

    fn z() -> CompareNat1Term {
        CompareNat1Term::Z
    }

    fn s(inner: CompareNat1Term) -> CompareNat1Term {
        CompareNat1Term::S(Box::new(inner))
    }

    #[test]
    fn proves_judgment_with_l_succ() {
        let derivation = prove_judgment(CompareNat1Judgment {
            left: s(s(z())),
            right: s(s(s(z()))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-Succ");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_judgment_with_l_trans_chain() {
        let derivation = prove_judgment(CompareNat1Judgment {
            left: s(s(z())),
            right: s(s(s(s(s(z()))))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "L-Trans");
        assert_eq!(derivation.subderivations.len(), 2);
        assert_eq!(derivation.subderivations[0].rule_name, "L-Succ");
        assert_eq!(derivation.subderivations[1].rule_name, "L-Trans");
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(CompareNat1Judgment {
            left: s(z()),
            right: z(),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in CompareNat1"));
        assert!(err
            .message()
            .contains("expected: S(Z) is less than S(S(Z)), actual: S(Z) is less than Z"));
        assert!(err
            .message()
            .contains("fix: replace right term with S(S(Z))"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_012() {
        let expected =
            parse_source(include_str!("../../../copl/012.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &CompareNat1Derivation, expected: &CompareNat1Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
