use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::syntax::{CompareNat1Derivation, CompareNat1Judgment, CompareNat1Term};

#[derive(Debug, Clone, Copy)]
enum CompareNat1DerivationRule {
    LSucc,
    LTrans,
}

impl CompareNat1DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "L-Succ" => Some(Self::LSucc),
            "L-Trans" => Some(Self::LTrans),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::LSucc => "L-Succ",
            Self::LTrans => "L-Trans",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CompareNat1Game;

impl Game for CompareNat1Game {
    fn kind(&self) -> GameKind {
        GameKind::CompareNat1
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let parsed = parse_source(source)?;
        let inferred = infer_judgment(&parsed)?;
        Ok(CheckReport {
            game: self.kind(),
            summary: inferred.to_string(),
        })
    }
}

fn infer_judgment(derivation: &CompareNat1Derivation) -> Result<CompareNat1Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &CompareNat1Derivation,
) -> Result<CompareNat1Judgment, CheckError> {
    let Some(rule) = CompareNat1DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &CompareNat1Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &CompareNat1Derivation,
    rule: CompareNat1DerivationRule,
) -> Result<CompareNat1Judgment, CheckError> {
    match rule {
        CompareNat1DerivationRule::LSucc => check_l_succ(derivation),
        CompareNat1DerivationRule::LTrans => check_l_trans(derivation),
    }
}

fn check_all_subderivations(subderivations: &[CompareNat1Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &CompareNat1Derivation,
    detail: String,
) -> Result<CompareNat1Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: L-Succ, L-Trans; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: CompareNat1DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: CompareNat1DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_rule_application_message_l_trans(
    expected_first: &CompareNat1Judgment,
    expected_second: &CompareNat1Judgment,
    actual_first: &CompareNat1Judgment,
    actual_second: &CompareNat1Judgment,
) -> String {
    format!(
        "Wrong rule application: L-Trans (expected premises: [{expected_first}] and [{expected_second}], actual premises: [{actual_first}] and [{actual_second}]; fix: connect the middle term consistently across both premises and the conclusion)"
    )
}

fn check_l_succ(derivation: &CompareNat1Derivation) -> Result<CompareNat1Judgment, CheckError> {
    let rule = CompareNat1DerivationRule::LSucc;
    match &derivation.judgment {
        CompareNat1Judgment {
            left,
            right: CompareNat1Term::S(inner),
        } if inner.as_ref() == left => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n is less than S(n)"),
        ),
    }
}

fn check_l_trans(derivation: &CompareNat1Derivation) -> Result<CompareNat1Judgment, CheckError> {
    let rule = CompareNat1DerivationRule::LTrans;
    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first_premise = infer_judgment(d1)?;
            let second_premise = infer_judgment(d2)?;

            if first_premise.left == derivation.judgment.left
                && first_premise.right == second_premise.left
                && second_premise.right == derivation.judgment.right
            {
                Ok(derivation.judgment.clone())
            } else {
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message_l_trans(
                        &CompareNat1Judgment {
                            left: derivation.judgment.left.clone(),
                            right: first_premise.right.clone(),
                        },
                        &CompareNat1Judgment {
                            left: first_premise.right.clone(),
                            right: derivation.judgment.right.clone(),
                        },
                        &first_premise,
                        &second_premise,
                    ),
                ))
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        ),
    }
}

fn rule_violation(derivation: &CompareNat1Derivation, detail: impl Into<String>) -> CheckError {
    let detail = detail.into();
    CheckError::rule_violation(format!(
        "{detail}: {} by {}",
        derivation.judgment, derivation.rule_name
    ))
    .with_span(derivation.span.clone())
}

#[cfg(test)]
mod tests {
    use crate::core::{CheckErrorKind, Game};

    use super::CompareNat1Game;

    #[test]
    fn reports_root_judgment_text_for_all_compare_nat1_fixtures() {
        let game = CompareNat1Game;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/009.copl"),
                "S(S(Z)) is less than S(S(S(Z)))",
            ),
            (
                include_str!("../../../copl/012.copl"),
                "S(S(Z)) is less than S(S(S(S(S(Z)))))",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z is less than S(Z) by L-Succ { Z is less than S(Z) by L-Succ {} }";
        let err = CompareNat1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: L-Succ"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_l_succ_conclusion() {
        let source = "Z is less than Z by L-Succ {}";
        let err = CompareNat1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: L-Succ"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_l_trans_link() {
        let source = r#"
Z is less than S(S(S(Z))) by L-Trans {
  Z is less than S(Z) by L-Succ {};
  S(S(Z)) is less than S(S(S(Z))) by L-Succ {}
}
"#;
        let err = CompareNat1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: L-Trans"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z is less than S(Z) by L-Unknown {}";
        let err = CompareNat1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: L-Succ, L-Trans"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
Z is less than S(S(Z)) by L-Trans {
  Z is less than S(Z) by L-Unknown {};
  S(Z) is less than S(S(Z)) by L-Succ {}
}
"#;
        let err = CompareNat1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
