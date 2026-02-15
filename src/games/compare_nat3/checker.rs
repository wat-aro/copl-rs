use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::syntax::{CompareNat3Derivation, CompareNat3Judgment, CompareNat3Term};

#[derive(Debug, Clone, Copy)]
enum CompareNat3DerivationRule {
    LSucc,
    LSuccR,
}

impl CompareNat3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "L-Succ" => Some(Self::LSucc),
            "L-SuccR" => Some(Self::LSuccR),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::LSucc => "L-Succ",
            Self::LSuccR => "L-SuccR",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CompareNat3Game;

impl Game for CompareNat3Game {
    fn kind(&self) -> GameKind {
        GameKind::CompareNat3
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

fn infer_judgment(derivation: &CompareNat3Derivation) -> Result<CompareNat3Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &CompareNat3Derivation,
) -> Result<CompareNat3Judgment, CheckError> {
    let Some(rule) = CompareNat3DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &CompareNat3Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &CompareNat3Derivation,
    rule: CompareNat3DerivationRule,
) -> Result<CompareNat3Judgment, CheckError> {
    match rule {
        CompareNat3DerivationRule::LSucc => check_l_succ(derivation),
        CompareNat3DerivationRule::LSuccR => check_l_succ_r(derivation),
    }
}

fn check_all_subderivations(subderivations: &[CompareNat3Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &CompareNat3Derivation,
    detail: String,
) -> Result<CompareNat3Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: L-Succ, L-SuccR; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: CompareNat3DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: CompareNat3DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_rule_application_message_l_succ_r(
    expected_premise: &CompareNat3Judgment,
    actual_premise: &CompareNat3Judgment,
) -> String {
    format!(
        "Wrong rule application: L-SuccR (expected premise: {expected_premise}, actual premise: {actual_premise}; fix: keep the left term unchanged and remove one trailing S from the right term when building the premise)"
    )
}

fn check_l_succ(derivation: &CompareNat3Derivation) -> Result<CompareNat3Judgment, CheckError> {
    let rule = CompareNat3DerivationRule::LSucc;
    match &derivation.judgment {
        CompareNat3Judgment {
            left,
            right: CompareNat3Term::S(inner),
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

fn check_l_succ_r(derivation: &CompareNat3Derivation) -> Result<CompareNat3Judgment, CheckError> {
    let rule = CompareNat3DerivationRule::LSuccR;
    match &derivation.judgment {
        CompareNat3Judgment {
            left,
            right: CompareNat3Term::S(right_inner),
        } => match derivation.subderivations.as_slice() {
            [premise_derivation] => {
                let premise = infer_judgment(premise_derivation)?;
                let expected_premise = CompareNat3Judgment {
                    left: left.clone(),
                    right: right_inner.as_ref().clone(),
                };
                if premise == expected_premise {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message_l_succ_r(&expected_premise, &premise),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 is less than S(n2)"),
        ),
    }
}

fn rule_violation(derivation: &CompareNat3Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::CompareNat3Game;

    #[test]
    fn reports_root_judgment_text_for_all_compare_nat3_fixtures() {
        let game = CompareNat3Game;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/011.copl"),
                "S(S(Z)) is less than S(S(S(Z)))",
            ),
            (
                include_str!("../../../copl/014.copl"),
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
        let err = CompareNat3Game
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
        let err = CompareNat3Game
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
    fn reports_rule_violation_for_inconsistent_l_succ_r_premise() {
        let source = r#"
Z is less than S(S(S(Z))) by L-SuccR {
  Z is less than S(Z) by L-Succ {}
}
"#;
        let err = CompareNat3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: L-SuccR"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z is less than S(Z) by L-Unknown {}";
        let err = CompareNat3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: L-Succ, L-SuccR"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
Z is less than S(S(Z)) by L-SuccR {
  Z is less than S(Z) by L-Unknown {}
}
"#;
        let err = CompareNat3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
