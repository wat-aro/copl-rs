use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{CompareNat2Derivation, CompareNat2Judgment, CompareNat2Term};

#[derive(Debug, Clone, Copy)]
enum CompareNat2DerivationRule {
    LZero,
    LSuccSucc,
}

impl CompareNat2DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "L-Zero" => Some(Self::LZero),
            "L-SuccSucc" => Some(Self::LSuccSucc),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::LZero => "L-Zero",
            Self::LSuccSucc => "L-SuccSucc",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CompareNat2Game;

impl Game for CompareNat2Game {
    fn kind(&self) -> GameKind {
        GameKind::CompareNat2
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let parsed = parse_source(source)?;
        let inferred = infer_judgment(&parsed).map_err(|err| {
            annotate_rule_violation_with_premise_path(
                err,
                &parsed,
                |derivation| &derivation.span,
                |derivation| derivation.subderivations.as_slice(),
            )
        })?;
        Ok(CheckReport {
            game: self.kind(),
            summary: inferred.to_string(),
        })
    }
}

fn infer_judgment(derivation: &CompareNat2Derivation) -> Result<CompareNat2Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &CompareNat2Derivation,
) -> Result<CompareNat2Judgment, CheckError> {
    let Some(rule) = CompareNat2DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &CompareNat2Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &CompareNat2Derivation,
    rule: CompareNat2DerivationRule,
) -> Result<CompareNat2Judgment, CheckError> {
    match rule {
        CompareNat2DerivationRule::LZero => check_l_zero(derivation),
        CompareNat2DerivationRule::LSuccSucc => check_l_succ_succ(derivation),
    }
}

fn check_all_subderivations(subderivations: &[CompareNat2Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &CompareNat2Derivation,
    detail: String,
) -> Result<CompareNat2Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: L-Zero, L-SuccSucc; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: CompareNat2DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: CompareNat2DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_rule_application_message_l_succ_succ(
    expected_premise: &CompareNat2Judgment,
    actual_premise: &CompareNat2Judgment,
) -> String {
    format!(
        "Wrong rule application: L-SuccSucc (expected premise: {expected_premise}, actual premise: {actual_premise}; fix: remove one leading S from both sides of the conclusion when building the premise)"
    )
}

fn check_l_zero(derivation: &CompareNat2Derivation) -> Result<CompareNat2Judgment, CheckError> {
    let rule = CompareNat2DerivationRule::LZero;
    match &derivation.judgment {
        CompareNat2Judgment {
            left: CompareNat2Term::Z,
            right: CompareNat2Term::S(_),
        } => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Z is less than S(n)"),
        ),
    }
}

fn check_l_succ_succ(
    derivation: &CompareNat2Derivation,
) -> Result<CompareNat2Judgment, CheckError> {
    let rule = CompareNat2DerivationRule::LSuccSucc;
    match &derivation.judgment {
        CompareNat2Judgment {
            left: CompareNat2Term::S(left_inner),
            right: CompareNat2Term::S(right_inner),
        } => match derivation.subderivations.as_slice() {
            [premise_derivation] => {
                let premise = infer_judgment(premise_derivation)?;
                let expected_premise = CompareNat2Judgment {
                    left: left_inner.as_ref().clone(),
                    right: right_inner.as_ref().clone(),
                };
                if premise == expected_premise {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message_l_succ_succ(&expected_premise, &premise),
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
            wrong_conclusion_form_message(rule, "S(n1) is less than S(n2)"),
        ),
    }
}

fn rule_violation(derivation: &CompareNat2Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::CompareNat2Game;

    #[test]
    fn reports_root_judgment_text_for_all_compare_nat2_fixtures() {
        let game = CompareNat2Game;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/010.copl"),
                "S(S(Z)) is less than S(S(S(Z)))",
            ),
            (
                include_str!("../../../copl/013.copl"),
                "S(S(Z)) is less than S(S(S(S(S(Z)))))",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z is less than S(Z) by L-Zero { Z is less than S(Z) by L-Zero {} }";
        let err = CompareNat2Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: L-Zero"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        assert!(err.message().contains("premise path: root"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_l_zero_conclusion() {
        let source = "S(Z) is less than S(S(Z)) by L-Zero {}";
        let err = CompareNat2Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: L-Zero"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_l_succ_succ_premise() {
        let source = r#"
S(Z) is less than S(S(Z)) by L-SuccSucc {
  Z is less than S(S(Z)) by L-Zero {}
}
"#;
        let err = CompareNat2Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: L-SuccSucc"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z is less than S(Z) by L-Unknown {}";
        let err = CompareNat2Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: L-Zero, L-SuccSucc"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
S(Z) is less than S(S(Z)) by L-SuccSucc {
  Z is less than S(Z) by L-Unknown {}
}
"#;
        let err = CompareNat2Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
