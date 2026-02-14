use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::syntax::{NatDerivation, NatJudgment, NatTerm};

#[derive(Debug, Clone, Copy)]
enum NatDerivationRule {
    PZero,
    PSucc,
    TZero,
    TSucc,
}

impl NatDerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "P-Zero" => Some(Self::PZero),
            "P-Succ" => Some(Self::PSucc),
            "T-Zero" => Some(Self::TZero),
            "T-Succ" => Some(Self::TSucc),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::PZero => "P-Zero",
            Self::PSucc => "P-Succ",
            Self::TZero => "T-Zero",
            Self::TSucc => "T-Succ",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NatGame;

impl Game for NatGame {
    fn kind(&self) -> GameKind {
        GameKind::Nat
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

fn infer_judgment(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let Some(rule) = NatDerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &NatDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &NatDerivation,
    rule: NatDerivationRule,
) -> Result<NatJudgment, CheckError> {
    match rule {
        NatDerivationRule::PZero => check_p_zero(derivation),
        NatDerivationRule::PSucc => check_p_succ(derivation),
        NatDerivationRule::TZero => check_t_zero(derivation),
        NatDerivationRule::TSucc => check_t_succ(derivation),
    }
}

fn check_all_subderivations(subderivations: &[NatDerivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &NatDerivation,
    detail: String,
) -> Result<NatJudgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: P-Zero, P-Succ, T-Zero, T-Succ; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(rule: NatDerivationRule, expected: usize, actual: usize) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: NatDerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_premise_form_message(
    rule: NatDerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &NatJudgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message_p_succ(
    n1: &NatTerm,
    n2: &NatTerm,
    n: &NatTerm,
    actual: &NatJudgment,
) -> String {
    format!(
        "Wrong rule application: P-Succ (expected premise: {n1} plus {n2} is {n}, actual premise: {actual}; fix: make premise terms consistent with conclusion S(n1) plus n2 is S(n))"
    )
}

fn wrong_rule_application_message_t_succ(
    first_expected_left: &NatTerm,
    expected_right: &NatTerm,
    expected_result: &NatTerm,
    first_actual: &NatJudgment,
    second_actual: &NatJudgment,
) -> String {
    format!(
        "Wrong rule application: T-Succ (expected links: first premise {first_expected_left} times {expected_right} is n3, second premise {expected_right} plus n3 is {expected_result}; actual premises: [{first_actual}] and [{second_actual}]; fix: make shared terms n2/n3/n4 consistent across conclusion and both premises)"
    )
}

fn check_p_zero(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let rule = NatDerivationRule::PZero;
    match &derivation.judgment {
        NatJudgment::PlusIs {
            left: NatTerm::Z,
            right: n,
            result: inferred_n,
        } if n == inferred_n => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Z plus n is n"),
        ),
    }
}

fn check_p_succ(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let rule = NatDerivationRule::PSucc;
    match &derivation.judgment {
        NatJudgment::PlusIs {
            left: NatTerm::S(n1),
            right: n2,
            result: NatTerm::S(result_inner),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let premise = infer_judgment(d1)?;
                match premise {
                    NatJudgment::PlusIs {
                        left: premise_n1,
                        right: premise_n2,
                        result: premise_n,
                    } => {
                        if premise_n1 == *n1.as_ref()
                            && premise_n2 == *n2
                            && premise_n == *result_inner.as_ref()
                        {
                            Ok(derivation.judgment.clone())
                        } else {
                            Err(rule_violation(
                                derivation,
                                wrong_rule_application_message_p_succ(
                                    n1.as_ref(),
                                    n2,
                                    result_inner.as_ref(),
                                    &NatJudgment::PlusIs {
                                        left: premise_n1,
                                        right: premise_n2,
                                        result: premise_n,
                                    },
                                ),
                            ))
                        }
                    }
                    actual => Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "n1 plus n2 is n", &actual),
                    )),
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "S(n1) plus n2 is S(n)"),
        ),
    }
}

fn check_t_zero(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let rule = NatDerivationRule::TZero;
    match &derivation.judgment {
        NatJudgment::TimesIs {
            left: NatTerm::Z,
            right: _n,
            result: NatTerm::Z,
        } => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Z times n is Z"),
        ),
    }
}

fn check_t_succ(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let rule = NatDerivationRule::TSucc;
    match &derivation.judgment {
        NatJudgment::TimesIs {
            left: NatTerm::S(n1),
            right: n2,
            result: n4,
        } => match derivation.subderivations.as_slice() {
            [d1, d2] => {
                let first_premise = infer_judgment(d1)?;
                let second_premise = infer_judgment(d2)?;
                match first_premise {
                    NatJudgment::TimesIs {
                        left: first_n1,
                        right: first_n2,
                        result: first_n3,
                    } => match second_premise {
                        NatJudgment::PlusIs {
                            left: second_n2,
                            right: second_n3,
                            result: second_n4,
                        } => {
                            if first_n1 == *n1.as_ref()
                                && second_n2 == first_n2
                                && second_n2 == *n2
                                && second_n3 == first_n3
                                && second_n4 == *n4
                            {
                                Ok(derivation.judgment.clone())
                            } else {
                                Err(rule_violation(
                                    derivation,
                                    wrong_rule_application_message_t_succ(
                                        n1.as_ref(),
                                        n2,
                                        n4,
                                        &NatJudgment::TimesIs {
                                            left: first_n1,
                                            right: first_n2,
                                            result: first_n3,
                                        },
                                        &NatJudgment::PlusIs {
                                            left: second_n2,
                                            right: second_n3,
                                            result: second_n4,
                                        },
                                    ),
                                ))
                            }
                        }
                        actual => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second", "n2 plus n3 is n4", &actual),
                        )),
                    },
                    actual => Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "n1 times n2 is n3", &actual),
                    )),
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "S(n1) times n2 is n4"),
        ),
    }
}

fn rule_violation(derivation: &NatDerivation, detail: impl Into<String>) -> CheckError {
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

    use super::NatGame;

    #[test]
    fn accepts_all_ascii_fixtures_001_to_008() {
        let game = NatGame;
        for source in [
            include_str!("../../../copl/001.copl"),
            include_str!("../../../copl/002.copl"),
            include_str!("../../../copl/003.copl"),
            include_str!("../../../copl/004.copl"),
            include_str!("../../../copl/005.copl"),
            include_str!("../../../copl/006.copl"),
            include_str!("../../../copl/007.copl"),
            include_str!("../../../copl/008.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains(" is "));
        }
    }

    #[test]
    fn reports_root_judgment_text_for_fixture_007() {
        let report = NatGame
            .check(include_str!("../../../copl/007.copl"))
            .expect("fixture should be valid");
        assert_eq!(report.summary, "S(S(Z)) times S(Z) is S(S(Z))");
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z plus Z is Z by P-Zero { Z plus Z is Z by P-Zero {} }";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: P-Zero"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_p_succ_premise() {
        let source = "S(Z) plus Z is S(Z) by P-Succ { Z plus S(Z) is S(Z) by P-Zero {} }";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: P-Succ"));
        assert!(err.message().contains("expected premise: Z plus Z is Z"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_result_term() {
        let source = "Z plus S(Z) is Z by P-Zero {}";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: P-Zero"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_t_succ_premise_link() {
        let source = r#"
S(Z) times Z is Z by T-Succ {
  Z times Z is Z by T-Zero {};
  S(Z) plus Z is S(Z) by P-Succ {
    Z plus Z is Z by P-Zero {}
  }
}
"#;
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: T-Succ"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z plus Z is Z by P-Unknown {}";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: P-Zero, P-Succ, T-Zero, T-Succ"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
S(Z) plus Z is S(Z) by P-Succ {
  Z plus Z is Z by P-Unknown {}
}
"#;
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
