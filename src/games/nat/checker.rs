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
        infer_judgment(&parsed)?;
        Ok(CheckReport {
            game: self.kind(),
            summary: format!("parsed root rule {}", parsed.rule_name),
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
            format!("No such rule: {}", derivation.rule_name),
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

fn wrong_premise_count_message(rule: NatDerivationRule) -> String {
    format!("The number of premises is wrong: {}", rule.name())
}

fn wrong_conclusion_form_message(rule: NatDerivationRule) -> String {
    format!("The form of conclusion is wrong: {}", rule.name())
}

fn wrong_premise_form_message(rule: NatDerivationRule, ordinal: &'static str) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {}",
        rule.name()
    )
}

fn wrong_rule_application_message(rule: NatDerivationRule) -> String {
    format!("Wrong rule application: {}", rule.name())
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
            _ => fail_after_checking_subderivations(derivation, wrong_premise_count_message(rule)),
        },
        _ => fail_after_checking_subderivations(derivation, wrong_conclusion_form_message(rule)),
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
                                wrong_rule_application_message(rule),
                            ))
                        }
                    }
                    _ => Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first"),
                    )),
                }
            }
            _ => fail_after_checking_subderivations(derivation, wrong_premise_count_message(rule)),
        },
        _ => fail_after_checking_subderivations(derivation, wrong_conclusion_form_message(rule)),
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
            _ => fail_after_checking_subderivations(derivation, wrong_premise_count_message(rule)),
        },
        _ => fail_after_checking_subderivations(derivation, wrong_conclusion_form_message(rule)),
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
                                    wrong_rule_application_message(rule),
                                ))
                            }
                        }
                        _ => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second"),
                        )),
                    },
                    _ => Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first"),
                    )),
                }
            }
            _ => fail_after_checking_subderivations(derivation, wrong_premise_count_message(rule)),
        },
        _ => fail_after_checking_subderivations(derivation, wrong_conclusion_form_message(rule)),
    }
}

fn rule_violation(derivation: &NatDerivation, detail: impl Into<String>) -> CheckError {
    let detail = detail.into();
    CheckError::rule_violation(format!(
        "{detail}: {} by {}",
        format_judgment(&derivation.judgment),
        derivation.rule_name
    ))
    .with_span(derivation.span.clone())
}

fn format_judgment(judgment: &NatJudgment) -> String {
    match judgment {
        NatJudgment::PlusIs {
            left,
            right,
            result,
        } => format!(
            "{} plus {} is {}",
            format_term(left),
            format_term(right),
            format_term(result)
        ),
        NatJudgment::TimesIs {
            left,
            right,
            result,
        } => format!(
            "{} times {} is {}",
            format_term(left),
            format_term(right),
            format_term(result)
        ),
    }
}

fn format_term(term: &NatTerm) -> String {
    match term {
        NatTerm::Z => "Z".to_string(),
        NatTerm::S(inner) => format!("S({})", format_term(inner)),
    }
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
            assert!(report.summary.contains("parsed root rule"));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z plus Z is Z by P-Zero { Z plus Z is Z by P-Zero {} }";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: P-Zero"));
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
