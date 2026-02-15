use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{EvalNatExpDerivation, EvalNatExpExpr, EvalNatExpJudgment, NatTerm};

#[derive(Debug, Clone, Copy)]
enum EvalNatExpDerivationRule {
    EConst,
    EPlus,
    ETimes,
    PZero,
    PSucc,
    TZero,
    TSucc,
}

impl EvalNatExpDerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Const" => Some(Self::EConst),
            "E-Plus" => Some(Self::EPlus),
            "E-Times" => Some(Self::ETimes),
            "P-Zero" => Some(Self::PZero),
            "P-Succ" => Some(Self::PSucc),
            "T-Zero" => Some(Self::TZero),
            "T-Succ" => Some(Self::TSucc),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::EConst => "E-Const",
            Self::EPlus => "E-Plus",
            Self::ETimes => "E-Times",
            Self::PZero => "P-Zero",
            Self::PSucc => "P-Succ",
            Self::TZero => "T-Zero",
            Self::TSucc => "T-Succ",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalNatExpGame;

impl Game for EvalNatExpGame {
    fn kind(&self) -> GameKind {
        GameKind::EvalNatExp
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

fn infer_judgment(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalNatExpDerivation,
) -> Result<EvalNatExpJudgment, CheckError> {
    let Some(rule) = EvalNatExpDerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalNatExpDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalNatExpDerivation,
    rule: EvalNatExpDerivationRule,
) -> Result<EvalNatExpJudgment, CheckError> {
    match rule {
        EvalNatExpDerivationRule::EConst => check_e_const(derivation),
        EvalNatExpDerivationRule::EPlus => check_e_plus(derivation),
        EvalNatExpDerivationRule::ETimes => check_e_times(derivation),
        EvalNatExpDerivationRule::PZero => check_p_zero(derivation),
        EvalNatExpDerivationRule::PSucc => check_p_succ(derivation),
        EvalNatExpDerivationRule::TZero => check_t_zero(derivation),
        EvalNatExpDerivationRule::TSucc => check_t_succ(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalNatExpDerivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalNatExpDerivation,
    detail: String,
) -> Result<EvalNatExpJudgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Const, E-Plus, E-Times, P-Zero, P-Succ, T-Zero, T-Succ; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalNatExpDerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalNatExpDerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_premise_form_message(
    rule: EvalNatExpDerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalNatExpJudgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalNatExpDerivationRule,
    expected: [&EvalNatExpJudgment; 3],
    actual: [&EvalNatExpJudgment; 3],
    fix: &'static str,
) -> String {
    let [expected_first, expected_second, expected_third] = expected;
    let [actual_first, actual_second, actual_third] = actual;
    format!(
        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], [{expected_third}], actual premises: [{actual_first}], [{actual_second}], [{actual_third}]; fix: {fix})",
        rule.name(),
    )
}

fn check_e_const(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::EConst;
    match &derivation.judgment {
        EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Nat(n1),
            value: n2,
        } if n1 == n2 => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n evalto n"),
        ),
    }
}

fn check_e_plus(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::EPlus;
    match &derivation.judgment {
        EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Plus(e1, e2),
            value: n,
        } => match derivation.subderivations.as_slice() {
            [d1, d2, d3] => {
                let first = infer_judgment(d1)?;
                let second = infer_judgment(d2)?;
                let third = infer_judgment(d3)?;

                let (first_expr, n1) = match &first {
                    EvalNatExpJudgment::EvalTo { expr, value } => (expr, value),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e1 evalto n1", actual),
                        ));
                    }
                };
                let (second_expr, n2) = match &second {
                    EvalNatExpJudgment::EvalTo { expr, value } => (expr, value),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second", "e2 evalto n2", actual),
                        ));
                    }
                };
                let (third_left, third_right, third_result) = match &third {
                    EvalNatExpJudgment::PlusIs {
                        left,
                        right,
                        result,
                    } => (left, right, result),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "third", "n1 plus n2 is n", actual),
                        ));
                    }
                };

                if first_expr == e1.as_ref()
                    && second_expr == e2.as_ref()
                    && third_left == n1
                    && third_right == n2
                    && third_result == n
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            [
                                &EvalNatExpJudgment::EvalTo {
                                    expr: e1.as_ref().clone(),
                                    value: n1.clone(),
                                },
                                &EvalNatExpJudgment::EvalTo {
                                    expr: e2.as_ref().clone(),
                                    value: n2.clone(),
                                },
                                &EvalNatExpJudgment::PlusIs {
                                    left: n1.clone(),
                                    right: n2.clone(),
                                    result: n.clone(),
                                },
                            ],
                            [&first, &second, &third],
                            "make e1/e2/n1/n2/n links consistent across conclusion and all premises",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 + e2 evalto n"),
        ),
    }
}

fn check_e_times(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::ETimes;
    match &derivation.judgment {
        EvalNatExpJudgment::EvalTo {
            expr: EvalNatExpExpr::Times(e1, e2),
            value: n,
        } => match derivation.subderivations.as_slice() {
            [d1, d2, d3] => {
                let first = infer_judgment(d1)?;
                let second = infer_judgment(d2)?;
                let third = infer_judgment(d3)?;

                let (first_expr, n1) = match &first {
                    EvalNatExpJudgment::EvalTo { expr, value } => (expr, value),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e1 evalto n1", actual),
                        ));
                    }
                };
                let (second_expr, n2) = match &second {
                    EvalNatExpJudgment::EvalTo { expr, value } => (expr, value),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second", "e2 evalto n2", actual),
                        ));
                    }
                };
                let (third_left, third_right, third_result) = match &third {
                    EvalNatExpJudgment::TimesIs {
                        left,
                        right,
                        result,
                    } => (left, right, result),
                    actual => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "third", "n1 times n2 is n", actual),
                        ));
                    }
                };

                if first_expr == e1.as_ref()
                    && second_expr == e2.as_ref()
                    && third_left == n1
                    && third_right == n2
                    && third_result == n
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            [
                                &EvalNatExpJudgment::EvalTo {
                                    expr: e1.as_ref().clone(),
                                    value: n1.clone(),
                                },
                                &EvalNatExpJudgment::EvalTo {
                                    expr: e2.as_ref().clone(),
                                    value: n2.clone(),
                                },
                                &EvalNatExpJudgment::TimesIs {
                                    left: n1.clone(),
                                    right: n2.clone(),
                                    result: n.clone(),
                                },
                            ],
                            [&first, &second, &third],
                            "make e1/e2/n1/n2/n links consistent across conclusion and all premises",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 * e2 evalto n"),
        ),
    }
}

fn check_p_zero(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::PZero;
    match &derivation.judgment {
        EvalNatExpJudgment::PlusIs {
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

fn check_p_succ(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::PSucc;
    match &derivation.judgment {
        EvalNatExpJudgment::PlusIs {
            left: NatTerm::S(n1),
            right: n2,
            result: NatTerm::S(result_inner),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let premise = infer_judgment(d1)?;
                match premise {
                    EvalNatExpJudgment::PlusIs {
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
                                format!(
                                    "Wrong rule application: P-Succ (expected premise: {} plus {} is {}, actual premise: {}; fix: make premise terms consistent with conclusion S(n1) plus n2 is S(n))",
                                    n1.as_ref(),
                                    n2,
                                    result_inner.as_ref(),
                                    EvalNatExpJudgment::PlusIs {
                                        left: premise_n1,
                                        right: premise_n2,
                                        result: premise_n,
                                    }
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

fn check_t_zero(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::TZero;
    match &derivation.judgment {
        EvalNatExpJudgment::TimesIs {
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

fn check_t_succ(derivation: &EvalNatExpDerivation) -> Result<EvalNatExpJudgment, CheckError> {
    let rule = EvalNatExpDerivationRule::TSucc;
    match &derivation.judgment {
        EvalNatExpJudgment::TimesIs {
            left: NatTerm::S(n1),
            right: n2,
            result: n4,
        } => match derivation.subderivations.as_slice() {
            [d1, d2] => {
                let first_premise = infer_judgment(d1)?;
                let second_premise = infer_judgment(d2)?;
                match first_premise {
                    EvalNatExpJudgment::TimesIs {
                        left: first_n1,
                        right: first_n2,
                        result: first_n3,
                    } => match second_premise {
                        EvalNatExpJudgment::PlusIs {
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
                                    format!(
                                        "Wrong rule application: T-Succ (expected links: first premise {} times {} is n3, second premise {} plus n3 is {}; actual premises: [{}] and [{}]; fix: make shared terms n2/n3/n4 consistent across conclusion and both premises)",
                                        n1.as_ref(),
                                        n2,
                                        n2,
                                        n4,
                                        EvalNatExpJudgment::TimesIs {
                                            left: first_n1,
                                            right: first_n2,
                                            result: first_n3,
                                        },
                                        EvalNatExpJudgment::PlusIs {
                                            left: second_n2,
                                            right: second_n3,
                                            result: second_n4,
                                        }
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

fn rule_violation(derivation: &EvalNatExpDerivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalNatExpGame;

    #[test]
    fn reports_root_judgment_text_for_all_eval_nat_exp_fixtures() {
        let game = EvalNatExpGame;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/015.copl"),
                "Z + S(S(Z)) evalto S(S(Z))",
            ),
            (
                include_str!("../../../copl/016.copl"),
                "S(S(Z)) + Z evalto S(S(Z))",
            ),
            (
                include_str!("../../../copl/017.copl"),
                "S(Z) + S(Z) + S(Z) evalto S(S(S(Z)))",
            ),
            (
                include_str!("../../../copl/018.copl"),
                "S(S(S(Z))) + S(S(Z)) * S(Z) evalto S(S(S(S(S(Z)))))",
            ),
            (
                include_str!("../../../copl/019.copl"),
                "(S(S(Z)) + S(S(Z))) * Z evalto Z",
            ),
            (
                include_str!("../../../copl/020.copl"),
                "Z * (S(S(Z)) + S(S(Z))) evalto Z",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z evalto Z by E-Const { Z evalto Z by E-Const {} }";
        let err = EvalNatExpGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Const"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        assert!(err.message().contains("premise path: root"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_e_const_conclusion() {
        let source = "Z + Z evalto Z by E-Const {}";
        let err = EvalNatExpGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: E-Const"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_e_plus_premises() {
        let source = r#"
Z + Z evalto Z by E-Plus {
  S(Z) evalto S(Z) by E-Const {};
  Z evalto Z by E-Const {};
  S(Z) plus Z is S(Z) by P-Succ {
    Z plus Z is Z by P-Zero {}
  }
}
"#;
        let err = EvalNatExpGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: E-Plus"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z evalto Z by E-Unknown {}";
        let err = EvalNatExpGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: E-Const, E-Plus, E-Times"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
Z + Z evalto Z by E-Plus {
  Z evalto Z by E-Const {};
  Z evalto Z by E-Unknown {};
  Z plus Z is Z by P-Zero {}
}
"#;
        let err = EvalNatExpGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 2"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 4);
        assert_eq!(span.column, 3);
    }
}
