use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::syntax::{NatTerm, ReduceNatExpDerivation, ReduceNatExpExpr, ReduceNatExpJudgment};

#[derive(Debug, Clone, Copy)]
enum ReduceNatExpDerivationRule {
    RPlus,
    RTimes,
    RPlusL,
    RPlusR,
    RTimesL,
    RTimesR,
    DRPlus,
    DRTimes,
    DRPlusL,
    DRPlusR,
    DRTimesL,
    DRTimesR,
    MRZero,
    MRMulti,
    MROne,
    PZero,
    PSucc,
    TZero,
    TSucc,
}

impl ReduceNatExpDerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "R-Plus" => Some(Self::RPlus),
            "R-Times" => Some(Self::RTimes),
            "R-PlusL" => Some(Self::RPlusL),
            "R-PlusR" => Some(Self::RPlusR),
            "R-TimesL" => Some(Self::RTimesL),
            "R-TimesR" => Some(Self::RTimesR),
            "DR-Plus" => Some(Self::DRPlus),
            "DR-Times" => Some(Self::DRTimes),
            "DR-PlusL" => Some(Self::DRPlusL),
            "DR-PlusR" => Some(Self::DRPlusR),
            "DR-TimesL" => Some(Self::DRTimesL),
            "DR-TimesR" => Some(Self::DRTimesR),
            "MR-Zero" => Some(Self::MRZero),
            "MR-Multi" => Some(Self::MRMulti),
            "MR-One" => Some(Self::MROne),
            "P-Zero" => Some(Self::PZero),
            "P-Succ" => Some(Self::PSucc),
            "T-Zero" => Some(Self::TZero),
            "T-Succ" => Some(Self::TSucc),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::RPlus => "R-Plus",
            Self::RTimes => "R-Times",
            Self::RPlusL => "R-PlusL",
            Self::RPlusR => "R-PlusR",
            Self::RTimesL => "R-TimesL",
            Self::RTimesR => "R-TimesR",
            Self::DRPlus => "DR-Plus",
            Self::DRTimes => "DR-Times",
            Self::DRPlusL => "DR-PlusL",
            Self::DRPlusR => "DR-PlusR",
            Self::DRTimesL => "DR-TimesL",
            Self::DRTimesR => "DR-TimesR",
            Self::MRZero => "MR-Zero",
            Self::MRMulti => "MR-Multi",
            Self::MROne => "MR-One",
            Self::PZero => "P-Zero",
            Self::PSucc => "P-Succ",
            Self::TZero => "T-Zero",
            Self::TSucc => "T-Succ",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ReduceNatExpGame;

impl Game for ReduceNatExpGame {
    fn kind(&self) -> GameKind {
        GameKind::ReduceNatExp
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

fn infer_judgment(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let Some(rule) = ReduceNatExpDerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &ReduceNatExpDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &ReduceNatExpDerivation,
    rule: ReduceNatExpDerivationRule,
) -> Result<ReduceNatExpJudgment, CheckError> {
    match rule {
        ReduceNatExpDerivationRule::RPlus => check_r_plus(derivation),
        ReduceNatExpDerivationRule::RTimes => check_r_times(derivation),
        ReduceNatExpDerivationRule::RPlusL => check_r_plus_l(derivation),
        ReduceNatExpDerivationRule::RPlusR => check_r_plus_r(derivation),
        ReduceNatExpDerivationRule::RTimesL => check_r_times_l(derivation),
        ReduceNatExpDerivationRule::RTimesR => check_r_times_r(derivation),
        ReduceNatExpDerivationRule::DRPlus => check_dr_plus(derivation),
        ReduceNatExpDerivationRule::DRTimes => check_dr_times(derivation),
        ReduceNatExpDerivationRule::DRPlusL => check_dr_plus_l(derivation),
        ReduceNatExpDerivationRule::DRPlusR => check_dr_plus_r(derivation),
        ReduceNatExpDerivationRule::DRTimesL => check_dr_times_l(derivation),
        ReduceNatExpDerivationRule::DRTimesR => check_dr_times_r(derivation),
        ReduceNatExpDerivationRule::MRZero => check_mr_zero(derivation),
        ReduceNatExpDerivationRule::MRMulti => check_mr_multi(derivation),
        ReduceNatExpDerivationRule::MROne => check_mr_one(derivation),
        ReduceNatExpDerivationRule::PZero => check_p_zero(derivation),
        ReduceNatExpDerivationRule::PSucc => check_p_succ(derivation),
        ReduceNatExpDerivationRule::TZero => check_t_zero(derivation),
        ReduceNatExpDerivationRule::TSucc => check_t_succ(derivation),
    }
}

fn check_all_subderivations(subderivations: &[ReduceNatExpDerivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &ReduceNatExpDerivation,
    detail: String,
) -> Result<ReduceNatExpJudgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn as_reduces_to(
    judgment: &ReduceNatExpJudgment,
) -> Option<(&ReduceNatExpExpr, &ReduceNatExpExpr)> {
    match judgment {
        ReduceNatExpJudgment::ReducesTo { from, to } => Some((from, to)),
        _ => None,
    }
}

fn as_deterministic_reduces_to(
    judgment: &ReduceNatExpJudgment,
) -> Option<(&ReduceNatExpExpr, &ReduceNatExpExpr)> {
    match judgment {
        ReduceNatExpJudgment::DeterministicReducesTo { from, to } => Some((from, to)),
        _ => None,
    }
}

fn as_multi_reduces_to(
    judgment: &ReduceNatExpJudgment,
) -> Option<(&ReduceNatExpExpr, &ReduceNatExpExpr)> {
    match judgment {
        ReduceNatExpJudgment::MultiReducesTo { from, to } => Some((from, to)),
        _ => None,
    }
}

fn as_nat_expr(expr: &ReduceNatExpExpr) -> Option<&NatTerm> {
    match expr {
        ReduceNatExpExpr::Nat(n) => Some(n),
        _ => None,
    }
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: R-Plus, R-Times, R-PlusL, R-PlusR, R-TimesL, R-TimesR, DR-Plus, DR-Times, DR-PlusL, DR-PlusR, DR-TimesL, DR-TimesR, MR-Zero, MR-Multi, MR-One, P-Zero, P-Succ, T-Zero, T-Succ; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: ReduceNatExpDerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: ReduceNatExpDerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_premise_form_message(
    rule: ReduceNatExpDerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &ReduceNatExpJudgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: ReduceNatExpDerivationRule,
    expected: &[ReduceNatExpJudgment],
    actual: &[&ReduceNatExpJudgment],
    fix: &'static str,
) -> String {
    let expected_text = expected
        .iter()
        .map(|j| format!("[{j}]"))
        .collect::<Vec<_>>()
        .join(", ");
    let actual_text = actual
        .iter()
        .map(|j| format!("[{j}]"))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "Wrong rule application: {} (expected premises: {expected_text}, actual premises: {actual_text}; fix: {fix})",
        rule.name(),
    )
}

fn check_r_plus(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RPlus;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Nat(n3),
        } => {
            let (Some(n1), Some(n2)) = (as_nat_expr(e1), as_nat_expr(e2)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 + n2 ---> n3"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    match &first {
                        ReduceNatExpJudgment::PlusIs {
                            left,
                            right,
                            result,
                        } => {
                            if left == n1 && right == n2 && result == n3 {
                                Ok(derivation.judgment.clone())
                            } else {
                                Err(rule_violation(
                                    derivation,
                                    wrong_rule_application_message(
                                        rule,
                                        &[ReduceNatExpJudgment::PlusIs {
                                            left: n1.clone(),
                                            right: n2.clone(),
                                            result: n3.clone(),
                                        }],
                                        &[&first],
                                        "make n1/n2/n3 links consistent across conclusion and premise",
                                    ),
                                ))
                            }
                        }
                        actual => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "n1 plus n2 is n3", actual),
                        )),
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 + n2 ---> n3"),
        ),
    }
}

fn check_r_times(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RTimes;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Nat(n3),
        } => {
            let (Some(n1), Some(n2)) = (as_nat_expr(e1), as_nat_expr(e2)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 * n2 ---> n3"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    match &first {
                        ReduceNatExpJudgment::TimesIs {
                            left,
                            right,
                            result,
                        } => {
                            if left == n1 && right == n2 && result == n3 {
                                Ok(derivation.judgment.clone())
                            } else {
                                Err(rule_violation(
                                    derivation,
                                    wrong_rule_application_message(
                                        rule,
                                        &[ReduceNatExpJudgment::TimesIs {
                                            left: n1.clone(),
                                            right: n2.clone(),
                                            result: n3.clone(),
                                        }],
                                        &[&first],
                                        "make n1/n2/n3 links consistent across conclusion and premise",
                                    ),
                                ))
                            }
                        }
                        actual => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "n1 times n2 is n3", actual),
                        )),
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 * n2 ---> n3"),
        ),
    }
}

fn check_r_plus_l(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RPlusL;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Plus(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 ---> e1'", &first),
                    ));
                };

                if premise_from == e1.as_ref() && premise_to == e1_prime.as_ref() && e2_prime == e2
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::ReducesTo {
                                from: e1.as_ref().clone(),
                                to: e1_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "make e1/e1'/e2 links consistent across conclusion and premise",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 + e2 ---> e1' + e2"),
        ),
    }
}

fn check_r_plus_r(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RPlusR;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Plus(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e2 ---> e2'", &first),
                    ));
                };

                if e1 == e1_prime && premise_from == e2.as_ref() && premise_to == e2_prime.as_ref()
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::ReducesTo {
                                from: e2.as_ref().clone(),
                                to: e2_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "keep the left expression fixed and make e2/e2' links consistent",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 + e2 ---> e1 + e2'"),
        ),
    }
}

fn check_r_times_l(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RTimesL;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Times(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 ---> e1'", &first),
                    ));
                };

                if premise_from == e1.as_ref() && premise_to == e1_prime.as_ref() && e2_prime == e2
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::ReducesTo {
                                from: e1.as_ref().clone(),
                                to: e1_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "make e1/e1'/e2 links consistent across conclusion and premise",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 * e2 ---> e1' * e2"),
        ),
    }
}

fn check_r_times_r(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::RTimesR;
    match &derivation.judgment {
        ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Times(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e2 ---> e2'", &first),
                    ));
                };

                if e1 == e1_prime && premise_from == e2.as_ref() && premise_to == e2_prime.as_ref()
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::ReducesTo {
                                from: e2.as_ref().clone(),
                                to: e2_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "keep the left expression fixed and make e2/e2' links consistent",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 * e2 ---> e1 * e2'"),
        ),
    }
}

fn check_dr_plus(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRPlus;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Nat(n3),
        } => {
            let (Some(n1), Some(n2)) = (as_nat_expr(e1), as_nat_expr(e2)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 + n2 -d-> n3"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    match &first {
                        ReduceNatExpJudgment::PlusIs {
                            left,
                            right,
                            result,
                        } => {
                            if left == n1 && right == n2 && result == n3 {
                                Ok(derivation.judgment.clone())
                            } else {
                                Err(rule_violation(
                                    derivation,
                                    wrong_rule_application_message(
                                        rule,
                                        &[ReduceNatExpJudgment::PlusIs {
                                            left: n1.clone(),
                                            right: n2.clone(),
                                            result: n3.clone(),
                                        }],
                                        &[&first],
                                        "make n1/n2/n3 links consistent across conclusion and premise",
                                    ),
                                ))
                            }
                        }
                        actual => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "n1 plus n2 is n3", actual),
                        )),
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 + n2 -d-> n3"),
        ),
    }
}

fn check_dr_times(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRTimes;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Nat(n3),
        } => {
            let (Some(n1), Some(n2)) = (as_nat_expr(e1), as_nat_expr(e2)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 * n2 -d-> n3"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    match &first {
                        ReduceNatExpJudgment::TimesIs {
                            left,
                            right,
                            result,
                        } => {
                            if left == n1 && right == n2 && result == n3 {
                                Ok(derivation.judgment.clone())
                            } else {
                                Err(rule_violation(
                                    derivation,
                                    wrong_rule_application_message(
                                        rule,
                                        &[ReduceNatExpJudgment::TimesIs {
                                            left: n1.clone(),
                                            right: n2.clone(),
                                            result: n3.clone(),
                                        }],
                                        &[&first],
                                        "make n1/n2/n3 links consistent across conclusion and premise",
                                    ),
                                ))
                            }
                        }
                        actual => Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "n1 times n2 is n3", actual),
                        )),
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 * n2 -d-> n3"),
        ),
    }
}

fn check_dr_plus_l(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRPlusL;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Plus(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_deterministic_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 -d-> e1'", &first),
                    ));
                };

                if premise_from == e1.as_ref() && premise_to == e1_prime.as_ref() && e2_prime == e2
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::DeterministicReducesTo {
                                from: e1.as_ref().clone(),
                                to: e1_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "make e1/e1'/e2 links consistent across conclusion and premise",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 + e2 -d-> e1' + e2"),
        ),
    }
}

fn check_dr_plus_r(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRPlusR;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Plus(e1, e2),
            to: ReduceNatExpExpr::Plus(e1_prime, e2_prime),
        } => {
            let (Some(n1), Some(n1_prime)) = (as_nat_expr(e1), as_nat_expr(e1_prime)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 + e2 -d-> n1 + e2'"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    let Some((premise_from, premise_to)) = as_deterministic_reduces_to(&first)
                    else {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e2 -d-> e2'", &first),
                        ));
                    };

                    if n1 == n1_prime
                        && premise_from == e2.as_ref()
                        && premise_to == e2_prime.as_ref()
                    {
                        Ok(derivation.judgment.clone())
                    } else {
                        Err(rule_violation(
                            derivation,
                            wrong_rule_application_message(
                                rule,
                                &[ReduceNatExpJudgment::DeterministicReducesTo {
                                    from: e2.as_ref().clone(),
                                    to: e2_prime.as_ref().clone(),
                                }],
                                &[&first],
                                "keep the left Nat fixed and make e2/e2' links consistent",
                            ),
                        ))
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 + e2 -d-> n1 + e2'"),
        ),
    }
}

fn check_dr_times_l(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRTimesL;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Times(e1_prime, e2_prime),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((premise_from, premise_to)) = as_deterministic_reduces_to(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 -d-> e1'", &first),
                    ));
                };

                if premise_from == e1.as_ref() && premise_to == e1_prime.as_ref() && e2_prime == e2
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[ReduceNatExpJudgment::DeterministicReducesTo {
                                from: e1.as_ref().clone(),
                                to: e1_prime.as_ref().clone(),
                            }],
                            &[&first],
                            "make e1/e1'/e2 links consistent across conclusion and premise",
                        ),
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
            wrong_conclusion_form_message(rule, "e1 * e2 -d-> e1' * e2"),
        ),
    }
}

fn check_dr_times_r(
    derivation: &ReduceNatExpDerivation,
) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::DRTimesR;
    match &derivation.judgment {
        ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Times(e1, e2),
            to: ReduceNatExpExpr::Times(e1_prime, e2_prime),
        } => {
            let (Some(n1), Some(n1_prime)) = (as_nat_expr(e1), as_nat_expr(e1_prime)) else {
                return fail_after_checking_subderivations(
                    derivation,
                    wrong_conclusion_form_message(rule, "n1 * e2 -d-> n1 * e2'"),
                );
            };

            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    let Some((premise_from, premise_to)) = as_deterministic_reduces_to(&first)
                    else {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e2 -d-> e2'", &first),
                        ));
                    };

                    if n1 == n1_prime
                        && premise_from == e2.as_ref()
                        && premise_to == e2_prime.as_ref()
                    {
                        Ok(derivation.judgment.clone())
                    } else {
                        Err(rule_violation(
                            derivation,
                            wrong_rule_application_message(
                                rule,
                                &[ReduceNatExpJudgment::DeterministicReducesTo {
                                    from: e2.as_ref().clone(),
                                    to: e2_prime.as_ref().clone(),
                                }],
                                &[&first],
                                "keep the left Nat fixed and make e2/e2' links consistent",
                            ),
                        ))
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "n1 * e2 -d-> n1 * e2'"),
        ),
    }
}

fn check_mr_zero(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::MRZero;
    match &derivation.judgment {
        ReduceNatExpJudgment::MultiReducesTo { from, to } if from == to => {
            match derivation.subderivations.as_slice() {
                [] => Ok(derivation.judgment.clone()),
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e -*-> e"),
        ),
    }
}

fn check_mr_multi(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::MRMulti;
    match &derivation.judgment {
        ReduceNatExpJudgment::MultiReducesTo { from, to } => {
            match derivation.subderivations.as_slice() {
                [d1, d2] => {
                    let first = infer_judgment(d1)?;
                    let second = infer_judgment(d2)?;

                    let Some((first_from, first_to)) = as_multi_reduces_to(&first) else {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e -*-> e'", &first),
                        ));
                    };
                    let Some((second_from, second_to)) = as_multi_reduces_to(&second) else {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second", "e' -*-> e''", &second),
                        ));
                    };

                    if first_from == from && first_to == second_from && second_to == to {
                        Ok(derivation.judgment.clone())
                    } else {
                        Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[
                                ReduceNatExpJudgment::MultiReducesTo {
                                    from: from.clone(),
                                    to: first_to.clone(),
                                },
                                ReduceNatExpJudgment::MultiReducesTo {
                                    from: first_to.clone(),
                                    to: to.clone(),
                                },
                            ],
                            &[&first, &second],
                            "make e/e'/e'' links consistent across conclusion and both premises",
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
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e -*-> e''"),
        ),
    }
}

fn check_mr_one(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::MROne;
    match &derivation.judgment {
        ReduceNatExpJudgment::MultiReducesTo { from, to } => {
            match derivation.subderivations.as_slice() {
                [d1] => {
                    let first = infer_judgment(d1)?;
                    let Some((premise_from, premise_to)) = as_reduces_to(&first) else {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "e ---> e'", &first),
                        ));
                    };

                    if premise_from == from && premise_to == to {
                        Ok(derivation.judgment.clone())
                    } else {
                        Err(rule_violation(
                            derivation,
                            wrong_rule_application_message(
                                rule,
                                &[ReduceNatExpJudgment::ReducesTo {
                                    from: from.clone(),
                                    to: to.clone(),
                                }],
                                &[&first],
                                "make e/e' links consistent across conclusion and premise",
                            ),
                        ))
                    }
                }
                _ => fail_after_checking_subderivations(
                    derivation,
                    wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
                ),
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e -*-> e'"),
        ),
    }
}

fn check_p_zero(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::PZero;
    match &derivation.judgment {
        ReduceNatExpJudgment::PlusIs {
            left: NatTerm::Z,
            right,
            result,
        } if right == result => match derivation.subderivations.as_slice() {
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

fn check_p_succ(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::PSucc;
    match &derivation.judgment {
        ReduceNatExpJudgment::PlusIs {
            left: NatTerm::S(n1),
            right: n2,
            result: NatTerm::S(n3),
        } => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                match &first {
                    ReduceNatExpJudgment::PlusIs {
                        left,
                        right,
                        result,
                    } => {
                        if left == n1.as_ref() && right == n2 && result == n3.as_ref() {
                            Ok(derivation.judgment.clone())
                        } else {
                            Err(rule_violation(
                                derivation,
                                wrong_rule_application_message(
                                    rule,
                                    &[ReduceNatExpJudgment::PlusIs {
                                        left: n1.as_ref().clone(),
                                        right: n2.clone(),
                                        result: n3.as_ref().clone(),
                                    }],
                                    &[&first],
                                    "make n1/n2/n links consistent across conclusion and premise",
                                ),
                            ))
                        }
                    }
                    actual => Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "n1 plus n2 is n", actual),
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

fn check_t_zero(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::TZero;
    match &derivation.judgment {
        ReduceNatExpJudgment::TimesIs {
            left: NatTerm::Z,
            result: NatTerm::Z,
            ..
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

fn check_t_succ(derivation: &ReduceNatExpDerivation) -> Result<ReduceNatExpJudgment, CheckError> {
    let rule = ReduceNatExpDerivationRule::TSucc;
    match &derivation.judgment {
        ReduceNatExpJudgment::TimesIs {
            left: NatTerm::S(n1),
            right: n2,
            result: n4,
        } => match derivation.subderivations.as_slice() {
            [d1, d2] => {
                let first = infer_judgment(d1)?;
                let second = infer_judgment(d2)?;

                let (left, right, n3) = match &first {
                    ReduceNatExpJudgment::TimesIs {
                        left,
                        right,
                        result,
                    } => (left, right, result),
                    _ => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "first", "n1 times n2 is n3", &first),
                        ));
                    }
                };
                let (plus_left, plus_right, plus_result) = match &second {
                    ReduceNatExpJudgment::PlusIs {
                        left,
                        right,
                        result,
                    } => (left, right, result),
                    _ => {
                        return Err(rule_violation(
                            derivation,
                            wrong_premise_form_message(rule, "second", "n2 plus n3 is n4", &second),
                        ));
                    }
                };

                if left == n1.as_ref()
                    && right == n2
                    && plus_left == n2
                    && plus_right == n3
                    && plus_result == n4
                {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[
                                ReduceNatExpJudgment::TimesIs {
                                    left: n1.as_ref().clone(),
                                    right: n2.clone(),
                                    result: plus_right.clone(),
                                },
                                ReduceNatExpJudgment::PlusIs {
                                    left: n2.clone(),
                                    right: plus_right.clone(),
                                    result: n4.clone(),
                                },
                            ],
                            &[&first, &second],
                            "make n1/n2/n3/n4 links consistent across conclusion and both premises",
                        ),
                    ))
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

fn rule_violation(derivation: &ReduceNatExpDerivation, detail: impl Into<String>) -> CheckError {
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

    use super::ReduceNatExpGame;

    #[test]
    fn reports_root_judgment_text_for_all_reduce_nat_exp_fixtures() {
        let game = ReduceNatExpGame;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/021.copl"),
                "Z + S(S(Z)) -*-> S(S(Z))",
            ),
            (
                include_str!("../../../copl/022.copl"),
                "S(Z) * S(Z) + S(Z) * S(Z) -d-> S(Z) + S(Z) * S(Z)",
            ),
            (
                include_str!("../../../copl/023.copl"),
                "S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) * S(Z) + S(Z)",
            ),
            (
                include_str!("../../../copl/024.copl"),
                "S(Z) * S(Z) + S(Z) * S(Z) -*-> S(S(Z))",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z -*-> Z by MR-One {}";
        let err = ReduceNatExpGame
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: MR-One"));
        assert!(err.message().contains("expected: 1, actual: 0"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_mr_zero_conclusion() {
        let source = "Z -*-> S(Z) by MR-Zero {}";
        let err = ReduceNatExpGame
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: MR-Zero"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_mr_multi_premises() {
        let source = r#"
Z -*-> S(Z) by MR-Multi {
  Z -*-> Z by MR-Zero {};
  S(Z) -*-> S(Z) by MR-Zero {}
}
"#;
        let err = ReduceNatExpGame
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: MR-Multi"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z -*-> Z by MR-Unknown {}";
        let err = ReduceNatExpGame
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: R-Plus, R-Times"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
Z -*-> Z by MR-One {
  Z ---> Z by R-Unknown {}
}
"#;
        let err = ReduceNatExpGame
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
