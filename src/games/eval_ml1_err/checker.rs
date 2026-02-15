use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalML1ErrBinOp, EvalML1ErrDerivation, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
};

#[derive(Debug, Clone, Copy)]
enum EvalML1ErrDerivationRule {
    EInt,
    EBool,
    EIfT,
    EIfF,
    EIfInt,
    EIfTError,
    EIfFError,
    EPlus,
    EPlusBoolL,
    EPlusBoolR,
    EPlusErrorL,
    EPlusErrorR,
    EMinus,
    EMinusBoolL,
    EMinusBoolR,
    EMinusErrorL,
    EMinusErrorR,
    ETimes,
    ETimesBoolL,
    ETimesBoolR,
    ETimesErrorL,
    ETimesErrorR,
    ELt,
    ELtBoolL,
    ELtBoolR,
    ELtErrorL,
    ELtErrorR,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalML1ErrDerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-IfT" => Some(Self::EIfT),
            "E-IfF" => Some(Self::EIfF),
            "E-IfInt" => Some(Self::EIfInt),
            "E-IfTError" => Some(Self::EIfTError),
            "E-IfFError" => Some(Self::EIfFError),
            "E-Plus" => Some(Self::EPlus),
            "E-PlusBoolL" => Some(Self::EPlusBoolL),
            "E-PlusBoolR" => Some(Self::EPlusBoolR),
            "E-PlusErrorL" => Some(Self::EPlusErrorL),
            "E-PlusErrorR" => Some(Self::EPlusErrorR),
            "E-Minus" => Some(Self::EMinus),
            "E-MinusBoolL" => Some(Self::EMinusBoolL),
            "E-MinusBoolR" => Some(Self::EMinusBoolR),
            "E-MinusErrorL" => Some(Self::EMinusErrorL),
            "E-MinusErrorR" => Some(Self::EMinusErrorR),
            "E-Times" => Some(Self::ETimes),
            "E-TimesBoolL" => Some(Self::ETimesBoolL),
            "E-TimesBoolR" => Some(Self::ETimesBoolR),
            "E-TimesErrorL" => Some(Self::ETimesErrorL),
            "E-TimesErrorR" => Some(Self::ETimesErrorR),
            "E-Lt" => Some(Self::ELt),
            "E-LtBoolL" => Some(Self::ELtBoolL),
            "E-LtBoolR" => Some(Self::ELtBoolR),
            "E-LtErrorL" => Some(Self::ELtErrorL),
            "E-LtErrorR" => Some(Self::ELtErrorR),
            "B-Plus" => Some(Self::BPlus),
            "B-Minus" => Some(Self::BMinus),
            "B-Times" => Some(Self::BTimes),
            "B-Lt" => Some(Self::BLt),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::EInt => "E-Int",
            Self::EBool => "E-Bool",
            Self::EIfT => "E-IfT",
            Self::EIfF => "E-IfF",
            Self::EIfInt => "E-IfInt",
            Self::EIfTError => "E-IfTError",
            Self::EIfFError => "E-IfFError",
            Self::EPlus => "E-Plus",
            Self::EPlusBoolL => "E-PlusBoolL",
            Self::EPlusBoolR => "E-PlusBoolR",
            Self::EPlusErrorL => "E-PlusErrorL",
            Self::EPlusErrorR => "E-PlusErrorR",
            Self::EMinus => "E-Minus",
            Self::EMinusBoolL => "E-MinusBoolL",
            Self::EMinusBoolR => "E-MinusBoolR",
            Self::EMinusErrorL => "E-MinusErrorL",
            Self::EMinusErrorR => "E-MinusErrorR",
            Self::ETimes => "E-Times",
            Self::ETimesBoolL => "E-TimesBoolL",
            Self::ETimesBoolR => "E-TimesBoolR",
            Self::ETimesErrorL => "E-TimesErrorL",
            Self::ETimesErrorR => "E-TimesErrorR",
            Self::ELt => "E-Lt",
            Self::ELtBoolL => "E-LtBoolL",
            Self::ELtBoolR => "E-LtBoolR",
            Self::ELtErrorL => "E-LtErrorL",
            Self::ELtErrorR => "E-LtErrorR",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalML1ErrGame;

impl Game for EvalML1ErrGame {
    fn kind(&self) -> GameKind {
        GameKind::EvalML1Err
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

fn infer_judgment(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    let Some(rule) = EvalML1ErrDerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalML1ErrDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalML1ErrDerivation,
    rule: EvalML1ErrDerivationRule,
) -> Result<EvalML1ErrJudgment, CheckError> {
    match rule {
        EvalML1ErrDerivationRule::EInt => check_e_int(derivation),
        EvalML1ErrDerivationRule::EBool => check_e_bool(derivation),
        EvalML1ErrDerivationRule::EIfT => check_e_if_t(derivation),
        EvalML1ErrDerivationRule::EIfF => check_e_if_f(derivation),
        EvalML1ErrDerivationRule::EIfInt => check_e_if_int(derivation),
        EvalML1ErrDerivationRule::EIfTError => check_e_if_t_error(derivation),
        EvalML1ErrDerivationRule::EIfFError => check_e_if_f_error(derivation),
        EvalML1ErrDerivationRule::EPlus => check_e_plus(derivation),
        EvalML1ErrDerivationRule::EPlusBoolL => check_e_plus_bool_l(derivation),
        EvalML1ErrDerivationRule::EPlusBoolR => check_e_plus_bool_r(derivation),
        EvalML1ErrDerivationRule::EPlusErrorL => check_e_plus_error_l(derivation),
        EvalML1ErrDerivationRule::EPlusErrorR => check_e_plus_error_r(derivation),
        EvalML1ErrDerivationRule::EMinus => check_e_minus(derivation),
        EvalML1ErrDerivationRule::EMinusBoolL => check_e_minus_bool_l(derivation),
        EvalML1ErrDerivationRule::EMinusBoolR => check_e_minus_bool_r(derivation),
        EvalML1ErrDerivationRule::EMinusErrorL => check_e_minus_error_l(derivation),
        EvalML1ErrDerivationRule::EMinusErrorR => check_e_minus_error_r(derivation),
        EvalML1ErrDerivationRule::ETimes => check_e_times(derivation),
        EvalML1ErrDerivationRule::ETimesBoolL => check_e_times_bool_l(derivation),
        EvalML1ErrDerivationRule::ETimesBoolR => check_e_times_bool_r(derivation),
        EvalML1ErrDerivationRule::ETimesErrorL => check_e_times_error_l(derivation),
        EvalML1ErrDerivationRule::ETimesErrorR => check_e_times_error_r(derivation),
        EvalML1ErrDerivationRule::ELt => check_e_lt(derivation),
        EvalML1ErrDerivationRule::ELtBoolL => check_e_lt_bool_l(derivation),
        EvalML1ErrDerivationRule::ELtBoolR => check_e_lt_bool_r(derivation),
        EvalML1ErrDerivationRule::ELtErrorL => check_e_lt_error_l(derivation),
        EvalML1ErrDerivationRule::ELtErrorR => check_e_lt_error_r(derivation),
        EvalML1ErrDerivationRule::BPlus => check_b_plus(derivation),
        EvalML1ErrDerivationRule::BMinus => check_b_minus(derivation),
        EvalML1ErrDerivationRule::BTimes => check_b_times(derivation),
        EvalML1ErrDerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalML1ErrDerivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalML1ErrDerivation,
    detail: String,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-IfT, E-IfF, E-IfInt, E-IfTError, E-IfFError, E-Plus, E-PlusBoolL, E-PlusBoolR, E-PlusErrorL, E-PlusErrorR, E-Minus, E-MinusBoolL, E-MinusBoolR, E-MinusErrorL, E-MinusErrorR, E-Times, E-TimesBoolL, E-TimesBoolR, E-TimesErrorL, E-TimesErrorR, E-Lt, E-LtBoolL, E-LtBoolR, E-LtErrorL, E-LtErrorR, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalML1ErrDerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalML1ErrDerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalML1ErrDerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalML1ErrJudgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalML1ErrDerivationRule,
    expected: &[EvalML1ErrJudgment],
    actual: &[EvalML1ErrJudgment],
    fix: &'static str,
) -> String {
    let expected_text = expected
        .iter()
        .map(|judgment| format!("[{judgment}]"))
        .collect::<Vec<_>>()
        .join(", ");
    let actual_text = actual
        .iter()
        .map(|judgment| format!("[{judgment}]"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "Wrong rule application: {} (expected premises: {expected_text}, actual premises: {actual_text}; fix: {fix})",
        rule.name()
    )
}

fn check_e_int(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EInt;
    match &derivation.judgment {
        EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::Int(expr_int),
            value: EvalML1ErrValue::Int(value_int),
        } if expr_int == value_int => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i evalto i"),
        ),
    }
}

fn check_e_bool(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EBool;
    match &derivation.judgment {
        EvalML1ErrJudgment::EvalTo {
            expr: EvalML1ErrExpr::Bool(expr_bool),
            value: EvalML1ErrValue::Bool(value_bool),
        } if expr_bool == value_bool => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "b evalto b"),
        ),
    }
}

fn check_e_if_t(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EIfT;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::If {
                condition,
                then_branch,
                else_branch: _,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_expr, first_value)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto true", &first),
                ));
            };
            let Some((second_expr, second_value)) = as_eval_to_non_error(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e2 evalto v", &second),
                ));
            };

            if first_expr == condition.as_ref()
                && first_value
                && second_expr == then_branch.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1ErrValue::Bool(true),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: then_branch.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "make e1/e2/v links consistent with the conclusion and enforce true in the first premise",
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

fn check_e_if_f(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EIfF;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::If {
                condition,
                then_branch: _,
                else_branch,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_expr, first_value)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto false", &first),
                ));
            };
            let Some((second_expr, second_value)) = as_eval_to_non_error(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e3 evalto v", &second),
                ));
            };

            if first_expr == condition.as_ref()
                && !first_value
                && second_expr == else_branch.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1ErrValue::Bool(false),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: else_branch.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "make e1/e3/v links consistent with the conclusion and enforce false in the first premise",
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

fn check_e_if_int(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EIfInt;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::If {
                condition,
                then_branch: _,
                else_branch: _,
            },
        value: EvalML1ErrValue::Error,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 evalto error"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_expr, first_int)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto i", &first),
                ));
            };

            if first_expr == condition.as_ref() {
                Ok(derivation.judgment.clone())
            } else {
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[EvalML1ErrJudgment::EvalTo {
                            expr: condition.as_ref().clone(),
                            value: EvalML1ErrValue::Int(first_int),
                        }],
                        &[first],
                        "make e1 consistent between conclusion and premise",
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

fn check_e_if_t_error(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EIfTError;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::If {
                condition,
                then_branch,
                else_branch: _,
            },
        value: EvalML1ErrValue::Error,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 evalto error"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let Some((first_expr, first_value)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto true", &first),
                ));
            };
            let Some(second_expr) = as_eval_to_error(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e2 evalto error", &second),
                ));
            };

            if first_expr == condition.as_ref()
                && first_value
                && second_expr == then_branch.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1ErrValue::Bool(true),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: then_branch.as_ref().clone(),
                    value: EvalML1ErrValue::Error,
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "make e1/e2 links consistent and enforce true + error premises",
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

fn check_e_if_f_error(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::EIfFError;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::If {
                condition,
                then_branch: _,
                else_branch,
            },
        value: EvalML1ErrValue::Error,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 evalto error"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let Some((first_expr, first_value)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto false", &first),
                ));
            };
            let Some(second_expr) = as_eval_to_error(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e3 evalto error", &second),
                ));
            };

            if first_expr == condition.as_ref()
                && !first_value
                && second_expr == else_branch.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1ErrValue::Bool(false),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: else_branch.as_ref().clone(),
                    value: EvalML1ErrValue::Error,
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "make e1/e3 links consistent and enforce false + error premises",
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

fn check_e_plus(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1ErrDerivationRule::EPlus,
        EvalML1ErrBinOp::Plus,
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1ErrDerivationRule::EMinus,
        EvalML1ErrBinOp::Minus,
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1ErrDerivationRule::ETimes,
        EvalML1ErrBinOp::Times,
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_plus_bool_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::EPlusBoolL,
        EvalML1ErrBinOp::Plus,
        Side::Left,
    )
}

fn check_e_plus_bool_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::EPlusBoolR,
        EvalML1ErrBinOp::Plus,
        Side::Right,
    )
}

fn check_e_plus_error_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::EPlusErrorL,
        EvalML1ErrBinOp::Plus,
        Side::Left,
    )
}

fn check_e_plus_error_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::EPlusErrorR,
        EvalML1ErrBinOp::Plus,
        Side::Right,
    )
}

fn check_e_minus_bool_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::EMinusBoolL,
        EvalML1ErrBinOp::Minus,
        Side::Left,
    )
}

fn check_e_minus_bool_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::EMinusBoolR,
        EvalML1ErrBinOp::Minus,
        Side::Right,
    )
}

fn check_e_minus_error_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::EMinusErrorL,
        EvalML1ErrBinOp::Minus,
        Side::Left,
    )
}

fn check_e_minus_error_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::EMinusErrorR,
        EvalML1ErrBinOp::Minus,
        Side::Right,
    )
}

fn check_e_times_bool_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::ETimesBoolL,
        EvalML1ErrBinOp::Times,
        Side::Left,
    )
}

fn check_e_times_bool_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::ETimesBoolR,
        EvalML1ErrBinOp::Times,
        Side::Right,
    )
}

fn check_e_times_error_l(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::ETimesErrorL,
        EvalML1ErrBinOp::Times,
        Side::Left,
    )
}

fn check_e_times_error_r(
    derivation: &EvalML1ErrDerivation,
) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::ETimesErrorR,
        EvalML1ErrBinOp::Times,
        Side::Right,
    )
}

fn check_e_lt_bool_l(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::ELtBoolL,
        EvalML1ErrBinOp::Lt,
        Side::Left,
    )
}

fn check_e_lt_bool_r(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_bool_side(
        derivation,
        EvalML1ErrDerivationRule::ELtBoolR,
        EvalML1ErrBinOp::Lt,
        Side::Right,
    )
}

fn check_e_lt_error_l(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::ELtErrorL,
        EvalML1ErrBinOp::Lt,
        Side::Left,
    )
}

fn check_e_lt_error_r(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    check_e_binop_error_side(
        derivation,
        EvalML1ErrDerivationRule::ELtErrorR,
        EvalML1ErrBinOp::Lt,
        Side::Right,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Left,
    Right,
}

fn check_e_binop_bool_side(
    derivation: &EvalML1ErrDerivation,
    rule: EvalML1ErrDerivationRule,
    op: EvalML1ErrBinOp,
    side: Side,
) -> Result<EvalML1ErrJudgment, CheckError> {
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML1ErrValue::Error,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "e1 op e2 evalto error (operator/type error premise required)",
            ),
        );
    };

    if *expr_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "e1 op e2 evalto error (operator/type error premise required)",
            ),
        );
    }

    match side {
        Side::Left => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((first_expr, _)) = as_eval_to_bool(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 evalto b", &first),
                    ));
                };
                if first_expr == left.as_ref() {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[EvalML1ErrJudgment::EvalTo {
                                expr: left.as_ref().clone(),
                                value: EvalML1ErrValue::Bool(true),
                            }],
                            &[first],
                            "make e1 consistent between conclusion and premise",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
        Side::Right => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some((first_expr, _)) = as_eval_to_bool(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e2 evalto b", &first),
                    ));
                };

                if first_expr == right.as_ref() {
                    Ok(derivation.judgment.clone())
                } else {
                    let expected = EvalML1ErrJudgment::EvalTo {
                        expr: right.as_ref().clone(),
                        value: EvalML1ErrValue::Bool(true),
                    };
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[expected],
                            &[first],
                            "make e2 consistent between conclusion and premise",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
    }
}

fn check_e_binop_error_side(
    derivation: &EvalML1ErrDerivation,
    rule: EvalML1ErrDerivationRule,
    op: EvalML1ErrBinOp,
    side: Side,
) -> Result<EvalML1ErrJudgment, CheckError> {
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML1ErrValue::Error,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "e1 op e2 evalto error (error-propagation premise required)",
            ),
        );
    };

    if *expr_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "e1 op e2 evalto error (error-propagation premise required)",
            ),
        );
    }

    match side {
        Side::Left => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some(first_expr) = as_eval_to_error(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e1 evalto error", &first),
                    ));
                };
                if first_expr == left.as_ref() {
                    Ok(derivation.judgment.clone())
                } else {
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[EvalML1ErrJudgment::EvalTo {
                                expr: left.as_ref().clone(),
                                value: EvalML1ErrValue::Error,
                            }],
                            &[first],
                            "make e1 consistent between conclusion and premise",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
        Side::Right => match derivation.subderivations.as_slice() {
            [d1] => {
                let first = infer_judgment(d1)?;
                let Some(first_expr) = as_eval_to_error(&first) else {
                    return Err(rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "e2 evalto error", &first),
                    ));
                };
                if first_expr == right.as_ref() {
                    Ok(derivation.judgment.clone())
                } else {
                    let expected = EvalML1ErrJudgment::EvalTo {
                        expr: right.as_ref().clone(),
                        value: EvalML1ErrValue::Error,
                    };
                    Err(rule_violation(
                        derivation,
                        wrong_rule_application_message(
                            rule,
                            &[expected],
                            &[first],
                            "make e2 consistent between conclusion and premise",
                        ),
                    ))
                }
            }
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
            ),
        },
    }
}

fn check_e_arith_binop(
    derivation: &EvalML1ErrDerivation,
    rule: EvalML1ErrDerivationRule,
    op: EvalML1ErrBinOp,
    as_arith_judgment: fn(&EvalML1ErrJudgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalML1ErrJudgment, CheckError> {
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML1ErrValue::Int(result),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 op e2 evalto i3"),
        );
    };

    if *expr_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 op e2 evalto i3"),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_expr, first_int)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto i1", &first),
                ));
            };
            let Some((second_expr, second_int)) = as_eval_to_int(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e2 evalto i2", &second),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_arith_judgment(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", third_shape, &third),
                ));
            };

            if first_expr == left.as_ref()
                && second_expr == right.as_ref()
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: left.as_ref().clone(),
                    value: EvalML1ErrValue::Int(third_left),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: right.as_ref().clone(),
                    value: EvalML1ErrValue::Int(third_right),
                };
                let expected_third = match op {
                    EvalML1ErrBinOp::Plus => EvalML1ErrJudgment::PlusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1ErrBinOp::Minus => EvalML1ErrJudgment::MinusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1ErrBinOp::Times => EvalML1ErrJudgment::TimesIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1ErrBinOp::Lt => unreachable!("lt is not arithmetic"),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "make e1/e2/i1/i2/i3 links consistent across conclusion and all premises",
                    ),
                ))
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_e_lt(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::ELt;
    let EvalML1ErrJudgment::EvalTo {
        expr:
            EvalML1ErrExpr::BinOp {
                op: EvalML1ErrBinOp::Lt,
                left,
                right,
            },
        value: EvalML1ErrValue::Bool(result),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 < e2 evalto b3"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_expr, first_int)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e1 evalto i1", &first),
                ));
            };
            let Some((second_expr, second_int)) = as_eval_to_int(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "e2 evalto i2", &second),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_less_than_is(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", "i1 less than i2 is b3", &third),
                ));
            };

            if first_expr == left.as_ref()
                && second_expr == right.as_ref()
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML1ErrJudgment::EvalTo {
                    expr: left.as_ref().clone(),
                    value: EvalML1ErrValue::Int(third_left),
                };
                let expected_second = EvalML1ErrJudgment::EvalTo {
                    expr: right.as_ref().clone(),
                    value: EvalML1ErrValue::Int(third_right),
                };
                let expected_third = EvalML1ErrJudgment::LessThanIs {
                    left: third_left,
                    right: third_right,
                    result: *result,
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "make e1/e2/i1/i2/b3 links consistent across conclusion and all premises",
                    ),
                ))
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_b_plus(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::BPlus;
    match &derivation.judgment {
        EvalML1ErrJudgment::PlusIs {
            left,
            right,
            result,
        } if *left + *right == *result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 plus i2 is i3 (where i3 = i1 + i2)"),
        ),
    }
}

fn check_b_minus(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::BMinus;
    match &derivation.judgment {
        EvalML1ErrJudgment::MinusIs {
            left,
            right,
            result,
        } if *left - *right == *result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 minus i2 is i3 (where i3 = i1 - i2)"),
        ),
    }
}

fn check_b_times(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::BTimes;
    match &derivation.judgment {
        EvalML1ErrJudgment::TimesIs {
            left,
            right,
            result,
        } if *left * *right == *result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 times i2 is i3 (where i3 = i1 * i2)"),
        ),
    }
}

fn check_b_lt(derivation: &EvalML1ErrDerivation) -> Result<EvalML1ErrJudgment, CheckError> {
    let rule = EvalML1ErrDerivationRule::BLt;
    match &derivation.judgment {
        EvalML1ErrJudgment::LessThanIs {
            left,
            right,
            result,
        } if (*left < *right) == *result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 less than i2 is b3 (where b3 = (i1 < i2))"),
        ),
    }
}

fn as_eval_to(judgment: &EvalML1ErrJudgment) -> Option<(&EvalML1ErrExpr, &EvalML1ErrValue)> {
    let EvalML1ErrJudgment::EvalTo { expr, value } = judgment else {
        return None;
    };
    Some((expr, value))
}

fn as_eval_to_non_error(
    judgment: &EvalML1ErrJudgment,
) -> Option<(&EvalML1ErrExpr, &EvalML1ErrValue)> {
    let (expr, value) = as_eval_to(judgment)?;
    if matches!(value, EvalML1ErrValue::Error) {
        return None;
    }
    Some((expr, value))
}

fn as_eval_to_int(judgment: &EvalML1ErrJudgment) -> Option<(&EvalML1ErrExpr, i64)> {
    let (expr, value) = as_eval_to(judgment)?;
    let EvalML1ErrValue::Int(value_int) = value else {
        return None;
    };
    Some((expr, *value_int))
}

fn as_eval_to_bool(judgment: &EvalML1ErrJudgment) -> Option<(&EvalML1ErrExpr, bool)> {
    let (expr, value) = as_eval_to(judgment)?;
    let EvalML1ErrValue::Bool(value_bool) = value else {
        return None;
    };
    Some((expr, *value_bool))
}

fn as_eval_to_error(judgment: &EvalML1ErrJudgment) -> Option<&EvalML1ErrExpr> {
    let (expr, value) = as_eval_to(judgment)?;
    if matches!(value, EvalML1ErrValue::Error) {
        Some(expr)
    } else {
        None
    }
}

fn as_plus_is(judgment: &EvalML1ErrJudgment) -> Option<(i64, i64, i64)> {
    let EvalML1ErrJudgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalML1ErrJudgment) -> Option<(i64, i64, i64)> {
    let EvalML1ErrJudgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalML1ErrJudgment) -> Option<(i64, i64, i64)> {
    let EvalML1ErrJudgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalML1ErrJudgment) -> Option<(i64, i64, bool)> {
    let EvalML1ErrJudgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn rule_violation(derivation: &EvalML1ErrDerivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalML1ErrGame;

    #[test]
    fn reports_root_judgment_text_for_all_eval_ml1_err_fixtures() {
        let game = EvalML1ErrGame;
        for (source, expected_summary) in [
            (
                include_str!("../../../copl/031.copl"),
                "1 + true + 2 evalto error",
            ),
            (
                include_str!("../../../copl/032.copl"),
                "if 2 + 3 then 1 else 3 evalto error",
            ),
            (
                include_str!("../../../copl/033.copl"),
                "if 3 < 4 then 1 < true else 3 - false evalto error",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "3 evalto 3 by E-Int { 3 evalto 3 by E-Int {} }";
        let err = EvalML1ErrGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Int"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        assert!(err.message().contains("premise path: root"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_e_int_conclusion() {
        let source = "3 evalto 4 by E-Int {}";
        let err = EvalML1ErrGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The form of conclusion is wrong: E-Int"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_e_plus_premises() {
        let source = r#"
3 + 5 evalto 9 by E-Plus {
  3 evalto 3 by E-Int {};
  5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {}
}
"#;
        let err = EvalML1ErrGame.check(source).expect_err("check should fail");
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
        let source = "3 evalto 3 by E-Unknown {}";
        let err = EvalML1ErrGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: E-Int, E-Bool, E-IfT, E-IfF"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
3 + 5 evalto 8 by E-Plus {
  3 evalto 3 by E-Unknown {};
  5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {}
}
"#;
        let err = EvalML1ErrGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
