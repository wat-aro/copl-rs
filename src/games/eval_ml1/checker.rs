use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{EvalML1BinOp, EvalML1Derivation, EvalML1Expr, EvalML1Judgment, EvalML1Value};

#[derive(Debug, Clone, Copy)]
enum EvalML1DerivationRule {
    EInt,
    EBool,
    EIfT,
    EIfF,
    EPlus,
    EMinus,
    ETimes,
    ELt,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalML1DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-IfT" => Some(Self::EIfT),
            "E-IfF" => Some(Self::EIfF),
            "E-Plus" => Some(Self::EPlus),
            "E-Minus" => Some(Self::EMinus),
            "E-Times" => Some(Self::ETimes),
            "E-Lt" => Some(Self::ELt),
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
            Self::EPlus => "E-Plus",
            Self::EMinus => "E-Minus",
            Self::ETimes => "E-Times",
            Self::ELt => "E-Lt",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalML1Game;

impl Game for EvalML1Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalML1
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

fn infer_judgment(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let Some(rule) = EvalML1DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalML1Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalML1Derivation,
    rule: EvalML1DerivationRule,
) -> Result<EvalML1Judgment, CheckError> {
    match rule {
        EvalML1DerivationRule::EInt => check_e_int(derivation),
        EvalML1DerivationRule::EBool => check_e_bool(derivation),
        EvalML1DerivationRule::EIfT => check_e_if_t(derivation),
        EvalML1DerivationRule::EIfF => check_e_if_f(derivation),
        EvalML1DerivationRule::EPlus => check_e_plus(derivation),
        EvalML1DerivationRule::EMinus => check_e_minus(derivation),
        EvalML1DerivationRule::ETimes => check_e_times(derivation),
        EvalML1DerivationRule::ELt => check_e_lt(derivation),
        EvalML1DerivationRule::BPlus => check_b_plus(derivation),
        EvalML1DerivationRule::BMinus => check_b_minus(derivation),
        EvalML1DerivationRule::BTimes => check_b_times(derivation),
        EvalML1DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalML1Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalML1Derivation,
    detail: String,
) -> Result<EvalML1Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-IfT, E-IfF, E-Plus, E-Minus, E-Times, E-Lt, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalML1DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalML1DerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalML1DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalML1Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalML1DerivationRule,
    expected: &[EvalML1Judgment],
    actual: &[EvalML1Judgment],
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

fn check_e_int(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::EInt;
    match &derivation.judgment {
        EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::Int(expr_int),
            value: EvalML1Value::Int(value_int),
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

fn check_e_bool(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::EBool;
    match &derivation.judgment {
        EvalML1Judgment::EvalTo {
            expr: EvalML1Expr::Bool(expr_bool),
            value: EvalML1Value::Bool(value_bool),
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

fn check_e_if_t(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::EIfT;
    let EvalML1Judgment::EvalTo {
        expr:
            EvalML1Expr::If {
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
            let Some((second_expr, second_value)) = as_eval_to(&second) else {
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
                let expected_first = EvalML1Judgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1Value::Bool(true),
                };
                let expected_second = EvalML1Judgment::EvalTo {
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

fn check_e_if_f(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::EIfF;
    let EvalML1Judgment::EvalTo {
        expr:
            EvalML1Expr::If {
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
            let Some((second_expr, second_value)) = as_eval_to(&second) else {
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
                let expected_first = EvalML1Judgment::EvalTo {
                    expr: condition.as_ref().clone(),
                    value: EvalML1Value::Bool(false),
                };
                let expected_second = EvalML1Judgment::EvalTo {
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

fn check_e_plus(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1DerivationRule::EPlus,
        EvalML1BinOp::Plus,
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1DerivationRule::EMinus,
        EvalML1BinOp::Minus,
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML1DerivationRule::ETimes,
        EvalML1BinOp::Times,
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_arith_binop(
    derivation: &EvalML1Derivation,
    rule: EvalML1DerivationRule,
    op: EvalML1BinOp,
    as_arith_judgment: fn(&EvalML1Judgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalML1Judgment, CheckError> {
    let EvalML1Judgment::EvalTo {
        expr:
            EvalML1Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML1Value::Int(result),
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
                let expected_first = EvalML1Judgment::EvalTo {
                    expr: left.as_ref().clone(),
                    value: EvalML1Value::Int(third_left),
                };
                let expected_second = EvalML1Judgment::EvalTo {
                    expr: right.as_ref().clone(),
                    value: EvalML1Value::Int(third_right),
                };
                let expected_third = match op {
                    EvalML1BinOp::Plus => EvalML1Judgment::PlusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1BinOp::Minus => EvalML1Judgment::MinusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1BinOp::Times => EvalML1Judgment::TimesIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML1BinOp::Lt => unreachable!("lt is not arithmetic"),
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

fn check_e_lt(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::ELt;
    let EvalML1Judgment::EvalTo {
        expr:
            EvalML1Expr::BinOp {
                op: EvalML1BinOp::Lt,
                left,
                right,
            },
        value: EvalML1Value::Bool(result),
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
                let expected_first = EvalML1Judgment::EvalTo {
                    expr: left.as_ref().clone(),
                    value: EvalML1Value::Int(third_left),
                };
                let expected_second = EvalML1Judgment::EvalTo {
                    expr: right.as_ref().clone(),
                    value: EvalML1Value::Int(third_right),
                };
                let expected_third = EvalML1Judgment::LessThanIs {
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

fn check_b_plus(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::BPlus;
    match &derivation.judgment {
        EvalML1Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::BMinus;
    match &derivation.judgment {
        EvalML1Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::BTimes;
    match &derivation.judgment {
        EvalML1Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalML1Derivation) -> Result<EvalML1Judgment, CheckError> {
    let rule = EvalML1DerivationRule::BLt;
    match &derivation.judgment {
        EvalML1Judgment::LessThanIs {
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

fn as_eval_to(judgment: &EvalML1Judgment) -> Option<(&EvalML1Expr, &EvalML1Value)> {
    let EvalML1Judgment::EvalTo { expr, value } = judgment else {
        return None;
    };
    Some((expr, value))
}

fn as_eval_to_int(judgment: &EvalML1Judgment) -> Option<(&EvalML1Expr, i64)> {
    let (expr, value) = as_eval_to(judgment)?;
    let EvalML1Value::Int(value_int) = value else {
        return None;
    };
    Some((expr, *value_int))
}

fn as_eval_to_bool(judgment: &EvalML1Judgment) -> Option<(&EvalML1Expr, bool)> {
    let (expr, value) = as_eval_to(judgment)?;
    let EvalML1Value::Bool(value_bool) = value else {
        return None;
    };
    Some((expr, *value_bool))
}

fn as_plus_is(judgment: &EvalML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalML1Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalML1Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalML1Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalML1Judgment) -> Option<(i64, i64, bool)> {
    let EvalML1Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn rule_violation(derivation: &EvalML1Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalML1Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_ml1_fixtures() {
        let game = EvalML1Game;
        for (source, expected_summary) in [
            (include_str!("../../../copl/025.copl"), "3 + 5 evalto 8"),
            (include_str!("../../../copl/026.copl"), "8 - 2 - 3 evalto 3"),
            (
                include_str!("../../../copl/027.copl"),
                "(4 + 5) * (1 - 10) evalto -81",
            ),
            (
                include_str!("../../../copl/028.copl"),
                "if 4 < 5 then 2 + 3 else 8 * 8 evalto 5",
            ),
            (
                include_str!("../../../copl/029.copl"),
                "3 + if -23 < -2 * 8 then 8 else 2 + 4 evalto 11",
            ),
            (
                include_str!("../../../copl/030.copl"),
                "3 + (if -23 < -2 * 8 then 8 else 2) + 4 evalto 15",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "3 evalto 3 by E-Int { 3 evalto 3 by E-Int {} }";
        let err = EvalML1Game.check(source).expect_err("check should fail");
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
        let err = EvalML1Game.check(source).expect_err("check should fail");
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
        let err = EvalML1Game.check(source).expect_err("check should fail");
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
        let err = EvalML1Game.check(source).expect_err("check should fail");
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
        let err = EvalML1Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
