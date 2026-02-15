use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalContML1BinOp, EvalContML1ContFrame, EvalContML1Continuation, EvalContML1Derivation,
    EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalContML1DerivationRule {
    EInt,
    EBool,
    EBinOp,
    EIf,
    CRet,
    CEvalR,
    CPlus,
    CMinus,
    CTimes,
    CLt,
    CIfT,
    CIfF,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalContML1DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-BinOp" => Some(Self::EBinOp),
            "E-If" => Some(Self::EIf),
            "C-Ret" => Some(Self::CRet),
            "C-EvalR" => Some(Self::CEvalR),
            "C-Plus" => Some(Self::CPlus),
            "C-Minus" => Some(Self::CMinus),
            "C-Times" => Some(Self::CTimes),
            "C-Lt" => Some(Self::CLt),
            "C-IfT" => Some(Self::CIfT),
            "C-IfF" => Some(Self::CIfF),
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
            Self::EBinOp => "E-BinOp",
            Self::EIf => "E-If",
            Self::CRet => "C-Ret",
            Self::CEvalR => "C-EvalR",
            Self::CPlus => "C-Plus",
            Self::CMinus => "C-Minus",
            Self::CTimes => "C-Times",
            Self::CLt => "C-Lt",
            Self::CIfT => "C-IfT",
            Self::CIfF => "C-IfF",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalContML1Game;

impl Game for EvalContML1Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalContML1
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

fn infer_judgment(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalContML1Derivation,
) -> Result<EvalContML1Judgment, CheckError> {
    let Some(rule) = EvalContML1DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalContML1Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalContML1Derivation,
    rule: EvalContML1DerivationRule,
) -> Result<EvalContML1Judgment, CheckError> {
    match rule {
        EvalContML1DerivationRule::EInt => check_e_int(derivation),
        EvalContML1DerivationRule::EBool => check_e_bool(derivation),
        EvalContML1DerivationRule::EBinOp => check_e_binop(derivation),
        EvalContML1DerivationRule::EIf => check_e_if(derivation),
        EvalContML1DerivationRule::CRet => check_c_ret(derivation),
        EvalContML1DerivationRule::CEvalR => check_c_eval_r(derivation),
        EvalContML1DerivationRule::CPlus => check_c_plus(derivation),
        EvalContML1DerivationRule::CMinus => check_c_minus(derivation),
        EvalContML1DerivationRule::CTimes => check_c_times(derivation),
        EvalContML1DerivationRule::CLt => check_c_lt(derivation),
        EvalContML1DerivationRule::CIfT => check_c_if_t(derivation),
        EvalContML1DerivationRule::CIfF => check_c_if_f(derivation),
        EvalContML1DerivationRule::BPlus => check_b_plus(derivation),
        EvalContML1DerivationRule::BMinus => check_b_minus(derivation),
        EvalContML1DerivationRule::BTimes => check_b_times(derivation),
        EvalContML1DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalContML1Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalContML1Derivation,
    detail: String,
) -> Result<EvalContML1Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-BinOp, E-If, C-Ret, C-EvalR, C-Plus, C-Minus, C-Times, C-Lt, C-IfT, C-IfF, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalContML1DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: EvalContML1DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalContML1DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalContML1Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalContML1DerivationRule,
    expected: &[EvalContML1Judgment],
    actual: &[EvalContML1Judgment],
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

fn check_e_int(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::EInt;
    let Some((expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i >> k evalto v"),
        );
    };
    let EvalContML1Expr::Int(expr_int) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual_judgment = infer_judgment(d1)?;
            let Some((actual_input, actual_continuation, actual_value)) =
                as_cont_eval_to(&actual_judgment)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "i => k evalto v", &actual_judgment),
                ));
            };

            let expected_judgment = cont_eval_to_judgment(
                EvalContML1Value::Int(*expr_int),
                continuation.clone(),
                value.clone(),
            );
            if actual_input != &EvalContML1Value::Int(*expr_int)
                || !actual_continuation.semantic_eq(continuation)
                || actual_value != value
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        &[actual_judgment],
                        "ensure premise is exactly i => k evalto v",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_e_bool(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::EBool;
    let Some((expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "b >> k evalto v"),
        );
    };
    let EvalContML1Expr::Bool(expr_bool) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "b >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual_judgment = infer_judgment(d1)?;
            let Some((actual_input, actual_continuation, actual_value)) =
                as_cont_eval_to(&actual_judgment)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "b => k evalto v", &actual_judgment),
                ));
            };

            let expected_judgment = cont_eval_to_judgment(
                EvalContML1Value::Bool(*expr_bool),
                continuation.clone(),
                value.clone(),
            );
            if actual_input != &EvalContML1Value::Bool(*expr_bool)
                || !actual_continuation.semantic_eq(continuation)
                || actual_value != value
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        &[actual_judgment],
                        "ensure premise is exactly b => k evalto v",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_e_binop(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::EBinOp;
    let Some((expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 op e2 >> k evalto v"),
        );
    };
    let EvalContML1Expr::BinOp { op, left, right } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "e1 op e2 >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual_judgment = infer_judgment(d1)?;
            let Some((actual_expr, actual_continuation, actual_value)) =
                as_eval_to(&actual_judgment)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "e1 >> {_ op e2} >> k evalto v",
                        &actual_judgment,
                    ),
                ));
            };

            let expected_continuation = prepend_frame(
                EvalContML1ContFrame::EvalR {
                    op: *op,
                    right: right.as_ref().clone(),
                },
                continuation,
            );
            let expected_judgment = eval_to_judgment(
                left.as_ref().clone(),
                expected_continuation.clone(),
                value.clone(),
            );

            if actual_expr != left.as_ref()
                || !actual_continuation.semantic_eq(&expected_continuation)
                || actual_value != value
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        &[actual_judgment],
                        "ensure premise evaluates e1 with continuation {_ op e2} >> k",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_e_if(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::EIf;
    let Some((expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 >> k evalto v"),
        );
    };
    let EvalContML1Expr::If {
        condition,
        then_branch,
        else_branch,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "if e1 then e2 else e3 >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual_judgment = infer_judgment(d1)?;
            let Some((actual_expr, actual_continuation, actual_value)) =
                as_eval_to(&actual_judgment)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "e1 >> {if _ then e2 else e3} >> k evalto v",
                        &actual_judgment,
                    ),
                ));
            };

            let expected_continuation = prepend_frame(
                EvalContML1ContFrame::If {
                    then_branch: then_branch.as_ref().clone(),
                    else_branch: else_branch.as_ref().clone(),
                },
                continuation,
            );
            let expected_judgment = eval_to_judgment(
                condition.as_ref().clone(),
                expected_continuation.clone(),
                value.clone(),
            );

            if actual_expr != condition.as_ref()
                || !actual_continuation.semantic_eq(&expected_continuation)
                || actual_value != value
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        &[actual_judgment],
                        "ensure premise evaluates the condition with continuation {if _ then e2 else e3} >> k",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_c_ret(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::CRet;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => _ evalto v"),
        );
    };

    if !continuation.frames.is_empty() || input != value {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => _ evalto v"),
        );
    }

    match derivation.subderivations.as_slice() {
        [] => Ok(derivation.judgment.clone()),
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_c_eval_r(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::CEvalR;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {_ op e2} >> k evalto v"),
        );
    };

    let EvalContML1Value::Int(left_value) = input else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {_ op e2} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {_ op e2} >> k evalto v"),
        );
    };

    let EvalContML1ContFrame::EvalR { op, right } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {_ op e2} >> k evalto v"),
        );
    };

    let next_frame = match op {
        EvalContML1BinOp::Plus => EvalContML1ContFrame::Plus { left: *left_value },
        EvalContML1BinOp::Minus => EvalContML1ContFrame::Minus { left: *left_value },
        EvalContML1BinOp::Times => EvalContML1ContFrame::Times { left: *left_value },
        EvalContML1BinOp::Lt => EvalContML1ContFrame::Lt { left: *left_value },
    };
    let expected_continuation = prepend_frame(next_frame, &tail);

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual_judgment = infer_judgment(d1)?;
            let Some((actual_expr, actual_continuation, actual_value)) =
                as_eval_to(&actual_judgment)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "e2 >> {i1 op _} >> k evalto v",
                        &actual_judgment,
                    ),
                ));
            };

            let expected_judgment =
                eval_to_judgment(right.clone(), expected_continuation.clone(), value.clone());
            if actual_expr != right
                || !actual_continuation.semantic_eq(&expected_continuation)
                || actual_value != value
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        &[actual_judgment],
                        "ensure premise evaluates e2 with continuation {i1 op _} >> k",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_c_plus(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML1DerivationRule::CPlus)
}

fn check_c_minus(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML1DerivationRule::CMinus)
}

fn check_c_times(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML1DerivationRule::CTimes)
}

fn check_c_lt(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::CLt;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };

    let EvalContML1Value::Int(right_value) = input else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };

    let EvalContML1ContFrame::Lt { left } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            if as_less_than_is(&first).is_none() {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "i1 less than i2 is b", &first),
                ));
            }
            if as_cont_eval_to(&second).is_none() {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "b => k evalto v", &second),
                ));
            }

            let bool_result = *left < *right_value;
            let expected_first = EvalContML1Judgment::LessThanIs {
                left: *left,
                right: *right_value,
                result: bool_result,
            };
            let expected_second = cont_eval_to_judgment(
                EvalContML1Value::Bool(bool_result),
                tail.clone(),
                value.clone(),
            );

            if !judgment_semantic_eq(&first, &expected_first)
                || !judgment_semantic_eq(&second, &expected_second)
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "ensure arithmetic result and continuation handoff are consistent",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        ),
    }
}

fn check_c_if_t(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::CIfT;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "true => {if _ then e2 else e3} >> k evalto v"),
        );
    };

    if input != &EvalContML1Value::Bool(true) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "true => {if _ then e2 else e3} >> k evalto v"),
        );
    }

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "true => {if _ then e2 else e3} >> k evalto v"),
        );
    };
    let EvalContML1ContFrame::If { then_branch, .. } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "true => {if _ then e2 else e3} >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual = infer_judgment(d1)?;
            if as_eval_to(&actual).is_none() {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e2 >> k evalto v", &actual),
                ));
            }

            let expected = eval_to_judgment(then_branch.clone(), tail.clone(), value.clone());
            if !judgment_semantic_eq(&actual, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[actual],
                        "evaluate the then-branch with continuation k",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_c_if_f(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::CIfF;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "false => {if _ then e2 else e3} >> k evalto v"),
        );
    };

    if input != &EvalContML1Value::Bool(false) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "false => {if _ then e2 else e3} >> k evalto v"),
        );
    }

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "false => {if _ then e2 else e3} >> k evalto v"),
        );
    };
    let EvalContML1ContFrame::If { else_branch, .. } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "false => {if _ then e2 else e3} >> k evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let actual = infer_judgment(d1)?;
            if as_eval_to(&actual).is_none() {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "e3 >> k evalto v", &actual),
                ));
            }

            let expected = eval_to_judgment(else_branch.clone(), tail.clone(), value.clone());
            if !judgment_semantic_eq(&actual, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[actual],
                        "evaluate the else-branch with continuation k",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_c_binop_int(
    derivation: &EvalContML1Derivation,
    rule: EvalContML1DerivationRule,
) -> Result<EvalContML1Judgment, CheckError> {
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
        );
    };

    let EvalContML1Value::Int(right_value) = input else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
        );
    };

    let (left_value, make_base_judgment, result_value) = match (rule, head) {
        (EvalContML1DerivationRule::CPlus, EvalContML1ContFrame::Plus { left }) => (
            *left,
            EvalContML1Judgment::PlusIs {
                left: *left,
                right: *right_value,
                result: *left + *right_value,
            },
            EvalContML1Value::Int(*left + *right_value),
        ),
        (EvalContML1DerivationRule::CMinus, EvalContML1ContFrame::Minus { left }) => (
            *left,
            EvalContML1Judgment::MinusIs {
                left: *left,
                right: *right_value,
                result: *left - *right_value,
            },
            EvalContML1Value::Int(*left - *right_value),
        ),
        (EvalContML1DerivationRule::CTimes, EvalContML1ContFrame::Times { left }) => (
            *left,
            EvalContML1Judgment::TimesIs {
                left: *left,
                right: *right_value,
                result: *left * *right_value,
            },
            EvalContML1Value::Int(*left * *right_value),
        ),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
            )
        }
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            if !(matches!(rule, EvalContML1DerivationRule::CPlus) && as_plus_is(&first).is_some()
                || matches!(rule, EvalContML1DerivationRule::CMinus)
                    && as_minus_is(&first).is_some()
                || matches!(rule, EvalContML1DerivationRule::CTimes)
                    && as_times_is(&first).is_some())
            {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "i1 op i2 is i3", &first),
                ));
            }
            if as_cont_eval_to(&second).is_none() {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "i3 => k evalto v", &second),
                ));
            }

            let expected_first = make_base_judgment;
            let expected_second =
                cont_eval_to_judgment(result_value.clone(), tail.clone(), value.clone());

            if !judgment_semantic_eq(&first, &expected_first)
                || !judgment_semantic_eq(&second, &expected_second)
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "ensure arithmetic result and continuation handoff are consistent",
                    ),
                ));
            }

            let _ = left_value;
            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        ),
    }
}

fn check_b_plus(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::BPlus;
    match &derivation.judgment {
        EvalContML1Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::BMinus;
    match &derivation.judgment {
        EvalContML1Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::BTimes;
    match &derivation.judgment {
        EvalContML1Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalContML1Derivation) -> Result<EvalContML1Judgment, CheckError> {
    let rule = EvalContML1DerivationRule::BLt;
    match &derivation.judgment {
        EvalContML1Judgment::LessThanIs {
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

fn split_continuation(
    continuation: &EvalContML1Continuation,
) -> Option<(&EvalContML1ContFrame, EvalContML1Continuation)> {
    let head = continuation.frames.first()?;
    let tail = EvalContML1Continuation {
        frames: continuation.frames.iter().skip(1).cloned().collect(),
        explicit_ret: continuation.explicit_ret,
    };
    Some((head, tail))
}

fn prepend_frame(
    frame: EvalContML1ContFrame,
    continuation: &EvalContML1Continuation,
) -> EvalContML1Continuation {
    let mut frames = vec![frame];
    frames.extend(continuation.frames.iter().cloned());
    EvalContML1Continuation {
        frames,
        explicit_ret: continuation.explicit_ret,
    }
}

fn eval_to_judgment(
    expr: EvalContML1Expr,
    continuation: EvalContML1Continuation,
    value: EvalContML1Value,
) -> EvalContML1Judgment {
    let has_continuation = !continuation.frames.is_empty() || continuation.explicit_ret;
    EvalContML1Judgment::EvalTo {
        expr,
        continuation,
        value,
        has_continuation,
    }
}

fn cont_eval_to_judgment(
    input: EvalContML1Value,
    continuation: EvalContML1Continuation,
    value: EvalContML1Value,
) -> EvalContML1Judgment {
    EvalContML1Judgment::ContEvalTo {
        input,
        continuation,
        value,
    }
}

fn judgment_semantic_eq(left: &EvalContML1Judgment, right: &EvalContML1Judgment) -> bool {
    match (left, right) {
        (
            EvalContML1Judgment::EvalTo {
                expr: left_expr,
                continuation: left_cont,
                value: left_value,
                ..
            },
            EvalContML1Judgment::EvalTo {
                expr: right_expr,
                continuation: right_cont,
                value: right_value,
                ..
            },
        ) => {
            left_expr == right_expr
                && left_value == right_value
                && left_cont.semantic_eq(right_cont)
        }
        (
            EvalContML1Judgment::ContEvalTo {
                input: left_input,
                continuation: left_cont,
                value: left_value,
            },
            EvalContML1Judgment::ContEvalTo {
                input: right_input,
                continuation: right_cont,
                value: right_value,
            },
        ) => {
            left_input == right_input
                && left_value == right_value
                && left_cont.semantic_eq(right_cont)
        }
        _ => left == right,
    }
}

fn as_eval_to(
    judgment: &EvalContML1Judgment,
) -> Option<(
    &EvalContML1Expr,
    &EvalContML1Continuation,
    &EvalContML1Value,
)> {
    let EvalContML1Judgment::EvalTo {
        expr,
        continuation,
        value,
        ..
    } = judgment
    else {
        return None;
    };
    Some((expr, continuation, value))
}

fn as_cont_eval_to(
    judgment: &EvalContML1Judgment,
) -> Option<(
    &EvalContML1Value,
    &EvalContML1Continuation,
    &EvalContML1Value,
)> {
    let EvalContML1Judgment::ContEvalTo {
        input,
        continuation,
        value,
    } = judgment
    else {
        return None;
    };
    Some((input, continuation, value))
}

fn as_plus_is(judgment: &EvalContML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalContML1Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalContML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalContML1Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalContML1Judgment) -> Option<(i64, i64, i64)> {
    let EvalContML1Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalContML1Judgment) -> Option<(i64, i64, bool)> {
    let EvalContML1Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn rule_violation(derivation: &EvalContML1Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalContML1Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_cont_ml1_fixtures() {
        let game = EvalContML1Game;
        for (source, expected_summary) in [
            (include_str!("../../../copl/124.copl"), "3 >> _ evalto 3"),
            (
                include_str!("../../../copl/125.copl"),
                "5 >> {3 + _} evalto 8",
            ),
            (include_str!("../../../copl/126.copl"), "3 + 5 evalto 8"),
            (
                include_str!("../../../copl/127.copl"),
                "(4 + 5) * (1 - 10) evalto -81",
            ),
            (
                include_str!("../../../copl/128.copl"),
                "if 4 < 5 then 2 + 3 else 8 * 8 evalto 5",
            ),
            (
                include_str!("../../../copl/129.copl"),
                "3 + (if -3 < -2 * 8 then 8 else 2) + 4 evalto 9",
            ),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert_eq!(report.summary, expected_summary);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "3 >> _ evalto 3 by E-Int {}";
        let err = EvalContML1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Int"));
        assert!(err.message().contains("expected: 1, actual: 0"));
        assert!(err.message().contains("premise path: root"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "3 >> _ evalto 3 by E-Unknown {}";
        let err = EvalContML1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: E-Int, E-Bool, E-BinOp, E-If"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
3 + 5 evalto 8 by E-BinOp {
  3 >> {_ + 5} evalto 8 by E-Unknown {}
}
"#;
        let err = EvalContML1Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
