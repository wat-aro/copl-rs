use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalNamelessML3BinOp, EvalNamelessML3Derivation, EvalNamelessML3Env, EvalNamelessML3Expr,
    EvalNamelessML3Judgment, EvalNamelessML3Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalNamelessML3DerivationRule {
    EInt,
    EBool,
    EVar,
    EIfT,
    EIfF,
    ELet,
    ELetRec,
    EFun,
    EApp,
    EAppRec,
    EPlus,
    EMinus,
    ETimes,
    ELt,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalNamelessML3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-Var" => Some(Self::EVar),
            "E-IfT" => Some(Self::EIfT),
            "E-IfF" => Some(Self::EIfF),
            "E-Let" => Some(Self::ELet),
            "E-LetRec" => Some(Self::ELetRec),
            "E-Fun" => Some(Self::EFun),
            "E-App" => Some(Self::EApp),
            "E-AppRec" => Some(Self::EAppRec),
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
            Self::EVar => "E-Var",
            Self::EIfT => "E-IfT",
            Self::EIfF => "E-IfF",
            Self::ELet => "E-Let",
            Self::ELetRec => "E-LetRec",
            Self::EFun => "E-Fun",
            Self::EApp => "E-App",
            Self::EAppRec => "E-AppRec",
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
pub struct EvalNamelessML3Game;

impl Game for EvalNamelessML3Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalNamelessML3
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

fn infer_judgment(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let Some(rule) = EvalNamelessML3DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalNamelessML3Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalNamelessML3Derivation,
    rule: EvalNamelessML3DerivationRule,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    match rule {
        EvalNamelessML3DerivationRule::EInt => check_e_int(derivation),
        EvalNamelessML3DerivationRule::EBool => check_e_bool(derivation),
        EvalNamelessML3DerivationRule::EVar => check_e_var(derivation),
        EvalNamelessML3DerivationRule::EIfT => check_e_if_t(derivation),
        EvalNamelessML3DerivationRule::EIfF => check_e_if_f(derivation),
        EvalNamelessML3DerivationRule::ELet => check_e_let(derivation),
        EvalNamelessML3DerivationRule::ELetRec => check_e_let_rec(derivation),
        EvalNamelessML3DerivationRule::EFun => check_e_fun(derivation),
        EvalNamelessML3DerivationRule::EApp => check_e_app(derivation),
        EvalNamelessML3DerivationRule::EAppRec => check_e_app_rec(derivation),
        EvalNamelessML3DerivationRule::EPlus => check_e_plus(derivation),
        EvalNamelessML3DerivationRule::EMinus => check_e_minus(derivation),
        EvalNamelessML3DerivationRule::ETimes => check_e_times(derivation),
        EvalNamelessML3DerivationRule::ELt => check_e_lt(derivation),
        EvalNamelessML3DerivationRule::BPlus => check_b_plus(derivation),
        EvalNamelessML3DerivationRule::BMinus => check_b_minus(derivation),
        EvalNamelessML3DerivationRule::BTimes => check_b_times(derivation),
        EvalNamelessML3DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(
    subderivations: &[EvalNamelessML3Derivation],
) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalNamelessML3Derivation,
    detail: String,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-Var, E-IfT, E-IfF, E-Let, E-LetRec, E-Fun, E-App, E-AppRec, E-Plus, E-Minus, E-Times, E-Lt, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalNamelessML3DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: EvalNamelessML3DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalNamelessML3DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalNamelessML3Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalNamelessML3DerivationRule,
    expected: &[EvalNamelessML3Judgment],
    actual: &[EvalNamelessML3Judgment],
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

fn check_e_int(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EInt;
    match &derivation.judgment {
        EvalNamelessML3Judgment::EvalTo {
            env: _,
            expr: EvalNamelessML3Expr::Int(expr_int),
            value: EvalNamelessML3Value::Int(value_int),
        } if expr_int == value_int => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- i evalto i"),
        ),
    }
}

fn check_e_bool(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EBool;
    match &derivation.judgment {
        EvalNamelessML3Judgment::EvalTo {
            env: _,
            expr: EvalNamelessML3Expr::Bool(expr_bool),
            value: EvalNamelessML3Value::Bool(value_bool),
        } if expr_bool == value_bool => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- b evalto b"),
        ),
    }
}

fn check_e_var(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EVar;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::Index(index),
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- #n evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            if *index >= 1 && *index <= env.0.len() {
                let resolved = &env.0[env.0.len() - index];
                if resolved == value {
                    return Ok(derivation.judgment.clone());
                }

                let expected = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: EvalNamelessML3Expr::Index(*index),
                    value: resolved.clone(),
                };
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        std::slice::from_ref(&derivation.judgment),
                        "resolve #n to the n-th value from the right in Gamma",
                    ),
                ));
            }

            fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "Gamma |- #n evalto v"),
            )
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_e_if_t(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EIfT;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr:
            EvalNamelessML3Expr::If {
                condition,
                then_branch,
                else_branch: _,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- if e1 then e2 else e3 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_expr, first_bool)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 evalto true", &first),
                ));
            };
            let Some((second_env, second_expr, second_value)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto v", &second),
                ));
            };

            if first_env == env
                && first_expr == condition.as_ref()
                && first_bool
                && second_env == env
                && second_expr == then_branch.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalNamelessML3Value::Bool(true),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: then_branch.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "evaluate condition to true, then evaluate the then-branch",
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

fn check_e_if_f(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EIfF;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr:
            EvalNamelessML3Expr::If {
                condition,
                then_branch: _,
                else_branch,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- if e1 then e2 else e3 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_expr, first_bool)) = as_eval_to_bool(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 evalto false", &first),
                ));
            };
            let Some((second_env, second_expr, second_value)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e3 evalto v", &second),
                ));
            };

            if first_env == env
                && first_expr == condition.as_ref()
                && !first_bool
                && second_env == env
                && second_expr == else_branch.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalNamelessML3Value::Bool(false),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: else_branch.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "evaluate condition to false, then evaluate the else-branch",
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

fn check_e_let(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::ELet;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::Let { bound_expr, body },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let . = e1 in e2 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 evalto v1", &first),
                ));
            };
            let Some((second_env, second_expr, second_value)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma, v1 |- e2 evalto v", &second),
                ));
            };

            let expected_second_env = push_value(env, first_value);
            if first_env == env
                && first_expr == bound_expr.as_ref()
                && second_env == &expected_second_env
                && second_expr == body.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: bound_expr.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: expected_second_env,
                    expr: body.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "evaluate e1 under Gamma, then evaluate e2 under Gamma extended with v1",
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

fn check_e_let_rec(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::ELetRec;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::LetRec { fun_body, body },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let rec . = fun . -> e1 in e2 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma, vr |- e2 evalto v", &first),
                ));
            };

            let recursive_value = EvalNamelessML3Value::RecClosure {
                env: env.clone(),
                body: fun_body.as_ref().clone(),
            };
            let expected_env = push_value(env, &recursive_value);
            if first_env == &expected_env && first_expr == body.as_ref() && first_value == value {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: expected_env,
                    expr: body.as_ref().clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "evaluate let-rec body under Gamma extended with the recursive closure",
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

fn check_e_fun(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EFun;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::Fun { body },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun . -> e evalto (Gamma)[fun . -> e]"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            let expected = EvalNamelessML3Value::Closure {
                env: env.clone(),
                body: body.as_ref().clone(),
            };
            if value == &expected {
                Ok(derivation.judgment.clone())
            } else {
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[EvalNamelessML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: EvalNamelessML3Expr::Fun {
                                body: Box::new(body.as_ref().clone()),
                            },
                            value: expected,
                        }],
                        std::slice::from_ref(&derivation.judgment),
                        "capture Gamma as closure environment",
                    ),
                ))
            }
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_e_app(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EApp;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::App { func, arg },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[fun . -> e0]",
                        &first,
                    ),
                ));
            };
            let EvalNamelessML3Value::Closure {
                env: closure_env,
                body,
            } = first_value
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[fun . -> e0]",
                        &first,
                    ),
                ));
            };
            let Some((second_env, second_expr, second_value)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto v2", &second),
                ));
            };
            let Some((third_env, third_expr, third_value)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", "Gamma', v2 |- e0 evalto v", &third),
                ));
            };

            let expected_third_env = push_value(closure_env, second_value);
            if first_env == env
                && first_expr == func.as_ref()
                && second_env == env
                && second_expr == arg.as_ref()
                && third_env == &expected_third_env
                && third_expr == body
                && third_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalNamelessML3Judgment::EvalTo {
                    env: expected_third_env,
                    expr: body.clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "evaluate e1/e2 under Gamma, then evaluate closure body under captured Gamma' extended with v2",
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

fn check_e_app_rec(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::EAppRec;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr: EvalNamelessML3Expr::App { func, arg },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[rec . = fun . -> e0]",
                        &first,
                    ),
                ));
            };
            let EvalNamelessML3Value::RecClosure {
                env: closure_env,
                body,
            } = first_value
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[rec . = fun . -> e0]",
                        &first,
                    ),
                ));
            };
            let Some((second_env, second_expr, second_value)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto v2", &second),
                ));
            };
            let Some((third_env, third_expr, third_value)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "Gamma', vr, v2 |- e0 evalto v",
                        &third,
                    ),
                ));
            };

            let recursive_value = first_value.clone();
            let expected_third_env =
                push_value(&push_value(closure_env, &recursive_value), second_value);

            if first_env == env
                && first_expr == func.as_ref()
                && second_env == env
                && second_expr == arg.as_ref()
                && third_env == &expected_third_env
                && third_expr == body
                && third_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalNamelessML3Judgment::EvalTo {
                    env: expected_third_env,
                    expr: body.clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "evaluate e1/e2 under Gamma, then evaluate recursive body under captured Gamma' extended with vr and v2",
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

fn check_e_plus(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalNamelessML3DerivationRule::EPlus,
        EvalNamelessML3BinOp::Plus,
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalNamelessML3DerivationRule::EMinus,
        EvalNamelessML3BinOp::Minus,
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalNamelessML3DerivationRule::ETimes,
        EvalNamelessML3BinOp::Times,
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_arith_binop(
    derivation: &EvalNamelessML3Derivation,
    rule: EvalNamelessML3DerivationRule,
    op: EvalNamelessML3BinOp,
    as_arith_judgment: fn(&EvalNamelessML3Judgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr:
            EvalNamelessML3Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalNamelessML3Value::Int(result),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 evalto i3"),
        );
    };

    if *expr_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 evalto i3"),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, first_int)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 evalto i1", &first),
                ));
            };
            let Some((second_env, second_expr, second_int)) = as_eval_to_int(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto i2", &second),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_arith_judgment(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", third_shape, &third),
                ));
            };

            if first_env == env
                && first_expr == left.as_ref()
                && second_env == env
                && second_expr == right.as_ref()
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalNamelessML3Value::Int(third_left),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalNamelessML3Value::Int(third_right),
                };
                let expected_third = match op {
                    EvalNamelessML3BinOp::Plus => EvalNamelessML3Judgment::PlusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalNamelessML3BinOp::Minus => EvalNamelessML3Judgment::MinusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalNamelessML3BinOp::Times => EvalNamelessML3Judgment::TimesIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalNamelessML3BinOp::Lt => {
                        unreachable!("lt is handled by check_e_lt")
                    }
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "make the arithmetic premises consistent with e1/e2 and i3",
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

fn check_e_lt(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::ELt;
    let EvalNamelessML3Judgment::EvalTo {
        env,
        expr:
            EvalNamelessML3Expr::BinOp {
                op: EvalNamelessML3BinOp::Lt,
                left,
                right,
            },
        value: EvalNamelessML3Value::Bool(result),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 evalto b"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, first_int)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 evalto i1", &first),
                ));
            };
            let Some((second_env, second_expr, second_int)) = as_eval_to_int(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto i2", &second),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_less_than_is(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", "i1 less than i2 is b", &third),
                ));
            };

            if first_env == env
                && first_expr == left.as_ref()
                && second_env == env
                && second_expr == right.as_ref()
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalNamelessML3Value::Int(third_left),
                };
                let expected_second = EvalNamelessML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalNamelessML3Value::Int(third_right),
                };
                let expected_third = EvalNamelessML3Judgment::LessThanIs {
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
                        "make the less-than premises consistent with e1/e2 and b",
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

fn check_b_plus(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::BPlus;
    match &derivation.judgment {
        EvalNamelessML3Judgment::PlusIs {
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
            wrong_conclusion_form_message(rule, "i1 plus i2 is i3"),
        ),
    }
}

fn check_b_minus(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::BMinus;
    match &derivation.judgment {
        EvalNamelessML3Judgment::MinusIs {
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
            wrong_conclusion_form_message(rule, "i1 minus i2 is i3"),
        ),
    }
}

fn check_b_times(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::BTimes;
    match &derivation.judgment {
        EvalNamelessML3Judgment::TimesIs {
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
            wrong_conclusion_form_message(rule, "i1 times i2 is i3"),
        ),
    }
}

fn check_b_lt(
    derivation: &EvalNamelessML3Derivation,
) -> Result<EvalNamelessML3Judgment, CheckError> {
    let rule = EvalNamelessML3DerivationRule::BLt;
    match &derivation.judgment {
        EvalNamelessML3Judgment::LessThanIs {
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
            wrong_conclusion_form_message(rule, "i1 less than i2 is b"),
        ),
    }
}

fn as_eval_to(
    judgment: &EvalNamelessML3Judgment,
) -> Option<(
    &EvalNamelessML3Env,
    &EvalNamelessML3Expr,
    &EvalNamelessML3Value,
)> {
    let EvalNamelessML3Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_int(
    judgment: &EvalNamelessML3Judgment,
) -> Option<(&EvalNamelessML3Env, &EvalNamelessML3Expr, i64)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalNamelessML3Value::Int(value_int) = value else {
        return None;
    };
    Some((env, expr, *value_int))
}

fn as_eval_to_bool(
    judgment: &EvalNamelessML3Judgment,
) -> Option<(&EvalNamelessML3Env, &EvalNamelessML3Expr, bool)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalNamelessML3Value::Bool(value_bool) = value else {
        return None;
    };
    Some((env, expr, *value_bool))
}

fn as_plus_is(judgment: &EvalNamelessML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalNamelessML3Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalNamelessML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalNamelessML3Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalNamelessML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalNamelessML3Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalNamelessML3Judgment) -> Option<(i64, i64, bool)> {
    let EvalNamelessML3Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn push_value(env: &EvalNamelessML3Env, value: &EvalNamelessML3Value) -> EvalNamelessML3Env {
    let mut values = env.0.clone();
    values.push(value.clone());
    EvalNamelessML3Env(values)
}

fn rule_violation(derivation: &EvalNamelessML3Derivation, detail: impl Into<String>) -> CheckError {
    CheckError::rule_violation(format!("{}: {}", derivation.rule_name, detail.into()))
        .with_span(derivation.span.clone())
}

#[cfg(test)]
mod tests {
    use crate::core::{CheckErrorKind, Game};

    use super::EvalNamelessML3Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_nameless_ml3_fixtures() {
        let game = EvalNamelessML3Game;
        for source in [
            include_str!("../../../copl/055.copl"),
            include_str!("../../../copl/057.copl"),
            include_str!("../../../copl/059.copl"),
            include_str!("../../../copl/061.copl"),
            include_str!("../../../copl/063.copl"),
            include_str!("../../../copl/065.copl"),
            include_str!("../../../copl/067.copl"),
            include_str!("../../../copl/069.copl"),
        ] {
            let report = game.check(source).expect("fixture should pass");
            assert!(report.summary.contains(" evalto "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 evalto 1 by E-Int { |- 1 evalto 1 by E-Int {} }";
        let err = EvalNamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("number of premises is wrong"));
    }

    #[test]
    fn reports_rule_violation_for_wrong_rule_application() {
        let source = "|- #1 evalto 1 by E-Var {}";
        let err = EvalNamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("E-Var"));
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "|- 1 evalto 1 by E-Unknown {}";
        let err = EvalNamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available"));
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
3 |- let . = 1 in #1 evalto 1 by E-Let {
  3 |- 1 evalto 1 by E-Int {};
  3, 1 |- #2 evalto 1 by E-Var {};
}
"#;

        let err = EvalNamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violations should carry a span");
        assert_eq!(span.line, 4);
        assert_eq!(span.column, 3);
        assert!(err.message().contains("premise path: 2"));
    }
}
