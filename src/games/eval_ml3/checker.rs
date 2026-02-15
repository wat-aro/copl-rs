use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalML3BinOp, EvalML3Binding, EvalML3Derivation, EvalML3Env, EvalML3Expr, EvalML3Judgment,
    EvalML3Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalML3DerivationRule {
    EInt,
    EBool,
    EVar1,
    EVar2,
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

impl EvalML3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-Var1" => Some(Self::EVar1),
            "E-Var2" => Some(Self::EVar2),
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
            Self::EVar1 => "E-Var1",
            Self::EVar2 => "E-Var2",
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
pub struct EvalML3Game;

impl Game for EvalML3Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalML3
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

fn infer_judgment(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let Some(rule) = EvalML3DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalML3Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalML3Derivation,
    rule: EvalML3DerivationRule,
) -> Result<EvalML3Judgment, CheckError> {
    match rule {
        EvalML3DerivationRule::EInt => check_e_int(derivation),
        EvalML3DerivationRule::EBool => check_e_bool(derivation),
        EvalML3DerivationRule::EVar1 => check_e_var1(derivation),
        EvalML3DerivationRule::EVar2 => check_e_var2(derivation),
        EvalML3DerivationRule::EIfT => check_e_if_t(derivation),
        EvalML3DerivationRule::EIfF => check_e_if_f(derivation),
        EvalML3DerivationRule::ELet => check_e_let(derivation),
        EvalML3DerivationRule::ELetRec => check_e_let_rec(derivation),
        EvalML3DerivationRule::EFun => check_e_fun(derivation),
        EvalML3DerivationRule::EApp => check_e_app(derivation),
        EvalML3DerivationRule::EAppRec => check_e_app_rec(derivation),
        EvalML3DerivationRule::EPlus => check_e_plus(derivation),
        EvalML3DerivationRule::EMinus => check_e_minus(derivation),
        EvalML3DerivationRule::ETimes => check_e_times(derivation),
        EvalML3DerivationRule::ELt => check_e_lt(derivation),
        EvalML3DerivationRule::BPlus => check_b_plus(derivation),
        EvalML3DerivationRule::BMinus => check_b_minus(derivation),
        EvalML3DerivationRule::BTimes => check_b_times(derivation),
        EvalML3DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalML3Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalML3Derivation,
    detail: String,
) -> Result<EvalML3Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-Var1, E-Var2, E-IfT, E-IfF, E-Let, E-LetRec, E-Fun, E-App, E-AppRec, E-Plus, E-Minus, E-Times, E-Lt, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalML3DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalML3DerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalML3DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalML3Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalML3DerivationRule,
    expected: &[EvalML3Judgment],
    actual: &[EvalML3Judgment],
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

fn check_e_int(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EInt;
    match &derivation.judgment {
        EvalML3Judgment::EvalTo {
            env: _,
            expr: EvalML3Expr::Int(expr_int),
            value: EvalML3Value::Int(value_int),
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

fn check_e_bool(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EBool;
    match &derivation.judgment {
        EvalML3Judgment::EvalTo {
            env: _,
            expr: EvalML3Expr::Bool(expr_bool),
            value: EvalML3Value::Bool(value_bool),
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

fn check_e_var1(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EVar1;
    let EvalML3Judgment::EvalTo {
        env,
        expr: EvalML3Expr::Var(var_name),
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x = v |- x evalto v"),
        );
    };

    let Some(last) = env.0.last() else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x = v |- x evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            if last.name == *var_name && last.value == *value {
                Ok(derivation.judgment.clone())
            } else {
                let expected = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: EvalML3Expr::Var(last.name.clone()),
                    value: last.value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        std::slice::from_ref(&derivation.judgment),
                        "make the last binding name/value consistent with the conclusion variable and value",
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

fn check_e_var2(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EVar2;
    let EvalML3Judgment::EvalTo {
        env,
        expr: EvalML3Expr::Var(var_name),
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, y = v' |- x evalto v"),
        );
    };

    let Some((prefix, last)) = split_last_binding(env) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, y = v' |- x evalto v"),
        );
    };

    if last.name == *var_name {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma, y = v' |- x evalto v (x and y must differ)",
            ),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- x evalto v", &first),
                ));
            };

            let expected_env = EvalML3Env(prefix.to_vec());
            if *first_env == expected_env
                && first_expr == &EvalML3Expr::Var(var_name.clone())
                && first_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML3Judgment::EvalTo {
                    env: expected_env,
                    expr: EvalML3Expr::Var(var_name.clone()),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "drop only the last binding from Gamma, and keep x/v consistent",
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

fn check_e_if_t(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EIfT;
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::If {
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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalML3Value::Bool(true),
                };
                let expected_second = EvalML3Judgment::EvalTo {
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
                        "keep Gamma/e1/e2/v links consistent and enforce true in the first premise",
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

fn check_e_if_f(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EIfF;
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::If {
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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalML3Value::Bool(false),
                };
                let expected_second = EvalML3Judgment::EvalTo {
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
                        "keep Gamma/e1/e3/v links consistent and enforce false in the first premise",
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

fn check_e_let(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::ELet;
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::Let {
                name,
                bound_expr,
                body,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let x = e1 in e2 evalto v"),
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
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma, x = v1 |- e2 evalto v",
                        &second,
                    ),
                ));
            };

            let expected_second_env = push_binding(env, name, first_value);
            if first_env == env
                && first_expr == bound_expr.as_ref()
                && second_env == &expected_second_env
                && second_expr == body.as_ref()
                && second_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: bound_expr.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML3Judgment::EvalTo {
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
                        "keep Gamma/x/e1/e2/v links consistent and extend Gamma with x = v1 only for the second premise",
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

fn check_e_let_rec(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::ELetRec;
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::LetRec {
                name,
                param,
                fun_body,
                body,
            },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let rec f = fun x -> e1 in e2 evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_env, first_expr, first_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma, f = (Gamma)[rec f = fun x -> e1] |- e2 evalto v",
                        &first,
                    ),
                ));
            };

            let recursive_value = EvalML3Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let expected_env = push_binding(env, name, &recursive_value);

            if first_env == &expected_env && first_expr == body.as_ref() && first_value == value {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML3Judgment::EvalTo {
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
                        "extend Gamma with recursive closure f before checking e2",
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

fn check_e_fun(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EFun;
    let EvalML3Judgment::EvalTo {
        env,
        expr: EvalML3Expr::Fun { param, body },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun x -> e evalto (Gamma)[fun x -> e]"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            let expected = EvalML3Value::Closure {
                env: env.clone(),
                param: param.clone(),
                body: body.as_ref().clone(),
            };
            if value == &expected {
                Ok(derivation.judgment.clone())
            } else {
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[EvalML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: EvalML3Expr::Fun {
                                param: param.clone(),
                                body: body.clone(),
                            },
                            value: expected,
                        }],
                        std::slice::from_ref(&derivation.judgment),
                        "closure must capture Gamma and preserve the same parameter/body as the conclusion function",
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

fn check_e_app(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EApp;
    let EvalML3Judgment::EvalTo {
        env,
        expr: EvalML3Expr::App { func, arg },
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
                        "Gamma |- e1 evalto (Gamma')[fun x -> e0]",
                        &first,
                    ),
                ));
            };
            let EvalML3Value::Closure {
                env: closure_env,
                param,
                body,
            } = first_value
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[fun x -> e0]",
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
                        "Gamma', x = v2 |- e0 evalto v",
                        &third,
                    ),
                ));
            };

            let expected_third_env = push_binding(closure_env, param, second_value);
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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalML3Judgment::EvalTo {
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
                        "evaluate e1/e2 under Gamma, then evaluate closure body under captured Gamma' extended with x = v2",
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

fn check_e_app_rec(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::EAppRec;
    let EvalML3Judgment::EvalTo {
        env,
        expr: EvalML3Expr::App { func, arg },
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
                        "Gamma |- e1 evalto (Gamma')[rec f = fun x -> e0]",
                        &first,
                    ),
                ));
            };
            let EvalML3Value::RecClosure {
                env: closure_env,
                name,
                param,
                body,
            } = first_value
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 evalto (Gamma')[rec f = fun x -> e0]",
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
                        "Gamma', f = (Gamma')[rec f = fun x -> e0], x = v2 |- e0 evalto v",
                        &third,
                    ),
                ));
            };

            let recursive_value = first_value.clone();
            let expected_third_env = push_binding(
                &push_binding(closure_env, name, &recursive_value),
                param,
                second_value,
            );

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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalML3Judgment::EvalTo {
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
                        "evaluate e1/e2 under Gamma, then evaluate recursive body under captured Gamma' extended with f and x = v2",
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

fn check_e_plus(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML3DerivationRule::EPlus,
        EvalML3BinOp::Plus,
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML3DerivationRule::EMinus,
        EvalML3BinOp::Minus,
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML3DerivationRule::ETimes,
        EvalML3BinOp::Times,
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_arith_binop(
    derivation: &EvalML3Derivation,
    rule: EvalML3DerivationRule,
    op: EvalML3BinOp,
    as_arith_judgment: fn(&EvalML3Judgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalML3Judgment, CheckError> {
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML3Value::Int(result),
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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalML3Value::Int(third_left),
                };
                let expected_second = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalML3Value::Int(third_right),
                };
                let expected_third = match op {
                    EvalML3BinOp::Plus => EvalML3Judgment::PlusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML3BinOp::Minus => EvalML3Judgment::MinusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML3BinOp::Times => EvalML3Judgment::TimesIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML3BinOp::Lt => unreachable!("lt is not arithmetic"),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "keep Gamma/e1/e2/i1/i2/i3 links consistent across conclusion and all premises",
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

fn check_e_lt(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::ELt;
    let EvalML3Judgment::EvalTo {
        env,
        expr:
            EvalML3Expr::BinOp {
                op: EvalML3BinOp::Lt,
                left,
                right,
            },
        value: EvalML3Value::Bool(result),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 evalto b3"),
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
                    wrong_premise_form_message(rule, "third", "i1 less than i2 is b3", &third),
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
                let expected_first = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalML3Value::Int(third_left),
                };
                let expected_second = EvalML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalML3Value::Int(third_right),
                };
                let expected_third = EvalML3Judgment::LessThanIs {
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
                        "keep Gamma/e1/e2/i1/i2/b3 links consistent across conclusion and all premises",
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

fn check_b_plus(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::BPlus;
    match &derivation.judgment {
        EvalML3Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::BMinus;
    match &derivation.judgment {
        EvalML3Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::BTimes;
    match &derivation.judgment {
        EvalML3Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalML3Derivation) -> Result<EvalML3Judgment, CheckError> {
    let rule = EvalML3DerivationRule::BLt;
    match &derivation.judgment {
        EvalML3Judgment::LessThanIs {
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

fn as_eval_to(judgment: &EvalML3Judgment) -> Option<(&EvalML3Env, &EvalML3Expr, &EvalML3Value)> {
    let EvalML3Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_int(judgment: &EvalML3Judgment) -> Option<(&EvalML3Env, &EvalML3Expr, i64)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalML3Value::Int(value_int) = value else {
        return None;
    };
    Some((env, expr, *value_int))
}

fn as_eval_to_bool(judgment: &EvalML3Judgment) -> Option<(&EvalML3Env, &EvalML3Expr, bool)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalML3Value::Bool(value_bool) = value else {
        return None;
    };
    Some((env, expr, *value_bool))
}

fn as_plus_is(judgment: &EvalML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalML3Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalML3Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalML3Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalML3Judgment) -> Option<(i64, i64, bool)> {
    let EvalML3Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn split_last_binding(env: &EvalML3Env) -> Option<(&[EvalML3Binding], &EvalML3Binding)> {
    let (last, prefix) = env.0.split_last()?;
    Some((prefix, last))
}

fn push_binding(env: &EvalML3Env, name: &str, value: &EvalML3Value) -> EvalML3Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalML3Binding {
        name: name.to_string(),
        value: value.clone(),
    });
    EvalML3Env(bindings)
}

fn rule_violation(derivation: &EvalML3Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalML3Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_ml3_fixtures() {
        let game = EvalML3Game;
        for source in [
            include_str!("../../../copl/040.copl"),
            include_str!("../../../copl/041.copl"),
            include_str!("../../../copl/042.copl"),
            include_str!("../../../copl/043.copl"),
            include_str!("../../../copl/044.copl"),
            include_str!("../../../copl/045.copl"),
            include_str!("../../../copl/046.copl"),
            include_str!("../../../copl/047.copl"),
            include_str!("../../../copl/048.copl"),
            include_str!("../../../copl/049.copl"),
            include_str!("../../../copl/050.copl"),
            include_str!("../../../copl/051.copl"),
            include_str!("../../../copl/052.copl"),
            include_str!("../../../copl/053.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains(" evalto "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- fun x -> x evalto ()[fun x -> x] by E-Fun { |- 0 evalto 0 by E-Int {} }";
        let err = EvalML3Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Fun"));
        assert!(err.message().contains("expected: 0, actual: 1"));
        assert!(err.message().contains("premise path: root"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_wrong_rule_application() {
        let source = "|- fun x -> x evalto ()[fun y -> y] by E-Fun {}";
        let err = EvalML3Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: E-Fun"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "|- fun x -> x evalto ()[fun x -> x] by E-Unknown {}";
        let err = EvalML3Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: E-Int, E-Bool, E-Var1, E-Var2"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
|- let y = 2 in fun x -> x + y evalto (y = 2)[fun x -> x + y] by E-Let {
  |- 2 evalto 2 by E-Unknown {};
  y = 2 |- fun x -> x + y evalto (y = 2)[fun x -> x + y] by E-Fun {}
  }
"#;
        let err = EvalML3Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
