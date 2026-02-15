use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalML5BinOp, EvalML5Binding, EvalML5Derivation, EvalML5Env, EvalML5Expr, EvalML5Judgment,
    EvalML5Pattern, EvalML5Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalML5DerivationRule {
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
    ENil,
    ECons,
    EMatchM1,
    EMatchM2,
    EMatchN,
    EPlus,
    EMinus,
    ETimes,
    ELt,
    MVar,
    MWild,
    MNil,
    MCons,
    NMConsNil,
    NMConsConsL,
    NMConsConsR,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalML5DerivationRule {
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
            "E-Nil" => Some(Self::ENil),
            "E-Cons" => Some(Self::ECons),
            "E-MatchM1" => Some(Self::EMatchM1),
            "E-MatchM2" => Some(Self::EMatchM2),
            "E-MatchN" => Some(Self::EMatchN),
            "E-Plus" => Some(Self::EPlus),
            "E-Minus" => Some(Self::EMinus),
            "E-Times" => Some(Self::ETimes),
            "E-Lt" => Some(Self::ELt),
            "M-Var" => Some(Self::MVar),
            "M-Wild" => Some(Self::MWild),
            "M-Nil" => Some(Self::MNil),
            "M-Cons" => Some(Self::MCons),
            "NM-ConsNil" => Some(Self::NMConsNil),
            "NM-ConsConsL" => Some(Self::NMConsConsL),
            "NM-ConsConsR" => Some(Self::NMConsConsR),
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
            Self::ENil => "E-Nil",
            Self::ECons => "E-Cons",
            Self::EMatchM1 => "E-MatchM1",
            Self::EMatchM2 => "E-MatchM2",
            Self::EMatchN => "E-MatchN",
            Self::EPlus => "E-Plus",
            Self::EMinus => "E-Minus",
            Self::ETimes => "E-Times",
            Self::ELt => "E-Lt",
            Self::MVar => "M-Var",
            Self::MWild => "M-Wild",
            Self::MNil => "M-Nil",
            Self::MCons => "M-Cons",
            Self::NMConsNil => "NM-ConsNil",
            Self::NMConsConsL => "NM-ConsConsL",
            Self::NMConsConsR => "NM-ConsConsR",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalML5Game;

impl Game for EvalML5Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalML5
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

fn infer_judgment(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let Some(rule) = EvalML5DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalML5Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalML5Derivation,
    rule: EvalML5DerivationRule,
) -> Result<EvalML5Judgment, CheckError> {
    match rule {
        EvalML5DerivationRule::EInt => check_e_int(derivation),
        EvalML5DerivationRule::EBool => check_e_bool(derivation),
        EvalML5DerivationRule::EVar => check_e_var(derivation),
        EvalML5DerivationRule::EIfT => check_e_if_t(derivation),
        EvalML5DerivationRule::EIfF => check_e_if_f(derivation),
        EvalML5DerivationRule::ELet => check_e_let(derivation),
        EvalML5DerivationRule::ELetRec => check_e_let_rec(derivation),
        EvalML5DerivationRule::EFun => check_e_fun(derivation),
        EvalML5DerivationRule::EApp => check_e_app(derivation),
        EvalML5DerivationRule::EAppRec => check_e_app_rec(derivation),
        EvalML5DerivationRule::ENil => check_e_nil(derivation),
        EvalML5DerivationRule::ECons => check_e_cons(derivation),
        EvalML5DerivationRule::EMatchM1 => check_e_match_m1(derivation),
        EvalML5DerivationRule::EMatchM2 => check_e_match_m2(derivation),
        EvalML5DerivationRule::EMatchN => check_e_match_n(derivation),
        EvalML5DerivationRule::EPlus => check_e_plus(derivation),
        EvalML5DerivationRule::EMinus => check_e_minus(derivation),
        EvalML5DerivationRule::ETimes => check_e_times(derivation),
        EvalML5DerivationRule::ELt => check_e_lt(derivation),
        EvalML5DerivationRule::MVar => check_m_var(derivation),
        EvalML5DerivationRule::MWild => check_m_wild(derivation),
        EvalML5DerivationRule::MNil => check_m_nil(derivation),
        EvalML5DerivationRule::MCons => check_m_cons(derivation),
        EvalML5DerivationRule::NMConsNil => check_nm_cons_nil(derivation),
        EvalML5DerivationRule::NMConsConsL => check_nm_cons_cons_l(derivation),
        EvalML5DerivationRule::NMConsConsR => check_nm_cons_cons_r(derivation),
        EvalML5DerivationRule::BPlus => check_b_plus(derivation),
        EvalML5DerivationRule::BMinus => check_b_minus(derivation),
        EvalML5DerivationRule::BTimes => check_b_times(derivation),
        EvalML5DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalML5Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalML5Derivation,
    detail: String,
) -> Result<EvalML5Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-Var, E-IfT, E-IfF, E-Let, E-LetRec, E-Fun, E-App, E-AppRec, E-Nil, E-Cons, E-MatchM1, E-MatchM2, E-MatchN, E-Plus, E-Minus, E-Times, E-Lt, M-Var, M-Wild, M-Nil, M-Cons, NM-ConsNil, NM-ConsConsL, NM-ConsConsR, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalML5DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalML5DerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalML5DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalML5Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalML5DerivationRule,
    expected: &[EvalML5Judgment],
    actual: &[EvalML5Judgment],
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

fn check_e_int(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EInt;
    match &derivation.judgment {
        EvalML5Judgment::EvalTo {
            env: _,
            expr: EvalML5Expr::Int(expr_int),
            value: EvalML5Value::Int(value_int),
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

fn check_e_bool(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EBool;
    match &derivation.judgment {
        EvalML5Judgment::EvalTo {
            env: _,
            expr: EvalML5Expr::Bool(expr_bool),
            value: EvalML5Value::Bool(value_bool),
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

fn check_e_var(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EVar;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::Var(var_name),
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x evalto v"),
        );
    };

    let Some(binding) = env.0.iter().rev().find(|binding| binding.name == *var_name) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, ..., x = v, ... |- x evalto v"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            if binding.value == *value {
                Ok(derivation.judgment.clone())
            } else {
                let expected = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: EvalML5Expr::Var(var_name.clone()),
                    value: binding.value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        std::slice::from_ref(&derivation.judgment),
                        "use the nearest binding for x in Gamma",
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

fn check_e_if_t(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EIfT;
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::If {
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalML5Value::Bool(true),
                };
                let expected_second = EvalML5Judgment::EvalTo {
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

fn check_e_if_f(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EIfF;
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::If {
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: condition.as_ref().clone(),
                    value: EvalML5Value::Bool(false),
                };
                let expected_second = EvalML5Judgment::EvalTo {
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

fn check_e_let(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::ELet;
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::Let {
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: bound_expr.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML5Judgment::EvalTo {
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

fn check_e_let_rec(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::ELetRec;
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::LetRec {
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

            let recursive_value = EvalML5Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let expected_env = push_binding(env, name, &recursive_value);

            if first_env == &expected_env && first_expr == body.as_ref() && first_value == value {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::EvalTo {
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

fn check_e_fun(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EFun;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::Fun { param, body },
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
            let expected = EvalML5Value::Closure {
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
                        &[EvalML5Judgment::EvalTo {
                            env: env.clone(),
                            expr: EvalML5Expr::Fun {
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

fn check_e_app(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EApp;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::App { func, arg },
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
            let EvalML5Value::Closure {
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalML5Judgment::EvalTo {
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

fn check_e_app_rec(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EAppRec;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::App { func, arg },
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
            let EvalML5Value::RecClosure {
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: func.as_ref().clone(),
                    value: first_value.clone(),
                };
                let expected_second = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: arg.as_ref().clone(),
                    value: second_value.clone(),
                };
                let expected_third = EvalML5Judgment::EvalTo {
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

fn check_e_nil(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::ENil;
    match &derivation.judgment {
        EvalML5Judgment::EvalTo {
            env: _,
            expr: EvalML5Expr::Nil,
            value: EvalML5Value::Nil,
        } => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- [] evalto []"),
        ),
    }
}

fn check_e_cons(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::ECons;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::Cons { head, tail },
        value:
            EvalML5Value::Cons {
                head: head_value,
                tail: tail_value,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 :: e2 evalto v1 :: v2"),
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
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 evalto v2", &second),
                ));
            };

            if first_env == env
                && first_expr == head.as_ref()
                && first_value == head_value.as_ref()
                && second_env == env
                && second_expr == tail.as_ref()
                && second_value == tail_value.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: head.as_ref().clone(),
                    value: head_value.as_ref().clone(),
                };
                let expected_second = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: tail.as_ref().clone(),
                    value: tail_value.as_ref().clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "evaluate e1/e2 under Gamma and keep v1/v2 consistent with the conclusion list value",
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

fn check_e_match_m1(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    check_e_match_hit(derivation, EvalML5DerivationRule::EMatchM1, true)
}

fn check_e_match_m2(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    check_e_match_hit(derivation, EvalML5DerivationRule::EMatchM2, false)
}

fn check_e_match_hit(
    derivation: &EvalML5Derivation,
    rule: EvalML5DerivationRule,
    require_single_clause: bool,
) -> Result<EvalML5Judgment, CheckError> {
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::Match { scrutinee, clauses },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- match e with p1 -> e1 | ... evalto v"),
        );
    };

    if clauses.is_empty() || (require_single_clause && clauses.len() != 1) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- match e with p1 -> e1 | ... evalto v"),
        );
    }

    let first_clause = &clauses[0];
    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, scrutinee_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e evalto v0", &first),
                ));
            };
            let Some((matched_pattern, matched_value, delta)) = as_matches(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "p1 matches v0 when (Delta)",
                        &second,
                    ),
                ));
            };
            let Some((third_env, third_expr, third_value)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "Gamma, Delta |- e1 evalto v",
                        &third,
                    ),
                ));
            };

            let expected_third_env = append_env(env, delta);
            if first_env == env
                && first_expr == scrutinee.as_ref()
                && matched_pattern == &first_clause.pattern
                && matched_value == scrutinee_value
                && third_env == &expected_third_env
                && third_expr == &first_clause.body
                && third_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: scrutinee.as_ref().clone(),
                    value: scrutinee_value.clone(),
                };
                let expected_second = EvalML5Judgment::Matches {
                    pattern: first_clause.pattern.clone(),
                    value: scrutinee_value.clone(),
                    bindings: delta.clone(),
                };
                let expected_third = EvalML5Judgment::EvalTo {
                    env: expected_third_env,
                    expr: first_clause.body.clone(),
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "evaluate scrutinee first, prove the first pattern matches, then evaluate body under Gamma extended by pattern bindings",
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

fn check_e_match_n(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::EMatchN;
    let EvalML5Judgment::EvalTo {
        env,
        expr: EvalML5Expr::Match { scrutinee, clauses },
        value,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- match e with p1 -> e1 | ... evalto v"),
        );
    };

    if clauses.len() < 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- match e with p1 -> e1 | p2 -> e2 | ... evalto v",
            ),
        );
    }

    let first_clause = &clauses[0];
    let rest_clauses = clauses[1..].to_vec();
    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_expr, scrutinee_value)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e evalto v0", &first),
                ));
            };
            let Some((not_pattern, not_value)) = as_not_match(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "p1 doesn't match v0", &second),
                ));
            };
            let Some((third_env, third_expr, third_value)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "Gamma |- match e with p2 -> e2 | ... evalto v",
                        &third,
                    ),
                ));
            };

            let expected_third_expr = EvalML5Expr::Match {
                scrutinee: scrutinee.clone(),
                clauses: rest_clauses,
            };
            if first_env == env
                && first_expr == scrutinee.as_ref()
                && not_pattern == &first_clause.pattern
                && not_value == scrutinee_value
                && third_env == env
                && third_expr == &expected_third_expr
                && third_value == value
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: scrutinee.as_ref().clone(),
                    value: scrutinee_value.clone(),
                };
                let expected_second = EvalML5Judgment::NotMatch {
                    pattern: first_clause.pattern.clone(),
                    value: scrutinee_value.clone(),
                };
                let expected_third = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: expected_third_expr,
                    value: value.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "if first pattern does not match, continue with the remaining clauses under the same Gamma",
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

fn check_m_var(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::MVar;
    let EvalML5Judgment::Matches {
        pattern: EvalML5Pattern::Var(name),
        value,
        bindings,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "x matches v when (x = v)"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            let expected = EvalML5Env(vec![EvalML5Binding {
                name: name.clone(),
                value: value.clone(),
            }]);
            if *bindings == expected {
                Ok(derivation.judgment.clone())
            } else {
                let expected_judgment = EvalML5Judgment::Matches {
                    pattern: EvalML5Pattern::Var(name.clone()),
                    value: value.clone(),
                    bindings: expected,
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_judgment],
                        std::slice::from_ref(&derivation.judgment),
                        "M-Var must bind the variable to the matched value",
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

fn check_m_wild(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::MWild;
    let EvalML5Judgment::Matches {
        pattern: EvalML5Pattern::Wildcard,
        value: _,
        bindings,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "_ matches v when ()"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] if bindings.0.is_empty() => Ok(derivation.judgment.clone()),
        [] => Err(rule_violation(
            derivation,
            "Wrong rule application: M-Wild (expected empty bindings; fix: use 'when ()')",
        )),
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_m_nil(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::MNil;
    let EvalML5Judgment::Matches {
        pattern: EvalML5Pattern::Nil,
        value: EvalML5Value::Nil,
        bindings,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "[] matches [] when ()"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] if bindings.0.is_empty() => Ok(derivation.judgment.clone()),
        [] => Err(rule_violation(
            derivation,
            "Wrong rule application: M-Nil (expected empty bindings; fix: use 'when ()')",
        )),
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_m_cons(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::MCons;
    let EvalML5Judgment::Matches {
        pattern: EvalML5Pattern::Cons { head, tail },
        value: EvalML5Value::Cons { head: v1, tail: v2 },
        bindings,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "p1 :: p2 matches v1 :: v2 when (Delta1, Delta2)"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let Some((p1, vv1, delta1)) = as_matches(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "p1 matches v1 when (Delta1)",
                        &first,
                    ),
                ));
            };
            let Some((p2, vv2, delta2)) = as_matches(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "p2 matches v2 when (Delta2)",
                        &second,
                    ),
                ));
            };

            let expected = append_env(delta1, delta2);
            if p1 == head.as_ref()
                && vv1 == v1.as_ref()
                && p2 == tail.as_ref()
                && vv2 == v2.as_ref()
                && &expected == bindings
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::Matches {
                    pattern: head.as_ref().clone(),
                    value: v1.as_ref().clone(),
                    bindings: delta1.clone(),
                };
                let expected_second = EvalML5Judgment::Matches {
                    pattern: tail.as_ref().clone(),
                    value: v2.as_ref().clone(),
                    bindings: delta2.clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "M-Cons composes bindings from head and tail matches in order",
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

fn check_nm_cons_nil(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::NMConsNil;
    match &derivation.judgment {
        EvalML5Judgment::NotMatch {
            pattern: EvalML5Pattern::Nil,
            value: EvalML5Value::Cons { .. },
        } => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "[] doesn't match v1 :: v2"),
        ),
    }
}

fn check_nm_cons_cons_l(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::NMConsConsL;
    let EvalML5Judgment::NotMatch {
        pattern: EvalML5Pattern::Cons { head, tail: _ },
        value: EvalML5Value::Cons { head: v1, tail: _ },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "p1 :: p2 doesn't match v1 :: v2"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((p1, vv1)) = as_not_match(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "p1 doesn't match v1", &first),
                ));
            };
            if p1 == head.as_ref() && vv1 == v1.as_ref() {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::NotMatch {
                    pattern: head.as_ref().clone(),
                    value: v1.as_ref().clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "NM-ConsConsL uses mismatch of head pattern/value",
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

fn check_nm_cons_cons_r(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::NMConsConsR;
    let EvalML5Judgment::NotMatch {
        pattern: EvalML5Pattern::Cons { head: _, tail },
        value: EvalML5Value::Cons { head: _, tail: v2 },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "p1 :: p2 doesn't match v1 :: v2"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((p2, vv2)) = as_not_match(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "p2 doesn't match v2", &first),
                ));
            };
            if p2 == tail.as_ref() && vv2 == v2.as_ref() {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = EvalML5Judgment::NotMatch {
                    pattern: tail.as_ref().clone(),
                    value: v2.as_ref().clone(),
                };
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "NM-ConsConsR uses mismatch of tail pattern/value",
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

fn check_e_plus(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML5DerivationRule::EPlus,
        EvalML5BinOp::Plus,
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML5DerivationRule::EMinus,
        EvalML5BinOp::Minus,
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalML5DerivationRule::ETimes,
        EvalML5BinOp::Times,
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_arith_binop(
    derivation: &EvalML5Derivation,
    rule: EvalML5DerivationRule,
    op: EvalML5BinOp,
    as_arith_judgment: fn(&EvalML5Judgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalML5Judgment, CheckError> {
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        value: EvalML5Value::Int(result),
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalML5Value::Int(third_left),
                };
                let expected_second = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalML5Value::Int(third_right),
                };
                let expected_third = match op {
                    EvalML5BinOp::Plus => EvalML5Judgment::PlusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML5BinOp::Minus => EvalML5Judgment::MinusIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML5BinOp::Times => EvalML5Judgment::TimesIs {
                        left: third_left,
                        right: third_right,
                        result: *result,
                    },
                    EvalML5BinOp::Lt => unreachable!("lt is not arithmetic"),
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

fn check_e_lt(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::ELt;
    let EvalML5Judgment::EvalTo {
        env,
        expr:
            EvalML5Expr::BinOp {
                op: EvalML5BinOp::Lt,
                left,
                right,
            },
        value: EvalML5Value::Bool(result),
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
                let expected_first = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: left.as_ref().clone(),
                    value: EvalML5Value::Int(third_left),
                };
                let expected_second = EvalML5Judgment::EvalTo {
                    env: env.clone(),
                    expr: right.as_ref().clone(),
                    value: EvalML5Value::Int(third_right),
                };
                let expected_third = EvalML5Judgment::LessThanIs {
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

fn check_b_plus(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::BPlus;
    match &derivation.judgment {
        EvalML5Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::BMinus;
    match &derivation.judgment {
        EvalML5Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::BTimes;
    match &derivation.judgment {
        EvalML5Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalML5Derivation) -> Result<EvalML5Judgment, CheckError> {
    let rule = EvalML5DerivationRule::BLt;
    match &derivation.judgment {
        EvalML5Judgment::LessThanIs {
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

fn as_eval_to(judgment: &EvalML5Judgment) -> Option<(&EvalML5Env, &EvalML5Expr, &EvalML5Value)> {
    let EvalML5Judgment::EvalTo { env, expr, value } = judgment else {
        return None;
    };
    Some((env, expr, value))
}

fn as_eval_to_int(judgment: &EvalML5Judgment) -> Option<(&EvalML5Env, &EvalML5Expr, i64)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalML5Value::Int(value_int) = value else {
        return None;
    };
    Some((env, expr, *value_int))
}

fn as_eval_to_bool(judgment: &EvalML5Judgment) -> Option<(&EvalML5Env, &EvalML5Expr, bool)> {
    let (env, expr, value) = as_eval_to(judgment)?;
    let EvalML5Value::Bool(value_bool) = value else {
        return None;
    };
    Some((env, expr, *value_bool))
}

fn as_matches(judgment: &EvalML5Judgment) -> Option<(&EvalML5Pattern, &EvalML5Value, &EvalML5Env)> {
    let EvalML5Judgment::Matches {
        pattern,
        value,
        bindings,
    } = judgment
    else {
        return None;
    };
    Some((pattern, value, bindings))
}

fn as_not_match(judgment: &EvalML5Judgment) -> Option<(&EvalML5Pattern, &EvalML5Value)> {
    let EvalML5Judgment::NotMatch { pattern, value } = judgment else {
        return None;
    };
    Some((pattern, value))
}

fn as_plus_is(judgment: &EvalML5Judgment) -> Option<(i64, i64, i64)> {
    let EvalML5Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalML5Judgment) -> Option<(i64, i64, i64)> {
    let EvalML5Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalML5Judgment) -> Option<(i64, i64, i64)> {
    let EvalML5Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalML5Judgment) -> Option<(i64, i64, bool)> {
    let EvalML5Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn push_binding(env: &EvalML5Env, name: &str, value: &EvalML5Value) -> EvalML5Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalML5Binding {
        name: name.to_string(),
        value: value.clone(),
    });
    EvalML5Env(bindings)
}

fn append_env(left: &EvalML5Env, right: &EvalML5Env) -> EvalML5Env {
    let mut bindings = left.0.clone();
    bindings.extend(right.0.iter().cloned());
    EvalML5Env(bindings)
}

fn rule_violation(derivation: &EvalML5Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalML5Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_ml5_fixtures() {
        let game = EvalML5Game;
        for source in [
            include_str!("../../../copl/078.copl"),
            include_str!("../../../copl/079.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains(" evalto "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "x matches 1 when (x = 1) by M-Var { x matches 1 when (x = 1) by M-Var {} }";
        let err = EvalML5Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: M-Var"));
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
        let source = "x matches 1 when (x = 2) by M-Var {}";
        let err = EvalML5Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: M-Var"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "x matches 1 when (x = 1) by M-Unknown {}";
        let err = EvalML5Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: E-Int, E-Bool, E-Var"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
|- match l with x :: [] -> x | x :: y :: z -> x evalto 1 by E-MatchN {
  |- l evalto 1 :: 2 :: [] by E-Unknown {};
  x :: [] doesn't match 1 :: 2 :: [] by NM-ConsConsR { [] doesn't match 2 :: [] by NM-ConsNil {} };
  |- match l with x :: y :: z -> x evalto 1 by E-MatchM1 {
    |- l evalto 1 :: 2 :: [] by E-Var {};
    x :: y :: z matches 1 :: 2 :: [] when (x = 1, y = 2, z = []) by M-Cons {
      x matches 1 when (x = 1) by M-Var {};
      y :: z matches 2 :: [] when (y = 2, z = []) by M-Cons {
        y matches 2 when (y = 2) by M-Var {};
        z matches [] when (z = []) by M-Var {};
      }
    };
    x = 1, y = 2, z = [] |- x evalto 1 by E-Var {}
  }
}
"#;
        let err = EvalML5Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
