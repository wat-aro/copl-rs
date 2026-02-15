use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Derivation, NamelessML3Env,
    NamelessML3Judgment,
};

#[derive(Debug, Clone, Copy)]
enum NamelessML3DerivationRule {
    TrInt,
    TrBool,
    TrVar1,
    TrVar2,
    TrIf,
    TrPlus,
    TrMinus,
    TrTimes,
    TrLt,
    TrLet,
    TrFun,
    TrApp,
    TrLetRec,
}

impl NamelessML3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "Tr-Int" => Some(Self::TrInt),
            "Tr-Bool" => Some(Self::TrBool),
            "Tr-Var1" => Some(Self::TrVar1),
            "Tr-Var2" => Some(Self::TrVar2),
            "Tr-If" => Some(Self::TrIf),
            "Tr-Plus" => Some(Self::TrPlus),
            "Tr-Minus" => Some(Self::TrMinus),
            "Tr-Times" => Some(Self::TrTimes),
            "Tr-Lt" => Some(Self::TrLt),
            "Tr-Let" => Some(Self::TrLet),
            "Tr-Fun" => Some(Self::TrFun),
            "Tr-App" => Some(Self::TrApp),
            "Tr-LetRec" => Some(Self::TrLetRec),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::TrInt => "Tr-Int",
            Self::TrBool => "Tr-Bool",
            Self::TrVar1 => "Tr-Var1",
            Self::TrVar2 => "Tr-Var2",
            Self::TrIf => "Tr-If",
            Self::TrPlus => "Tr-Plus",
            Self::TrMinus => "Tr-Minus",
            Self::TrTimes => "Tr-Times",
            Self::TrLt => "Tr-Lt",
            Self::TrLet => "Tr-Let",
            Self::TrFun => "Tr-Fun",
            Self::TrApp => "Tr-App",
            Self::TrLetRec => "Tr-LetRec",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NamelessML3Game;

impl Game for NamelessML3Game {
    fn kind(&self) -> GameKind {
        GameKind::NamelessML3
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

fn infer_judgment(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &NamelessML3Derivation,
) -> Result<NamelessML3Judgment, CheckError> {
    let Some(rule) = NamelessML3DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &NamelessML3Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &NamelessML3Derivation,
    rule: NamelessML3DerivationRule,
) -> Result<NamelessML3Judgment, CheckError> {
    match rule {
        NamelessML3DerivationRule::TrInt => check_tr_int(derivation),
        NamelessML3DerivationRule::TrBool => check_tr_bool(derivation),
        NamelessML3DerivationRule::TrVar1 => check_tr_var1(derivation),
        NamelessML3DerivationRule::TrVar2 => check_tr_var2(derivation),
        NamelessML3DerivationRule::TrIf => check_tr_if(derivation),
        NamelessML3DerivationRule::TrPlus => check_tr_binop(
            derivation,
            NamelessML3DerivationRule::TrPlus,
            NamelessML3BinOp::Plus,
        ),
        NamelessML3DerivationRule::TrMinus => check_tr_binop(
            derivation,
            NamelessML3DerivationRule::TrMinus,
            NamelessML3BinOp::Minus,
        ),
        NamelessML3DerivationRule::TrTimes => check_tr_binop(
            derivation,
            NamelessML3DerivationRule::TrTimes,
            NamelessML3BinOp::Times,
        ),
        NamelessML3DerivationRule::TrLt => check_tr_binop(
            derivation,
            NamelessML3DerivationRule::TrLt,
            NamelessML3BinOp::Lt,
        ),
        NamelessML3DerivationRule::TrLet => check_tr_let(derivation),
        NamelessML3DerivationRule::TrFun => check_tr_fun(derivation),
        NamelessML3DerivationRule::TrApp => check_tr_app(derivation),
        NamelessML3DerivationRule::TrLetRec => check_tr_let_rec(derivation),
    }
}

fn check_all_subderivations(subderivations: &[NamelessML3Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &NamelessML3Derivation,
    detail: String,
) -> Result<NamelessML3Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: Tr-Int, Tr-Bool, Tr-Var1, Tr-Var2, Tr-If, Tr-Plus, Tr-Minus, Tr-Times, Tr-Lt, Tr-Let, Tr-Fun, Tr-App, Tr-LetRec; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: NamelessML3DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: NamelessML3DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: NamelessML3DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &NamelessML3Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: NamelessML3DerivationRule,
    expected: &[NamelessML3Judgment],
    actual: &[NamelessML3Judgment],
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

fn check_tr_int(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrInt;
    match &derivation.judgment {
        NamelessML3Judgment::Translates {
            env: _,
            named: NamedExpr::Int(named),
            nameless: NamelessExpr::Int(nameless),
        } if named == nameless => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- i ==> i"),
        ),
    }
}

fn check_tr_bool(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrBool;
    match &derivation.judgment {
        NamelessML3Judgment::Translates {
            env: _,
            named: NamedExpr::Bool(named),
            nameless: NamelessExpr::Bool(nameless),
        } if named == nameless => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- b ==> b"),
        ),
    }
}

fn check_tr_var1(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrVar1;
    let NamelessML3Judgment::Translates {
        env,
        named: NamedExpr::Var(name),
        nameless: NamelessExpr::Index(index),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x |- x ==> #1"),
        );
    };

    match derivation.subderivations.as_slice() {
        [] => {
            if *index == 1 && env.0.last().is_some_and(|last| last == name) {
                Ok(derivation.judgment.clone())
            } else {
                let Some(last) = env.0.last() else {
                    return Err(rule_violation(
                        derivation,
                        wrong_conclusion_form_message(rule, "Gamma, x |- x ==> #1"),
                    ));
                };
                let expected = expected_trans(
                    env.clone(),
                    NamedExpr::Var(last.clone()),
                    NamelessExpr::Index(1),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        std::slice::from_ref(&derivation.judgment),
                        "use Tr-Var1 only when the variable is the nearest binder and the nameless side is #1",
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

fn check_tr_var2(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrVar2;
    let NamelessML3Judgment::Translates {
        env,
        named: NamedExpr::Var(name),
        nameless: NamelessExpr::Index(index),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x |- y ==> #n+1"),
        );
    };

    let Some((prefix, last_name)) = split_last_var(env) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x |- y ==> #n+1"),
        );
    };

    if *index <= 1 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma, x |- y ==> #n+1"),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- y ==> #n", &first),
                ));
            };

            let expected_env = NamelessML3Env(prefix.to_vec());
            let expected_named = NamedExpr::Var(name.clone());
            let expected_nameless = NamelessExpr::Index(index - 1);

            if last_name != name
                && first_env == &expected_env
                && first_named == &expected_named
                && first_nameless == &expected_nameless
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first =
                    expected_trans(expected_env, expected_named, expected_nameless);
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "drop the nearest binder once and increment the de Bruijn index by one",
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

fn check_tr_if(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrIf;
    let NamelessML3Judgment::Translates {
        env,
        named:
            NamedExpr::If {
                condition,
                then_branch,
                else_branch,
            },
        nameless:
            NamelessExpr::If {
                condition: nameless_condition,
                then_branch: nameless_then,
                else_branch: nameless_else,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- if e1 then e2 else e3 ==> if e1' then e2' else e3'",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 ==> e1'", &first),
                ));
            };
            let Some((second_env, second_named, second_nameless)) = as_translates(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 ==> e2'", &second),
                ));
            };
            let Some((third_env, third_named, third_nameless)) = as_translates(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", "Gamma |- e3 ==> e3'", &third),
                ));
            };

            if first_env == env
                && first_named == condition.as_ref()
                && first_nameless == nameless_condition.as_ref()
                && second_env == env
                && second_named == then_branch.as_ref()
                && second_nameless == nameless_then.as_ref()
                && third_env == env
                && third_named == else_branch.as_ref()
                && third_nameless == nameless_else.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    env.clone(),
                    condition.as_ref().clone(),
                    nameless_condition.as_ref().clone(),
                );
                let expected_second = expected_trans(
                    env.clone(),
                    then_branch.as_ref().clone(),
                    nameless_then.as_ref().clone(),
                );
                let expected_third = expected_trans(
                    env.clone(),
                    else_branch.as_ref().clone(),
                    nameless_else.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second, expected_third],
                        &[first, second, third],
                        "translate condition/then/else under the same environment",
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

fn check_tr_binop(
    derivation: &NamelessML3Derivation,
    rule: NamelessML3DerivationRule,
    op: NamelessML3BinOp,
) -> Result<NamelessML3Judgment, CheckError> {
    let NamelessML3Judgment::Translates {
        env,
        named:
            NamedExpr::BinOp {
                op: named_op,
                left: named_left,
                right: named_right,
            },
        nameless:
            NamelessExpr::BinOp {
                op: nameless_op,
                left: nameless_left,
                right: nameless_right,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 ==> e1' op e2'"),
        );
    };

    if *named_op != op || *nameless_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 ==> e1' op e2'"),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 ==> e1'", &first),
                ));
            };
            let Some((second_env, second_named, second_nameless)) = as_translates(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 ==> e2'", &second),
                ));
            };

            if first_env == env
                && first_named == named_left.as_ref()
                && first_nameless == nameless_left.as_ref()
                && second_env == env
                && second_named == named_right.as_ref()
                && second_nameless == nameless_right.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    env.clone(),
                    named_left.as_ref().clone(),
                    nameless_left.as_ref().clone(),
                );
                let expected_second = expected_trans(
                    env.clone(),
                    named_right.as_ref().clone(),
                    nameless_right.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "translate both operands under the same environment",
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

fn check_tr_let(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrLet;
    let NamelessML3Judgment::Translates {
        env,
        named:
            NamedExpr::Let {
                name,
                bound_expr,
                body,
            },
        nameless:
            NamelessExpr::Let {
                bound_expr: nameless_bound,
                body: nameless_body,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let x = e1 in e2 ==> let . = e1' in e2'"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 ==> e1'", &first),
                ));
            };
            let Some((second_env, second_named, second_nameless)) = as_translates(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma, x |- e2 ==> e2'", &second),
                ));
            };

            let expected_second_env = push_var(env, name);
            if first_env == env
                && first_named == bound_expr.as_ref()
                && first_nameless == nameless_bound.as_ref()
                && second_env == &expected_second_env
                && second_named == body.as_ref()
                && second_nameless == nameless_body.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    env.clone(),
                    bound_expr.as_ref().clone(),
                    nameless_bound.as_ref().clone(),
                );
                let expected_second = expected_trans(
                    expected_second_env,
                    body.as_ref().clone(),
                    nameless_body.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "translate e1 under Gamma and e2 under Gamma extended with x",
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

fn check_tr_fun(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrFun;
    let NamelessML3Judgment::Translates {
        env,
        named: NamedExpr::Fun { param, body },
        nameless: NamelessExpr::Fun {
            body: nameless_body,
        },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun x -> e ==> fun . -> e'"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma, x |- e ==> e'", &first),
                ));
            };

            let expected_env = push_var(env, param);
            if first_env == &expected_env
                && first_named == body.as_ref()
                && first_nameless == nameless_body.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    expected_env,
                    body.as_ref().clone(),
                    nameless_body.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "translate function body under Gamma extended with the function parameter",
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

fn check_tr_app(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrApp;
    let NamelessML3Judgment::Translates {
        env,
        named: NamedExpr::App { func, arg },
        nameless:
            NamelessExpr::App {
                func: nameless_func,
                arg: nameless_arg,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 ==> e1' e2'"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma |- e1 ==> e1'", &first),
                ));
            };
            let Some((second_env, second_named, second_nameless)) = as_translates(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma |- e2 ==> e2'", &second),
                ));
            };

            if first_env == env
                && first_named == func.as_ref()
                && first_nameless == nameless_func.as_ref()
                && second_env == env
                && second_named == arg.as_ref()
                && second_nameless == nameless_arg.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    env.clone(),
                    func.as_ref().clone(),
                    nameless_func.as_ref().clone(),
                );
                let expected_second = expected_trans(
                    env.clone(),
                    arg.as_ref().clone(),
                    nameless_arg.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "translate function and argument under the same environment",
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

fn check_tr_let_rec(derivation: &NamelessML3Derivation) -> Result<NamelessML3Judgment, CheckError> {
    let rule = NamelessML3DerivationRule::TrLetRec;
    let NamelessML3Judgment::Translates {
        env,
        named:
            NamedExpr::LetRec {
                name,
                param,
                fun_body,
                body,
            },
        nameless:
            NamelessExpr::LetRec {
                fun_body: nameless_fun_body,
                body: nameless_body,
            },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let rec f = fun x -> e1 in e2 ==> let rec . = fun . -> e1' in e2'",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_named, first_nameless)) = as_translates(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "Gamma, f, x |- e1 ==> e1'", &first),
                ));
            };
            let Some((second_env, second_named, second_nameless)) = as_translates(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "second", "Gamma, f |- e2 ==> e2'", &second),
                ));
            };

            let expected_first_env = push_var(&push_var(env, name), param);
            let expected_second_env = push_var(env, name);
            if first_env == &expected_first_env
                && first_named == fun_body.as_ref()
                && first_nameless == nameless_fun_body.as_ref()
                && second_env == &expected_second_env
                && second_named == body.as_ref()
                && second_nameless == nameless_body.as_ref()
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = expected_trans(
                    expected_first_env,
                    fun_body.as_ref().clone(),
                    nameless_fun_body.as_ref().clone(),
                );
                let expected_second = expected_trans(
                    expected_second_env,
                    body.as_ref().clone(),
                    nameless_body.as_ref().clone(),
                );
                Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "translate recursive function body under Gamma,f,x and the let-rec body under Gamma,f",
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

fn expected_trans(
    env: NamelessML3Env,
    named: NamedExpr,
    nameless: NamelessExpr,
) -> NamelessML3Judgment {
    NamelessML3Judgment::Translates {
        env,
        named,
        nameless,
    }
}

fn as_translates(
    judgment: &NamelessML3Judgment,
) -> Option<(&NamelessML3Env, &NamedExpr, &NamelessExpr)> {
    let NamelessML3Judgment::Translates {
        env,
        named,
        nameless,
    } = judgment;
    Some((env, named, nameless))
}

fn split_last_var(env: &NamelessML3Env) -> Option<(&[String], &String)> {
    let (last, prefix) = env.0.split_last()?;
    Some((prefix, last))
}

fn push_var(env: &NamelessML3Env, name: &str) -> NamelessML3Env {
    let mut vars = env.0.clone();
    vars.push(name.to_string());
    NamelessML3Env(vars)
}

fn rule_violation(derivation: &NamelessML3Derivation, detail: impl Into<String>) -> CheckError {
    let detail = detail.into();
    CheckError::rule_violation(format!("{}: {}", derivation.rule_name, detail))
        .with_span(derivation.span.clone())
}

#[cfg(test)]
mod tests {
    use crate::core::{CheckErrorKind, Game};

    use super::NamelessML3Game;

    #[test]
    fn reports_root_judgment_text_for_all_nameless_ml3_fixtures() {
        let game = NamelessML3Game;
        for source in [
            include_str!("../../../copl/054.copl"),
            include_str!("../../../copl/056.copl"),
            include_str!("../../../copl/058.copl"),
            include_str!("../../../copl/060.copl"),
            include_str!("../../../copl/062.copl"),
            include_str!("../../../copl/064.copl"),
            include_str!("../../../copl/066.copl"),
            include_str!("../../../copl/068.copl"),
        ] {
            let report = game.check(source).expect("fixture should pass");
            assert!(report.summary.contains(" ==> "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 ==> 1 by Tr-Int { |- 1 ==> 1 by Tr-Int {} }";
        let err = NamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("number of premises is wrong"));
    }

    #[test]
    fn reports_rule_violation_for_wrong_rule_application() {
        let source = "x |- x ==> #2 by Tr-Var1 {}";
        let err = NamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application"));
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "|- 1 ==> 1 by Tr-Unknown {}";
        let err = NamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available"));
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
|- let x = 3 in x ==> let . = 3 in #1 by Tr-Let {
  |- 3 ==> 3 by Tr-Int {};
  x |- x ==> #2 by Tr-Var1 {};
}
"#;

        let err = NamelessML3Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violations should carry a span");
        assert_eq!(span.line, 4);
        assert_eq!(span.column, 3);
        assert!(err.message().contains("premise path: 2"));
    }
}
