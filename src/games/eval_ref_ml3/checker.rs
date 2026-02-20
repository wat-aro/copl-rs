use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalRefML3Binding, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr, EvalRefML3Judgment,
    EvalRefML3Store, EvalRefML3Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalRefML3DerivationRule {
    Int,
    Unit,
    Loc,
    Var,
    Let,
    Ref,
    Deref,
    Assign,
}

impl EvalRefML3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::Int),
            "E-Unit" => Some(Self::Unit),
            "E-Loc" => Some(Self::Loc),
            "E-Var" => Some(Self::Var),
            "E-Let" => Some(Self::Let),
            "E-Ref" => Some(Self::Ref),
            "E-Deref" => Some(Self::Deref),
            "E-Assign" => Some(Self::Assign),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::Int => "E-Int",
            Self::Unit => "E-Unit",
            Self::Loc => "E-Loc",
            Self::Var => "E-Var",
            Self::Let => "E-Let",
            Self::Ref => "E-Ref",
            Self::Deref => "E-Deref",
            Self::Assign => "E-Assign",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalRefML3Game;

impl Game for EvalRefML3Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalRefML3
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

fn infer_judgment(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalRefML3Derivation,
) -> Result<EvalRefML3Judgment, CheckError> {
    let Some(rule) = EvalRefML3DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalRefML3Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalRefML3Derivation,
    rule: EvalRefML3DerivationRule,
) -> Result<EvalRefML3Judgment, CheckError> {
    match rule {
        EvalRefML3DerivationRule::Int => check_e_int(derivation),
        EvalRefML3DerivationRule::Unit => check_e_unit(derivation),
        EvalRefML3DerivationRule::Loc => check_e_loc(derivation),
        EvalRefML3DerivationRule::Var => check_e_var(derivation),
        EvalRefML3DerivationRule::Let => check_e_let(derivation),
        EvalRefML3DerivationRule::Ref => check_e_ref(derivation),
        EvalRefML3DerivationRule::Deref => check_e_deref(derivation),
        EvalRefML3DerivationRule::Assign => check_e_assign(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalRefML3Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalRefML3Derivation,
    detail: String,
) -> Result<EvalRefML3Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Unit, E-Loc, E-Var, E-Let, E-Ref, E-Deref, E-Assign; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalRefML3DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: EvalRefML3DerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name()
    )
}

fn wrong_premise_form_message(
    rule: EvalRefML3DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalRefML3Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalRefML3DerivationRule,
    expected: &[EvalRefML3Judgment],
    actual: &[EvalRefML3Judgment],
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

fn check_e_int(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Int;
    let Some((_, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- i / sigma evalto i / sigma"),
        );
    };

    match (expr, value, store == result_store) {
        (EvalRefML3Expr::Int(expr_int), EvalRefML3Value::Int(value_int), true)
            if expr_int == value_int =>
        {
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
            wrong_conclusion_form_message(rule, "Gamma |- i / sigma evalto i / sigma"),
        ),
    }
}

fn check_e_unit(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Unit;
    let Some((_, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- () / sigma evalto () / sigma"),
        );
    };

    match (expr, value, store == result_store) {
        (EvalRefML3Expr::Unit, EvalRefML3Value::Unit, true) => {
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
            wrong_conclusion_form_message(rule, "Gamma |- () / sigma evalto () / sigma"),
        ),
    }
}

fn check_e_loc(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Loc;
    let Some((_, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- @l / sigma evalto @l / sigma"),
        );
    };

    let (EvalRefML3Expr::Loc(expr_loc), EvalRefML3Value::Loc(value_loc), true) =
        (expr, value, store == result_store)
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- @l / sigma evalto @l / sigma"),
        );
    };

    if expr_loc != value_loc {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- @l / sigma evalto @l / sigma"),
        );
    }

    if !store.contains(expr_loc) {
        return fail_after_checking_subderivations(
            derivation,
            format!(
                "Wrong rule application: {} (expected @{} to exist in sigma, actual: sigma = {}; fix: use an existing location or add the location to sigma)",
                rule.name(),
                expr_loc,
                store
            ),
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

fn check_e_var(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Var;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x / sigma evalto v / sigma"),
        );
    };

    let EvalRefML3Expr::Var(name) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x / sigma evalto v / sigma"),
        );
    };

    if store != result_store {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x / sigma evalto v / sigma"),
        );
    }

    let Some(binding) = env.0.iter().rev().find(|binding| binding.name == *name) else {
        return fail_after_checking_subderivations(
            derivation,
            format!(
                "Wrong rule application: {} (expected variable `{name}` to be bound in Gamma, actual: {env}; fix: add `{name}` to Gamma or replace the expression)",
                rule.name()
            ),
        );
    };

    if &binding.value != value {
        return fail_after_checking_subderivations(
            derivation,
            format!(
                "Wrong rule application: {} (expected value: {}, actual: {}; fix: replace the conclusion value with the bound value)",
                rule.name(),
                binding.value,
                value
            ),
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

fn check_e_let(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Let;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let x = e1 in e2 / sigma evalto v2 / sigma2",
            ),
        );
    };

    let EvalRefML3Expr::Let {
        name,
        bound_expr,
        body,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let x = e1 in e2 / sigma evalto v2 / sigma2",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto v1 / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((_, _, _, second_value, second_result_store)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma, x = v1 |- e2 / sigma1 evalto v2 / sigma2",
                        &second,
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                bound_expr.as_ref().clone(),
                store.clone(),
                first_value.clone(),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                extend_env(env, name, first_value.clone()),
                body.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected = [expected_first.clone(), expected_second.clone()];
            let actual = [first.clone(), second.clone()];

            let is_valid = actual[0] == expected_first
                && actual[1] == expected_second
                && value == second_value
                && result_store == second_result_store;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "ensure the first premise evaluates e1, the second premise evaluates e2 under Gamma, x = v1 and sigma1, and the conclusion forwards v2 / sigma2",
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

fn check_e_ref(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Ref;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- ref e / sigma evalto @l / sigma1, @l = v",
            ),
        );
    };

    let EvalRefML3Expr::Ref { expr: inner } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- ref e / sigma evalto @l / sigma1, @l = v",
            ),
        );
    };
    let EvalRefML3Value::Loc(location) = value else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- ref e / sigma evalto @l / sigma1, @l = v",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e / sigma evalto v / sigma1",
                        &first,
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                inner.as_ref().clone(),
                store.clone(),
                first_value.clone(),
                first_result_store.clone(),
            );
            if first != expected_first {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "ensure the premise evaluates the operand `e` under the same Gamma and sigma as the conclusion",
                    ),
                ));
            }

            if first_result_store.contains(location) {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected @{} to be fresh for sigma1 = {}; fix: choose a location name not present in sigma1)",
                        rule.name(),
                        location,
                        first_result_store
                    ),
                ));
            }

            let expected_store =
                first_result_store.with_appended(location.clone(), first_value.clone());
            if result_store != &expected_store {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected result store: {}, actual: {}; fix: append @{} = {} to sigma1)",
                        rule.name(),
                        expected_store,
                        result_store,
                        location,
                        first_value
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

fn check_e_deref(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Deref;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- !e / sigma evalto v / sigma1"),
        );
    };

    let EvalRefML3Expr::Deref { expr: inner } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- !e / sigma evalto v / sigma1"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e / sigma evalto @l / sigma1",
                        &first,
                    ),
                ));
            };
            let EvalRefML3Value::Loc(location) = first_value else {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[eval_to_judgment(
                            env.clone(),
                            inner.as_ref().clone(),
                            store.clone(),
                            EvalRefML3Value::Loc("l".to_string()),
                            first_result_store.clone(),
                        )],
                        &[first],
                        "ensure the premise evaluates to a location value",
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                inner.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Loc(location.clone()),
                first_result_store.clone(),
            );
            if first != expected_first {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "ensure the premise evaluates the operand `e` under the same Gamma and sigma as the conclusion",
                    ),
                ));
            }

            let Some(expected_value) = first_result_store.lookup(location) else {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected @{} to exist in sigma1 = {}; fix: evaluate to an existing location)",
                        rule.name(),
                        location,
                        first_result_store
                    ),
                ));
            };

            if value != expected_value || result_store != first_result_store {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected conclusion: {} |- !{} / {} evalto {} / {}, actual: {}; fix: load the value at @{} and keep sigma1 unchanged)",
                        rule.name(),
                        env,
                        inner,
                        store,
                        expected_value,
                        first_result_store,
                        derivation.judgment,
                        location
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

fn check_e_assign(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Assign;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 := e2 / sigma evalto () / sigma2"),
        );
    };

    let EvalRefML3Expr::Assign { target, value: rhs } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 := e2 / sigma evalto () / sigma2"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto @l / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((_, _, _, second_value, second_result_store)) = as_eval_to(&second) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma |- e2 / sigma1 evalto v / sigma2",
                        &second,
                    ),
                ));
            };
            let EvalRefML3Value::Loc(location) = first_value else {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[eval_to_judgment(
                            env.clone(),
                            target.as_ref().clone(),
                            store.clone(),
                            EvalRefML3Value::Loc("l".to_string()),
                            first_result_store.clone(),
                        )],
                        &[first],
                        "ensure the first premise evaluates assignment target e1 to a location value",
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                target.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Loc(location.clone()),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                rhs.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected = [expected_first.clone(), expected_second.clone()];
            let actual = [first.clone(), second.clone()];
            if actual[0] != expected_first || actual[1] != expected_second {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "ensure the first premise evaluates e1 to @l and the second premise evaluates e2 from sigma1",
                    ),
                ));
            }

            let Some(updated_store) =
                second_result_store.with_updated(location, second_value.clone())
            else {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected @{} to exist in sigma2 = {}; fix: evaluate e1 to an existing location)",
                        rule.name(),
                        location,
                        second_result_store
                    ),
                ));
            };

            if value != &EvalRefML3Value::Unit || result_store != &updated_store {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected conclusion: {} |- {} := {} / {} evalto () / {}, actual: {}; fix: update @{} in sigma2 and set result value to ())",
                        rule.name(),
                        env,
                        target,
                        rhs,
                        store,
                        updated_store,
                        derivation.judgment,
                        location
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

fn as_eval_to(
    judgment: &EvalRefML3Judgment,
) -> Option<(
    &EvalRefML3Env,
    &EvalRefML3Expr,
    &EvalRefML3Store,
    &EvalRefML3Value,
    &EvalRefML3Store,
)> {
    let EvalRefML3Judgment::EvalTo {
        env,
        expr,
        store,
        value,
        result_store,
    } = judgment;
    Some((env, expr, store, value, result_store))
}

fn eval_to_judgment(
    env: EvalRefML3Env,
    expr: EvalRefML3Expr,
    store: EvalRefML3Store,
    value: EvalRefML3Value,
    result_store: EvalRefML3Store,
) -> EvalRefML3Judgment {
    EvalRefML3Judgment::EvalTo {
        env,
        expr,
        store,
        value,
        result_store,
    }
}

fn extend_env(env: &EvalRefML3Env, name: &str, value: EvalRefML3Value) -> EvalRefML3Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalRefML3Binding {
        name: name.to_string(),
        value,
    });
    EvalRefML3Env(bindings)
}

fn rule_violation(derivation: &EvalRefML3Derivation, detail: impl Into<String>) -> CheckError {
    let detail = detail.into();
    CheckError::rule_violation(format!(
        "{detail}: {} by {}",
        derivation.judgment, derivation.rule_name
    ))
    .with_span(derivation.span.clone())
}

#[cfg(test)]
mod tests {
    use super::EvalRefML3Game;
    use crate::core::{CheckErrorKind, Game};

    #[test]
    fn accepts_valid_leaf_derivation() {
        let source = "|- 1 / () evalto 1 / () by E-Int {}";
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- 1 / () evalto 1 / ()");
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 / () evalto 1 / () by E-Int { |- 1 / () evalto 1 / () by E-Int {} }";
        let err = EvalRefML3Game
            .check(source)
            .expect_err("derivation should be invalid");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Int"));
        assert!(err.message().contains("premise path: root"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn rejects_unknown_rule_name() {
        let source = "|- 1 / () evalto 1 / () by E-Unknown {}";
        let err = EvalRefML3Game
            .check(source)
            .expect_err("derivation should be invalid");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err
            .message()
            .contains("available: E-Int, E-Unit, E-Loc, E-Var, E-Let, E-Ref, E-Deref, E-Assign"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn accepts_non_canonical_fresh_location_for_e_ref() {
        let source =
            "|- ref 1 / () evalto @l1 / @l1 = 1 by E-Ref { |- 1 / () evalto 1 / () by E-Int {} }";
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- ref 1 / () evalto @l1 / @l1 = 1");
    }

    #[test]
    fn reports_rule_violation_with_premise_path_for_nested_failure() {
        let source = r#"
|- let x = 1 in x / () evalto 1 / () by E-Let {
  |- 1 / () evalto 1 / () by E-Int {};
  x = 1 |- x / () evalto 1 / () by E-Unknown {}
}
"#;
        let err = EvalRefML3Game
            .check(source)
            .expect_err("derivation should be invalid");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 2"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 4);
        assert_eq!(span.column, 3);
    }
}
