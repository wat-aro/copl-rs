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
    Bool,
    Unit,
    Loc,
    Var,
    IfT,
    IfF,
    Let,
    LetRec,
    Fun,
    App,
    AppRec,
    Plus,
    Minus,
    Times,
    Lt,
    BPlus,
    BMinus,
    BTimes,
    BLt,
    Ref,
    Deref,
    Assign,
}

impl EvalRefML3DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::Int),
            "E-Bool" => Some(Self::Bool),
            "E-Unit" => Some(Self::Unit),
            "E-Loc" => Some(Self::Loc),
            "E-Var" => Some(Self::Var),
            "E-IfT" => Some(Self::IfT),
            "E-IfF" => Some(Self::IfF),
            "E-Let" => Some(Self::Let),
            "E-LetRec" => Some(Self::LetRec),
            "E-Fun" => Some(Self::Fun),
            "E-App" => Some(Self::App),
            "E-AppRec" => Some(Self::AppRec),
            "E-Plus" => Some(Self::Plus),
            "E-Minus" => Some(Self::Minus),
            "E-Times" => Some(Self::Times),
            "E-Lt" => Some(Self::Lt),
            "B-Plus" => Some(Self::BPlus),
            "B-Minus" => Some(Self::BMinus),
            "B-Times" => Some(Self::BTimes),
            "B-Lt" => Some(Self::BLt),
            "E-Ref" => Some(Self::Ref),
            "E-Deref" => Some(Self::Deref),
            "E-Assign" => Some(Self::Assign),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::Int => "E-Int",
            Self::Bool => "E-Bool",
            Self::Unit => "E-Unit",
            Self::Loc => "E-Loc",
            Self::Var => "E-Var",
            Self::IfT => "E-IfT",
            Self::IfF => "E-IfF",
            Self::Let => "E-Let",
            Self::LetRec => "E-LetRec",
            Self::Fun => "E-Fun",
            Self::App => "E-App",
            Self::AppRec => "E-AppRec",
            Self::Plus => "E-Plus",
            Self::Minus => "E-Minus",
            Self::Times => "E-Times",
            Self::Lt => "E-Lt",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
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
        EvalRefML3DerivationRule::Bool => check_e_bool(derivation),
        EvalRefML3DerivationRule::Unit => check_e_unit(derivation),
        EvalRefML3DerivationRule::Loc => check_e_loc(derivation),
        EvalRefML3DerivationRule::Var => check_e_var(derivation),
        EvalRefML3DerivationRule::IfT => check_e_if_t(derivation),
        EvalRefML3DerivationRule::IfF => check_e_if_f(derivation),
        EvalRefML3DerivationRule::Let => check_e_let(derivation),
        EvalRefML3DerivationRule::LetRec => check_e_let_rec(derivation),
        EvalRefML3DerivationRule::Fun => check_e_fun(derivation),
        EvalRefML3DerivationRule::App => check_e_app(derivation),
        EvalRefML3DerivationRule::AppRec => check_e_app_rec(derivation),
        EvalRefML3DerivationRule::Plus => check_e_plus(derivation),
        EvalRefML3DerivationRule::Minus => check_e_minus(derivation),
        EvalRefML3DerivationRule::Times => check_e_times(derivation),
        EvalRefML3DerivationRule::Lt => check_e_lt(derivation),
        EvalRefML3DerivationRule::BPlus => check_b_plus(derivation),
        EvalRefML3DerivationRule::BMinus => check_b_minus(derivation),
        EvalRefML3DerivationRule::BTimes => check_b_times(derivation),
        EvalRefML3DerivationRule::BLt => check_b_lt(derivation),
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
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-Unit, E-Loc, E-Var, E-IfT, E-IfF, E-Let, E-LetRec, E-Fun, E-App, E-AppRec, E-Plus, E-Minus, E-Times, E-Lt, B-Plus, B-Minus, B-Times, B-Lt, E-Ref, E-Deref, E-Assign; fix: replace the rule name after 'by')"
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

fn check_e_bool(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Bool;
    let Some((_, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- b / sigma evalto b / sigma"),
        );
    };

    match (expr, value, store == result_store) {
        (EvalRefML3Expr::Bool(expr_bool), EvalRefML3Value::Bool(value_bool), true)
            if expr_bool == value_bool =>
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
            wrong_conclusion_form_message(rule, "Gamma |- b / sigma evalto b / sigma"),
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

fn check_e_if_t(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::IfT;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- if e1 then e2 else e3 / sigma evalto v / sigma2",
            ),
        );
    };

    let EvalRefML3Expr::If {
        condition,
        then_branch,
        else_branch: _,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- if e1 then e2 else e3 / sigma evalto v / sigma2",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_expr, first_store, first_bool, first_result_store)) =
                as_eval_to_bool(&first)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto true / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((second_env, second_expr, second_store, second_value, second_result_store)) =
                as_eval_to(&second)
            else {
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

            let expected_first = eval_to_judgment(
                env.clone(),
                condition.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Bool(true),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                then_branch.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected = [expected_first.clone(), expected_second.clone()];
            let actual = [first.clone(), second.clone()];
            let is_valid = first_env == env
                && first_expr == condition.as_ref()
                && first_store == store
                && first_bool
                && second_env == env
                && second_expr == then_branch.as_ref()
                && second_store == first_result_store
                && value == second_value
                && result_store == second_result_store
                && actual[0] == expected_first
                && actual[1] == expected_second;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1 first to true, thread sigma1 into e2, and forward e2's value/store to the conclusion",
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

fn check_e_if_f(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::IfF;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- if e1 then e2 else e3 / sigma evalto v / sigma2",
            ),
        );
    };

    let EvalRefML3Expr::If {
        condition,
        then_branch: _,
        else_branch,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- if e1 then e2 else e3 / sigma evalto v / sigma2",
            ),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;

            let Some((first_env, first_expr, first_store, first_bool, first_result_store)) =
                as_eval_to_bool(&first)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto false / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((second_env, second_expr, second_store, second_value, second_result_store)) =
                as_eval_to(&second)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma |- e3 / sigma1 evalto v / sigma2",
                        &second,
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                condition.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Bool(false),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                else_branch.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected = [expected_first.clone(), expected_second.clone()];
            let actual = [first.clone(), second.clone()];
            let is_valid = first_env == env
                && first_expr == condition.as_ref()
                && first_store == store
                && !first_bool
                && second_env == env
                && second_expr == else_branch.as_ref()
                && second_store == first_result_store
                && value == second_value
                && result_store == second_result_store
                && actual[0] == expected_first
                && actual[1] == expected_second;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1 first to false, thread sigma1 into e3, and forward e3's value/store to the conclusion",
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

fn check_e_let_rec(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::LetRec;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let rec f = fun x -> e1 in e2 / sigma evalto v / sigma1",
            ),
        );
    };

    let EvalRefML3Expr::LetRec {
        name,
        param,
        fun_body,
        body,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let rec f = fun x -> e1 in e2 / sigma evalto v / sigma1",
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
                        "Gamma, f = (Gamma)[rec f = fun x -> e1] |- e2 / sigma evalto v / sigma1",
                        &first,
                    ),
                ));
            };

            let recursive_value = EvalRefML3Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let expected_first = eval_to_judgment(
                extend_env(env, name, recursive_value),
                body.as_ref().clone(),
                store.clone(),
                first_value.clone(),
                first_result_store.clone(),
            );
            if first != expected_first || value != first_value || result_store != first_result_store
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first],
                        &[first],
                        "extend Gamma with recursive closure f, evaluate e2 under the same sigma, and forward e2's value/store",
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

fn check_e_fun(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Fun;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- fun x -> e / sigma evalto (Gamma)[fun x -> e] / sigma",
            ),
        );
    };

    let EvalRefML3Expr::Fun { param, body } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- fun x -> e / sigma evalto (Gamma)[fun x -> e] / sigma",
            ),
        );
    };

    let expected_value = EvalRefML3Value::Closure {
        env: env.clone(),
        param: param.clone(),
        body: body.as_ref().clone(),
    };

    match derivation.subderivations.as_slice() {
        [] if value == &expected_value && store == result_store => Ok(derivation.judgment.clone()),
        [] => Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &[eval_to_judgment(
                    env.clone(),
                    expr.clone(),
                    store.clone(),
                    expected_value,
                    store.clone(),
                )],
                std::slice::from_ref(&derivation.judgment),
                "closure must capture Gamma, preserve function syntax, and keep sigma unchanged",
            ),
        )),
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        ),
    }
}

fn check_e_app(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::App;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 / sigma evalto v / sigma3"),
        );
    };

    let EvalRefML3Expr::App { func, arg } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 / sigma evalto v / sigma3"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto (Gamma')[fun x -> e0] / sigma1",
                        &first,
                    ),
                ));
            };
            let EvalRefML3Value::Closure {
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
                        "Gamma |- e1 / sigma evalto (Gamma')[fun x -> e0] / sigma1",
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
                        "Gamma |- e2 / sigma1 evalto v2 / sigma2",
                        &second,
                    ),
                ));
            };
            let Some((_, _, _, third_value, third_result_store)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "Gamma', x = v2 |- e0 / sigma2 evalto v / sigma3",
                        &third,
                    ),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                func.as_ref().clone(),
                store.clone(),
                first_value.clone(),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                arg.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected_third = eval_to_judgment(
                extend_env(closure_env, param, second_value.clone()),
                body.clone(),
                second_result_store.clone(),
                third_value.clone(),
                third_result_store.clone(),
            );
            let expected = [
                expected_first.clone(),
                expected_second.clone(),
                expected_third.clone(),
            ];
            let actual = [first.clone(), second.clone(), third.clone()];
            let is_valid = actual[0] == expected_first
                && actual[1] == expected_second
                && actual[2] == expected_third
                && value == third_value
                && result_store == third_result_store;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1, thread sigma1 to e2, then evaluate closure body under captured Gamma' extended with x = v2 from sigma2",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_e_app_rec(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::AppRec;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 / sigma evalto v / sigma3"),
        );
    };

    let EvalRefML3Expr::App { func, arg } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 / sigma evalto v / sigma3"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((_, _, _, first_value, first_result_store)) = as_eval_to(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto (Gamma')[rec f = fun x -> e0] / sigma1",
                        &first,
                    ),
                ));
            };
            let EvalRefML3Value::RecClosure {
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
                        "Gamma |- e1 / sigma evalto (Gamma')[rec f = fun x -> e0] / sigma1",
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
                        "Gamma |- e2 / sigma1 evalto v2 / sigma2",
                        &second,
                    ),
                ));
            };
            let Some((_, _, _, third_value, third_result_store)) = as_eval_to(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "Gamma', f = (Gamma')[rec f = fun x -> e0], x = v2 |- e0 / sigma2 evalto v / sigma3",
                        &third,
                    ),
                ));
            };

            let recursive_value = first_value.clone();
            let expected_first = eval_to_judgment(
                env.clone(),
                func.as_ref().clone(),
                store.clone(),
                first_value.clone(),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                arg.as_ref().clone(),
                first_result_store.clone(),
                second_value.clone(),
                second_result_store.clone(),
            );
            let expected_third = eval_to_judgment(
                extend_env(
                    &extend_env(closure_env, name, recursive_value),
                    param,
                    second_value.clone(),
                ),
                body.clone(),
                second_result_store.clone(),
                third_value.clone(),
                third_result_store.clone(),
            );
            let expected = [
                expected_first.clone(),
                expected_second.clone(),
                expected_third.clone(),
            ];
            let actual = [first.clone(), second.clone(), third.clone()];
            let is_valid = actual[0] == expected_first
                && actual[1] == expected_second
                && actual[2] == expected_third
                && value == third_value
                && result_store == third_result_store;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1/e2 with store threading, then evaluate recursive body under Gamma' extended with f and x = v2 from sigma2",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_e_plus(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalRefML3DerivationRule::Plus,
        "Gamma |- e1 + e2 / sigma evalto i3 / sigma2",
        as_plus_is,
        "i1 plus i2 is i3",
    )
}

fn check_e_minus(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalRefML3DerivationRule::Minus,
        "Gamma |- e1 - e2 / sigma evalto i3 / sigma2",
        as_minus_is,
        "i1 minus i2 is i3",
    )
}

fn check_e_times(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    check_e_arith_binop(
        derivation,
        EvalRefML3DerivationRule::Times,
        "Gamma |- e1 * e2 / sigma evalto i3 / sigma2",
        as_times_is,
        "i1 times i2 is i3",
    )
}

fn check_e_arith_binop(
    derivation: &EvalRefML3Derivation,
    rule: EvalRefML3DerivationRule,
    conclusion_shape: &'static str,
    as_arith_judgment: fn(&EvalRefML3Judgment) -> Option<(i64, i64, i64)>,
    third_shape: &'static str,
) -> Result<EvalRefML3Judgment, CheckError> {
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, conclusion_shape),
        );
    };
    let EvalRefML3Expr::BinOp { op, left, right } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, conclusion_shape),
        );
    };
    let expected_op = match rule {
        EvalRefML3DerivationRule::Plus => "+",
        EvalRefML3DerivationRule::Minus => "-",
        EvalRefML3DerivationRule::Times => "*",
        _ => unreachable!("only arithmetic rules are allowed"),
    };
    let matches_op = matches!(
        (rule, op),
        (
            EvalRefML3DerivationRule::Plus,
            super::syntax::EvalRefML3BinOp::Plus
        ) | (
            EvalRefML3DerivationRule::Minus,
            super::syntax::EvalRefML3BinOp::Minus
        ) | (
            EvalRefML3DerivationRule::Times,
            super::syntax::EvalRefML3BinOp::Times
        )
    );
    if !matches_op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                match expected_op {
                    "+" => "Gamma |- e1 + e2 / sigma evalto i3 / sigma2",
                    "-" => "Gamma |- e1 - e2 / sigma evalto i3 / sigma2",
                    "*" => "Gamma |- e1 * e2 / sigma evalto i3 / sigma2",
                    _ => unreachable!(),
                },
            ),
        );
    }
    let EvalRefML3Value::Int(result) = value else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, conclusion_shape),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((_, _, _, first_int, first_result_store)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto i1 / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((_, _, second_store, second_int, second_result_store)) =
                as_eval_to_int(&second)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma |- e2 / sigma1 evalto i2 / sigma2",
                        &second,
                    ),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_arith_judgment(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", third_shape, &third),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                left.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Int(first_int),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                right.as_ref().clone(),
                first_result_store.clone(),
                EvalRefML3Value::Int(second_int),
                second_result_store.clone(),
            );
            let expected_third = match rule {
                EvalRefML3DerivationRule::Plus => EvalRefML3Judgment::PlusIs {
                    left: third_left,
                    right: third_right,
                    result: *result,
                },
                EvalRefML3DerivationRule::Minus => EvalRefML3Judgment::MinusIs {
                    left: third_left,
                    right: third_right,
                    result: *result,
                },
                EvalRefML3DerivationRule::Times => EvalRefML3Judgment::TimesIs {
                    left: third_left,
                    right: third_right,
                    result: *result,
                },
                _ => unreachable!("only arithmetic rules are allowed"),
            };
            let expected = [
                expected_first.clone(),
                expected_second.clone(),
                expected_third.clone(),
            ];
            let actual = [first.clone(), second.clone(), third.clone()];

            let is_valid = second_store == first_result_store
                && actual[0] == expected_first
                && actual[1] == expected_second
                && actual[2] == expected_third
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
                && result_store == second_result_store;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1 then e2 with store threading, and keep i1/i2/i3 consistent with the B-* premise and the conclusion",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_e_lt(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::Lt;
    let Some((env, expr, store, value, result_store)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 / sigma evalto b3 / sigma2"),
        );
    };
    let EvalRefML3Expr::BinOp {
        op: super::syntax::EvalRefML3BinOp::Lt,
        left,
        right,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 / sigma evalto b3 / sigma2"),
        );
    };
    let EvalRefML3Value::Bool(result) = value else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 / sigma evalto b3 / sigma2"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1, d2, d3] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            let third = infer_judgment(d3)?;

            let Some((_, _, _, first_int, first_result_store)) = as_eval_to_int(&first) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "Gamma |- e1 / sigma evalto i1 / sigma1",
                        &first,
                    ),
                ));
            };
            let Some((_, _, second_store, second_int, second_result_store)) =
                as_eval_to_int(&second)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "Gamma |- e2 / sigma1 evalto i2 / sigma2",
                        &second,
                    ),
                ));
            };
            let Some((third_left, third_right, third_result)) = as_less_than_is(&third) else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "third", "i1 less than i2 is b3", &third),
                ));
            };

            let expected_first = eval_to_judgment(
                env.clone(),
                left.as_ref().clone(),
                store.clone(),
                EvalRefML3Value::Int(first_int),
                first_result_store.clone(),
            );
            let expected_second = eval_to_judgment(
                env.clone(),
                right.as_ref().clone(),
                first_result_store.clone(),
                EvalRefML3Value::Int(second_int),
                second_result_store.clone(),
            );
            let expected_third = EvalRefML3Judgment::LessThanIs {
                left: third_left,
                right: third_right,
                result: *result,
            };
            let expected = [
                expected_first.clone(),
                expected_second.clone(),
                expected_third.clone(),
            ];
            let actual = [first.clone(), second.clone(), third.clone()];
            let is_valid = second_store == first_result_store
                && actual[0] == expected_first
                && actual[1] == expected_second
                && actual[2] == expected_third
                && first_int == third_left
                && second_int == third_right
                && *result == third_result
                && result_store == second_result_store;
            if !is_valid {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &expected,
                        &actual,
                        "evaluate e1 then e2 with store threading, and keep i1/i2/b3 consistent with the B-Lt premise and the conclusion",
                    ),
                ));
            }

            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        ),
    }
}

fn check_b_plus(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::BPlus;
    match &derivation.judgment {
        EvalRefML3Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::BMinus;
    match &derivation.judgment {
        EvalRefML3Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::BTimes;
    match &derivation.judgment {
        EvalRefML3Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalRefML3Derivation) -> Result<EvalRefML3Judgment, CheckError> {
    let rule = EvalRefML3DerivationRule::BLt;
    match &derivation.judgment {
        EvalRefML3Judgment::LessThanIs {
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
            wrong_conclusion_form_message(rule, "Gamma |- e1 := e2 / sigma evalto v / sigma3"),
        );
    };

    let EvalRefML3Expr::Assign { target, value: rhs } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 := e2 / sigma evalto v / sigma3"),
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

            if value != second_value || result_store != &updated_store {
                return Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected conclusion: {} |- {} := {} / {} evalto {} / {}, actual: {}; fix: update @{} in sigma2 and forward e2 result value)",
                        rule.name(),
                        env,
                        target,
                        rhs,
                        store,
                        second_value,
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
    match judgment {
        EvalRefML3Judgment::EvalTo {
            env,
            expr,
            store,
            value,
            result_store,
        } => Some((env, expr, store, value, result_store)),
        _ => None,
    }
}

fn as_eval_to_int(
    judgment: &EvalRefML3Judgment,
) -> Option<(
    &EvalRefML3Env,
    &EvalRefML3Expr,
    &EvalRefML3Store,
    i64,
    &EvalRefML3Store,
)> {
    let (env, expr, store, value, result_store) = as_eval_to(judgment)?;
    let EvalRefML3Value::Int(value_int) = value else {
        return None;
    };
    Some((env, expr, store, *value_int, result_store))
}

fn as_eval_to_bool(
    judgment: &EvalRefML3Judgment,
) -> Option<(
    &EvalRefML3Env,
    &EvalRefML3Expr,
    &EvalRefML3Store,
    bool,
    &EvalRefML3Store,
)> {
    let (env, expr, store, value, result_store) = as_eval_to(judgment)?;
    let EvalRefML3Value::Bool(value_bool) = value else {
        return None;
    };
    Some((env, expr, store, *value_bool, result_store))
}

fn as_plus_is(judgment: &EvalRefML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalRefML3Judgment::PlusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_minus_is(judgment: &EvalRefML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalRefML3Judgment::MinusIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_times_is(judgment: &EvalRefML3Judgment) -> Option<(i64, i64, i64)> {
    let EvalRefML3Judgment::TimesIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
}

fn as_less_than_is(judgment: &EvalRefML3Judgment) -> Option<(i64, i64, bool)> {
    let EvalRefML3Judgment::LessThanIs {
        left,
        right,
        result,
    } = judgment
    else {
        return None;
    };
    Some((*left, *right, *result))
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
        let source = "|- 1 evalto 1 by E-Int {}";
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- 1 evalto 1");
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
        assert!(err.message().contains(
            "available: E-Int, E-Bool, E-Unit, E-Loc, E-Var, E-IfT, E-IfF, E-Let, E-LetRec, E-Fun, E-App, E-AppRec, E-Plus, E-Minus, E-Times, E-Lt, B-Plus, B-Minus, B-Times, B-Lt, E-Ref, E-Deref, E-Assign"
        ));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn accepts_non_canonical_fresh_location_for_e_ref() {
        let source = "|- ref 1 evalto @l1 / @l1 = 1 by E-Ref { |- 1 evalto 1 by E-Int {} }";
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- ref 1 evalto @l1 / @l1 = 1");
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

    #[test]
    fn accepts_e_assign_that_returns_rhs_value() {
        let source = r#"
@l = 2 / x = @l |- x := 3 evalto 3 / @l = 3 by E-Assign {
  @l = 2 / x = @l |- x evalto @l / @l = 2 by E-Var {};
  @l = 2 / x = @l |- 3 evalto 3 / @l = 2 by E-Int {}
}
"#;
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(
            report.summary,
            "@l = 2 / x = @l |- x := 3 evalto 3 / @l = 3"
        );
    }

    #[test]
    fn accepts_e_app_rec_derivation() {
        let source = r#"
|- let rec f = fun x -> x in f 1 evalto 1 by E-LetRec {
  f = ()[rec f = fun x -> x] |- f 1 evalto 1 by E-AppRec {
    f = ()[rec f = fun x -> x] |- f evalto ()[rec f = fun x -> x] by E-Var {};
    f = ()[rec f = fun x -> x] |- 1 evalto 1 by E-Int {};
    f = ()[rec f = fun x -> x], x = 1 |- x evalto 1 by E-Var {}
  }
}
"#;
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- let rec f = fun x -> x in f 1 evalto 1");
    }

    #[test]
    fn accepts_minus_times_and_lt_derivations() {
        let game = EvalRefML3Game;
        for source in [
            r#"
|- 5 - 2 / () evalto 3 / () by E-Minus {
  |- 5 / () evalto 5 / () by E-Int {};
  |- 2 / () evalto 2 / () by E-Int {};
  5 minus 2 is 3 by B-Minus {}
}
"#,
            r#"
|- 2 * 3 / () evalto 6 / () by E-Times {
  |- 2 / () evalto 2 / () by E-Int {};
  |- 3 / () evalto 3 / () by E-Int {};
  2 times 3 is 6 by B-Times {}
}
"#,
            r#"
|- 1 < 2 / () evalto true / () by E-Lt {
  |- 1 / () evalto 1 / () by E-Int {};
  |- 2 / () evalto 2 / () by E-Int {};
  1 less than 2 is true by B-Lt {}
}
"#,
        ] {
            game.check(source).expect("derivation should be valid");
        }
    }

    #[test]
    fn reports_root_judgment_text_for_eval_ref_ml3_fixtures_141_to_145() {
        let game = EvalRefML3Game;
        for source in [
            include_str!("../../../copl/141.copl"),
            include_str!("../../../copl/142.copl"),
            include_str!("../../../copl/143.copl"),
            include_str!("../../../copl/144.copl"),
            include_str!("../../../copl/145.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(!report.summary.is_empty());
            assert!(report.summary.contains(" evalto "));
        }
    }
}
