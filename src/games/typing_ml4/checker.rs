use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    TypingML4BinOp, TypingML4Binding, TypingML4Derivation, TypingML4Env, TypingML4Expr,
    TypingML4Judgment, TypingML4Type,
};

#[derive(Debug, Clone, Copy)]
enum TypingML4DerivationRule {
    Int,
    Bool,
    Var,
    If,
    Plus,
    Minus,
    Times,
    Lt,
    Let,
    Fun,
    App,
    LetRec,
    Nil,
    Cons,
    Match,
}

impl TypingML4DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "T-Int" => Some(Self::Int),
            "T-Bool" => Some(Self::Bool),
            "T-Var" => Some(Self::Var),
            "T-If" => Some(Self::If),
            "T-Plus" => Some(Self::Plus),
            "T-Minus" => Some(Self::Minus),
            "T-Times" => Some(Self::Times),
            "T-Lt" => Some(Self::Lt),
            "T-Let" => Some(Self::Let),
            "T-Fun" => Some(Self::Fun),
            "T-App" => Some(Self::App),
            "T-LetRec" => Some(Self::LetRec),
            "T-Nil" => Some(Self::Nil),
            "T-Cons" => Some(Self::Cons),
            "T-Match" => Some(Self::Match),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::Int => "T-Int",
            Self::Bool => "T-Bool",
            Self::Var => "T-Var",
            Self::If => "T-If",
            Self::Plus => "T-Plus",
            Self::Minus => "T-Minus",
            Self::Times => "T-Times",
            Self::Lt => "T-Lt",
            Self::Let => "T-Let",
            Self::Fun => "T-Fun",
            Self::App => "T-App",
            Self::LetRec => "T-LetRec",
            Self::Nil => "T-Nil",
            Self::Cons => "T-Cons",
            Self::Match => "T-Match",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TypingML4Game;

impl Game for TypingML4Game {
    fn kind(&self) -> GameKind {
        GameKind::TypingML4
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

fn infer_judgment(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let Some(rule) = TypingML4DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &TypingML4Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &TypingML4Derivation,
    rule: TypingML4DerivationRule,
) -> Result<TypingML4Judgment, CheckError> {
    match rule {
        TypingML4DerivationRule::Int => check_t_int(derivation),
        TypingML4DerivationRule::Bool => check_t_bool(derivation),
        TypingML4DerivationRule::Var => check_t_var(derivation),
        TypingML4DerivationRule::If => check_t_if(derivation),
        TypingML4DerivationRule::Plus => check_t_plus(derivation),
        TypingML4DerivationRule::Minus => check_t_minus(derivation),
        TypingML4DerivationRule::Times => check_t_times(derivation),
        TypingML4DerivationRule::Lt => check_t_lt(derivation),
        TypingML4DerivationRule::Let => check_t_let(derivation),
        TypingML4DerivationRule::Fun => check_t_fun(derivation),
        TypingML4DerivationRule::App => check_t_app(derivation),
        TypingML4DerivationRule::LetRec => check_t_let_rec(derivation),
        TypingML4DerivationRule::Nil => check_t_nil(derivation),
        TypingML4DerivationRule::Cons => check_t_cons(derivation),
        TypingML4DerivationRule::Match => check_t_match(derivation),
    }
}

fn check_all_subderivations(subderivations: &[TypingML4Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn infer_subjudgments(
    derivation: &TypingML4Derivation,
) -> Result<Vec<TypingML4Judgment>, CheckError> {
    derivation
        .subderivations
        .iter()
        .map(infer_judgment)
        .collect::<Result<Vec<_>, _>>()
}

fn fail_after_checking_subderivations(
    derivation: &TypingML4Derivation,
    detail: String,
) -> Result<TypingML4Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: T-Int, T-Bool, T-Var, T-If, T-Plus, T-Minus, T-Times, T-Lt, T-Let, T-Fun, T-App, T-LetRec, T-Nil, T-Cons, T-Match; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: TypingML4DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(rule: TypingML4DerivationRule, expected: &'static str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: TypingML4DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &TypingML4Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: TypingML4DerivationRule,
    expected: &[TypingML4Judgment],
    actual: &[TypingML4Judgment],
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

fn check_t_int(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Int;
    match &derivation.judgment {
        TypingML4Judgment::HasType {
            expr: TypingML4Expr::Int(_),
            ty: TypingML4Type::Int,
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
            wrong_conclusion_form_message(rule, "Gamma |- n : int"),
        ),
    }
}

fn check_t_bool(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Bool;
    match &derivation.judgment {
        TypingML4Judgment::HasType {
            expr: TypingML4Expr::Bool(_),
            ty: TypingML4Type::Bool,
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
            wrong_conclusion_form_message(rule, "Gamma |- b : bool"),
        ),
    }
}

fn check_t_var(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Var;
    let TypingML4Judgment::HasType {
        env,
        expr: TypingML4Expr::Var(name),
        ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x : t"),
        );
    };

    if !derivation.subderivations.is_empty() {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
        );
    }

    let Some(found) = lookup_type(env, name) else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected: environment contains '{name} : {ty}', actual environment: {env}; fix: add a matching binding or use T-Var with the bound type)",
                rule.name(),
            ),
        ));
    };

    if found != ty {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected type: {found}, actual type: {ty}; fix: make the conclusion type match the variable binding)",
                rule.name(),
            ),
        ));
    }

    Ok(derivation.judgment.clone())
}

fn check_t_if(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::If;
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::If {
                condition,
                then_branch,
                else_branch,
            },
        ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- if e1 then e2 else e3 : t"),
        );
    };

    if derivation.subderivations.len() != 3 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let expected = vec![
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: condition.as_ref().clone(),
            ty: TypingML4Type::Bool,
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: then_branch.as_ref().clone(),
            ty: ty.clone(),
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: else_branch.as_ref().clone(),
            ty: ty.clone(),
        },
    ];

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "check the condition type (bool) and branch result type alignment",
            ),
        ))
    }
}

fn check_t_plus(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        TypingML4DerivationRule::Plus,
        TypingML4BinOp::Plus,
    )
}

fn check_t_minus(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        TypingML4DerivationRule::Minus,
        TypingML4BinOp::Minus,
    )
}

fn check_t_times(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        TypingML4DerivationRule::Times,
        TypingML4BinOp::Times,
    )
}

fn check_t_int_binop(
    derivation: &TypingML4Derivation,
    rule: TypingML4DerivationRule,
    op: TypingML4BinOp,
) -> Result<TypingML4Judgment, CheckError> {
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        ty: TypingML4Type::Int,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 : int"),
        );
    };

    if *expr_op != op {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 : int"),
        );
    }

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let expected = vec![
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: left.as_ref().clone(),
            ty: TypingML4Type::Int,
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: right.as_ref().clone(),
            ty: TypingML4Type::Int,
        },
    ];

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "ensure both operands are typed as int",
            ),
        ))
    }
}

fn check_t_lt(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Lt;
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::BinOp {
                op: TypingML4BinOp::Lt,
                left,
                right,
            },
        ty: TypingML4Type::Bool,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 < e2 : bool"),
        );
    };

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let expected = vec![
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: left.as_ref().clone(),
            ty: TypingML4Type::Int,
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: right.as_ref().clone(),
            ty: TypingML4Type::Int,
        },
    ];

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "ensure both operands are typed as int",
            ),
        ))
    }
}

fn check_t_let(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Let;
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::Let {
                name,
                bound_expr,
                body,
            },
        ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let x = e1 in e2 : t2"),
        );
    };

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let Some((first_env, first_expr, first_ty)) = as_has_type(&actual[0]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "first", "Gamma |- e1 : t1", &actual[0]),
        ));
    };
    let Some((_second_env, _second_expr, _second_ty)) = as_has_type(&actual[1]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "second", "Gamma, x : t1 |- e2 : t2", &actual[1]),
        ));
    };

    let expected_first = TypingML4Judgment::HasType {
        env: env.clone(),
        expr: bound_expr.as_ref().clone(),
        ty: first_ty.clone(),
    };
    let expected_second = TypingML4Judgment::HasType {
        env: push_binding(env, name, first_ty),
        expr: body.as_ref().clone(),
        ty: ty.clone(),
    };
    let expected = vec![expected_first, expected_second];

    if first_env == env && first_expr == bound_expr.as_ref() && actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "type e1 first and extend Gamma with x : t1 only for the second premise",
            ),
        ))
    }
}

fn check_t_fun(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Fun;
    let TypingML4Judgment::HasType {
        env,
        expr: TypingML4Expr::Fun { param, body },
        ty: TypingML4Type::Fun {
            param: param_ty,
            ret,
        },
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun x -> e : t1 -> t2"),
        );
    };

    if derivation.subderivations.len() != 1 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let expected = vec![TypingML4Judgment::HasType {
        env: push_binding(env, param, param_ty),
        expr: body.as_ref().clone(),
        ty: ret.as_ref().clone(),
    }];

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "extend Gamma with x : t1 when typing the function body",
            ),
        ))
    }
}

fn check_t_app(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::App;
    let TypingML4Judgment::HasType {
        env,
        expr: TypingML4Expr::App { func, arg },
        ty: result_ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 : t2"),
        );
    };

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let Some((first_env, first_expr, first_ty)) = as_has_type(&actual[0]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "first", "Gamma |- e1 : t1 -> t2", &actual[0]),
        ));
    };
    let Some((_second_env, _second_expr, _second_ty)) = as_has_type(&actual[1]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "second", "Gamma |- e2 : t1", &actual[1]),
        ));
    };

    let TypingML4Type::Fun { param, ret } = first_ty else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise type: t1 -> {result_ty}, actual first premise: {}; fix: type the function expression as an arrow type ending in the conclusion type)",
                rule.name(),
                actual[0]
            ),
        ));
    };

    let expected_first = TypingML4Judgment::HasType {
        env: env.clone(),
        expr: func.as_ref().clone(),
        ty: TypingML4Type::Fun {
            param: param.clone(),
            ret: ret.clone(),
        },
    };
    let expected_second = TypingML4Judgment::HasType {
        env: env.clone(),
        expr: arg.as_ref().clone(),
        ty: param.as_ref().clone(),
    };
    let expected = vec![expected_first, expected_second];

    if first_env == env
        && first_expr == func.as_ref()
        && ret.as_ref() == result_ty
        && actual == expected
    {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "ensure first premise has type t1 -> t2 and second premise has type t1",
            ),
        ))
    }
}

fn check_t_let_rec(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::LetRec;
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::LetRec {
                name,
                param,
                fun_body,
                body,
            },
        ty: body_ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let rec f = fun x -> e1 in e2 : t"),
        );
    };

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let Some((first_env, first_expr, first_ty)) = as_has_type(&actual[0]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(
                rule,
                "first",
                "Gamma, f : t1 -> t2, x : t1 |- e1 : t2",
                &actual[0],
            ),
        ));
    };
    let Some((_second_env, _second_expr, _second_ty)) = as_has_type(&actual[1]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "second", "Gamma, f : t1 -> t2 |- e2 : t", &actual[1]),
        ));
    };

    let Some(prefix_len) = first_env.0.len().checked_sub(2) else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise environment: Gamma, {name} : t1 -> t2, {param} : t1; fix: extend the environment in this order)",
                rule.name(),
            ),
        ));
    };

    let prefix = &first_env.0[..prefix_len];
    let rec_binding = &first_env.0[prefix_len];
    let param_binding = &first_env.0[prefix_len + 1];

    let fun_ty = TypingML4Type::Fun {
        param: Box::new(param_binding.ty.clone()),
        ret: Box::new(first_ty.clone()),
    };

    let expected_first_env =
        push_binding(&push_binding(env, name, &fun_ty), param, &param_binding.ty);
    let expected_first = TypingML4Judgment::HasType {
        env: expected_first_env,
        expr: fun_body.as_ref().clone(),
        ty: first_ty.clone(),
    };

    let expected_second = TypingML4Judgment::HasType {
        env: push_binding(env, name, &fun_ty),
        expr: body.as_ref().clone(),
        ty: body_ty.clone(),
    };

    let expected = vec![expected_first, expected_second];

    if prefix == env.0.as_slice()
        && rec_binding.name == *name
        && rec_binding.ty == fun_ty
        && param_binding.name == *param
        && first_expr == fun_body.as_ref()
        && actual == expected
    {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "derive e1 under Gamma, f : t1 -> t2, x : t1 and e2 under Gamma, f : t1 -> t2",
            ),
        ))
    }
}

fn check_t_nil(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Nil;
    match &derivation.judgment {
        TypingML4Judgment::HasType {
            expr: TypingML4Expr::Nil,
            ty: TypingML4Type::List(_),
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
            wrong_conclusion_form_message(rule, "Gamma |- [] : t list"),
        ),
    }
}

fn check_t_cons(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Cons;
    let TypingML4Judgment::HasType {
        env,
        expr: TypingML4Expr::Cons { head, tail },
        ty: TypingML4Type::List(elem_ty),
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 :: e2 : t list"),
        );
    };

    if derivation.subderivations.len() != 2 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 2, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;
    let expected = vec![
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: head.as_ref().clone(),
            ty: elem_ty.as_ref().clone(),
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: tail.as_ref().clone(),
            ty: TypingML4Type::List(Box::new(elem_ty.as_ref().clone())),
        },
    ];

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "type the head as t and the tail as t list",
            ),
        ))
    }
}

fn check_t_match(derivation: &TypingML4Derivation) -> Result<TypingML4Judgment, CheckError> {
    let rule = TypingML4DerivationRule::Match;
    let TypingML4Judgment::HasType {
        env,
        expr:
            TypingML4Expr::Match {
                scrutinee,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            },
        ty: result_ty,
    } = &derivation.judgment
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- match e0 with [] -> e1 | x :: y -> e2 : t",
            ),
        );
    };

    if derivation.subderivations.len() != 3 {
        return fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 3, derivation.subderivations.len()),
        );
    }

    let actual = infer_subjudgments(derivation)?;

    let Some((first_env, first_expr, first_ty)) = as_has_type(&actual[0]) else {
        return Err(rule_violation(
            derivation,
            wrong_premise_form_message(rule, "first", "Gamma |- e0 : t1 list", &actual[0]),
        ));
    };

    let TypingML4Type::List(elem_ty) = first_ty else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise type: t1 list, actual first premise: {}; fix: type the scrutinee as a list)",
                rule.name(),
                actual[0]
            ),
        ));
    };

    let expected = vec![
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: scrutinee.as_ref().clone(),
            ty: TypingML4Type::List(Box::new(elem_ty.as_ref().clone())),
        },
        TypingML4Judgment::HasType {
            env: env.clone(),
            expr: nil_case.as_ref().clone(),
            ty: result_ty.clone(),
        },
        TypingML4Judgment::HasType {
            env: push_binding(
                &push_binding(env, head_name, elem_ty),
                tail_name,
                &TypingML4Type::List(Box::new(elem_ty.as_ref().clone())),
            ),
            expr: cons_case.as_ref().clone(),
            ty: result_ty.clone(),
        },
    ];

    if first_env == env && first_expr == scrutinee.as_ref() && actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "align scrutinee/list element type and both branch result types",
            ),
        ))
    }
}

fn as_has_type(
    judgment: &TypingML4Judgment,
) -> Option<(&TypingML4Env, &TypingML4Expr, &TypingML4Type)> {
    let TypingML4Judgment::HasType { env, expr, ty } = judgment;
    Some((env, expr, ty))
}

fn lookup_type<'a>(env: &'a TypingML4Env, name: &str) -> Option<&'a TypingML4Type> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.ty)
}

fn push_binding(env: &TypingML4Env, name: &str, ty: &TypingML4Type) -> TypingML4Env {
    let mut bindings = env.0.clone();
    bindings.push(TypingML4Binding {
        name: name.to_string(),
        ty: ty.clone(),
    });
    TypingML4Env(bindings)
}

fn rule_violation(derivation: &TypingML4Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::TypingML4Game;

    #[test]
    fn reports_root_judgment_text_for_all_typing_ml4_fixtures() {
        let game = TypingML4Game;
        for source in [
            include_str!("../../../copl/080.copl"),
            include_str!("../../../copl/081.copl"),
            include_str!("../../../copl/082.copl"),
            include_str!("../../../copl/083.copl"),
            include_str!("../../../copl/084.copl"),
            include_str!("../../../copl/085.copl"),
            include_str!("../../../copl/086.copl"),
            include_str!("../../../copl/087.copl"),
            include_str!("../../../copl/088.copl"),
            include_str!("../../../copl/089.copl"),
            include_str!("../../../copl/090.copl"),
            include_str!("../../../copl/091.copl"),
            include_str!("../../../copl/092.copl"),
            include_str!("../../../copl/093.copl"),
            include_str!("../../../copl/094.copl"),
            include_str!("../../../copl/095.copl"),
            include_str!("../../../copl/096.copl"),
            include_str!("../../../copl/097.copl"),
            include_str!("../../../copl/098.copl"),
            include_str!("../../../copl/099.copl"),
            include_str!("../../../copl/100.copl"),
            include_str!("../../../copl/101.copl"),
            include_str!("../../../copl/102.copl"),
            include_str!("../../../copl/103.copl"),
            include_str!("../../../copl/104.copl"),
            include_str!("../../../copl/105.copl"),
            include_str!("../../../copl/106.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains(" : "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 : int by T-Int { |- 2 : int by T-Int {} }";
        let err = TypingML4Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: T-Int"));
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
        let source = "x : int |- x : bool by T-Var {}";
        let err = TypingML4Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("Wrong rule application: T-Var"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "|- 1 : int by T-Unknown {}";
        let err = TypingML4Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: T-Int, T-Bool, T-Var"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
|- if true then 1 else 2 : int by T-If {
  |- true : bool by T-Unknown {};
  |- 1 : int by T-Int {};
  |- 2 : int by T-Int {};
}
"#;
        let err = TypingML4Game.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
