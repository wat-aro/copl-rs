use std::collections::{HashMap, HashSet};

use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    PolyTypingML4BinOp, PolyTypingML4Binding, PolyTypingML4Derivation, PolyTypingML4Env,
    PolyTypingML4Expr, PolyTypingML4Judgment, PolyTypingML4Type, PolyTypingML4TypeScheme,
};

#[derive(Debug, Clone, Copy)]
enum PolyTypingML4DerivationRule {
    Int,
    Bool,
    Var,
    If,
    Plus,
    Minus,
    Mult,
    Lt,
    Let,
    Abs,
    App,
    LetRec,
    Nil,
    Cons,
    Match,
}

impl PolyTypingML4DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "T-Int" => Some(Self::Int),
            "T-Bool" => Some(Self::Bool),
            "T-Var" => Some(Self::Var),
            "T-If" => Some(Self::If),
            "T-Plus" => Some(Self::Plus),
            "T-Minus" => Some(Self::Minus),
            "T-Mult" | "T-Times" => Some(Self::Mult),
            "T-Lt" => Some(Self::Lt),
            "T-Let" => Some(Self::Let),
            "T-Abs" | "T-Fun" => Some(Self::Abs),
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
            Self::Mult => "T-Mult",
            Self::Lt => "T-Lt",
            Self::Let => "T-Let",
            Self::Abs => "T-Abs",
            Self::App => "T-App",
            Self::LetRec => "T-LetRec",
            Self::Nil => "T-Nil",
            Self::Cons => "T-Cons",
            Self::Match => "T-Match",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct PolyTypingML4Game;

impl Game for PolyTypingML4Game {
    fn kind(&self) -> GameKind {
        GameKind::PolyTypingML4
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
    derivation: &PolyTypingML4Derivation,
) -> Result<PolyTypingML4Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &PolyTypingML4Derivation,
) -> Result<PolyTypingML4Judgment, CheckError> {
    let Some(rule) = PolyTypingML4DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &PolyTypingML4Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &PolyTypingML4Derivation,
    rule: PolyTypingML4DerivationRule,
) -> Result<PolyTypingML4Judgment, CheckError> {
    match rule {
        PolyTypingML4DerivationRule::Int => check_t_int(derivation),
        PolyTypingML4DerivationRule::Bool => check_t_bool(derivation),
        PolyTypingML4DerivationRule::Var => check_t_var(derivation),
        PolyTypingML4DerivationRule::If => check_t_if(derivation),
        PolyTypingML4DerivationRule::Plus => check_t_plus(derivation),
        PolyTypingML4DerivationRule::Minus => check_t_minus(derivation),
        PolyTypingML4DerivationRule::Mult => check_t_mult(derivation),
        PolyTypingML4DerivationRule::Lt => check_t_lt(derivation),
        PolyTypingML4DerivationRule::Let => check_t_let(derivation),
        PolyTypingML4DerivationRule::Abs => check_t_abs(derivation),
        PolyTypingML4DerivationRule::App => check_t_app(derivation),
        PolyTypingML4DerivationRule::LetRec => check_t_let_rec(derivation),
        PolyTypingML4DerivationRule::Nil => check_t_nil(derivation),
        PolyTypingML4DerivationRule::Cons => check_t_cons(derivation),
        PolyTypingML4DerivationRule::Match => check_t_match(derivation),
    }
}

fn check_all_subderivations(subderivations: &[PolyTypingML4Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn infer_subjudgments(
    derivation: &PolyTypingML4Derivation,
) -> Result<Vec<PolyTypingML4Judgment>, CheckError> {
    derivation
        .subderivations
        .iter()
        .map(infer_judgment)
        .collect::<Result<Vec<_>, _>>()
}

fn fail_after_checking_subderivations(
    derivation: &PolyTypingML4Derivation,
    detail: String,
) -> Result<PolyTypingML4Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: T-Int, T-Bool, T-Var, T-If, T-Plus, T-Minus, T-Mult, T-Lt, T-Let, T-Abs, T-App, T-LetRec, T-Nil, T-Cons, T-Match; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: PolyTypingML4DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: PolyTypingML4DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: PolyTypingML4DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &PolyTypingML4Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: PolyTypingML4DerivationRule,
    expected: &[PolyTypingML4Judgment],
    actual: &[PolyTypingML4Judgment],
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

fn check_t_int(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Int;
    match &derivation.judgment {
        PolyTypingML4Judgment::HasType {
            expr: PolyTypingML4Expr::Int(_),
            ty: PolyTypingML4Type::Int,
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

fn check_t_bool(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Bool;
    match &derivation.judgment {
        PolyTypingML4Judgment::HasType {
            expr: PolyTypingML4Expr::Bool(_),
            ty: PolyTypingML4Type::Bool,
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

fn check_t_var(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Var;
    let PolyTypingML4Judgment::HasType {
        env,
        expr: PolyTypingML4Expr::Var(name),
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

    let Some(scheme) = lookup_scheme(env, name) else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected: environment contains '{name} : {ty}', actual environment: {env}; fix: add a matching binding before using T-Var)",
                rule.name(),
            ),
        ));
    };

    if !is_instance_of_scheme(scheme, ty) {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected type: an instance of {scheme}, actual type: {ty}; fix: instantiate the quantified type variables consistently)",
                rule.name(),
            ),
        ));
    }

    Ok(derivation.judgment.clone())
}

fn check_t_if(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::If;
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::If {
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: condition.as_ref().clone(),
            ty: PolyTypingML4Type::Bool,
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: then_branch.as_ref().clone(),
            ty: ty.clone(),
        },
        PolyTypingML4Judgment::HasType {
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
                "type condition as bool and both branches as the same result type",
            ),
        ))
    }
}

fn check_t_plus(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        PolyTypingML4DerivationRule::Plus,
        PolyTypingML4BinOp::Plus,
    )
}

fn check_t_minus(
    derivation: &PolyTypingML4Derivation,
) -> Result<PolyTypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        PolyTypingML4DerivationRule::Minus,
        PolyTypingML4BinOp::Minus,
    )
}

fn check_t_mult(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    check_t_int_binop(
        derivation,
        PolyTypingML4DerivationRule::Mult,
        PolyTypingML4BinOp::Times,
    )
}

fn check_t_int_binop(
    derivation: &PolyTypingML4Derivation,
    rule: PolyTypingML4DerivationRule,
    op: PolyTypingML4BinOp,
) -> Result<PolyTypingML4Judgment, CheckError> {
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::BinOp {
                op: expr_op,
                left,
                right,
            },
        ty: PolyTypingML4Type::Int,
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: left.as_ref().clone(),
            ty: PolyTypingML4Type::Int,
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: right.as_ref().clone(),
            ty: PolyTypingML4Type::Int,
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

fn check_t_lt(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Lt;
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::BinOp {
                op: PolyTypingML4BinOp::Lt,
                left,
                right,
            },
        ty: PolyTypingML4Type::Bool,
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: left.as_ref().clone(),
            ty: PolyTypingML4Type::Int,
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: right.as_ref().clone(),
            ty: PolyTypingML4Type::Int,
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

fn check_t_let(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Let;
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::Let {
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

    let generalized = generalize_type(env, first_ty);

    let expected = vec![
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: bound_expr.as_ref().clone(),
            ty: first_ty.clone(),
        },
        PolyTypingML4Judgment::HasType {
            env: push_binding(env, name, &generalized),
            expr: body.as_ref().clone(),
            ty: ty.clone(),
        },
    ];

    if first_env == env && first_expr == bound_expr.as_ref() && actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "generalize the first premise type over Gamma and bind it in the second premise",
            ),
        ))
    }
}

fn check_t_abs(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Abs;
    let PolyTypingML4Judgment::HasType {
        env,
        expr: PolyTypingML4Expr::Fun { param, body },
        ty: PolyTypingML4Type::Fun {
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

    let expected = vec![PolyTypingML4Judgment::HasType {
        env: push_binding(
            env,
            param,
            &PolyTypingML4TypeScheme::mono(param_ty.as_ref().clone()),
        ),
        expr: body.as_ref().clone(),
        ty: ret.as_ref().clone(),
    }];
    let actual = infer_subjudgments(derivation)?;

    if actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "type function body under Gamma extended with x : t1",
            ),
        ))
    }
}

fn check_t_app(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::App;
    let PolyTypingML4Judgment::HasType {
        env,
        expr: PolyTypingML4Expr::App { func, arg },
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

    let PolyTypingML4Type::Fun { param, ret } = first_ty else {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise type: t1 -> {result_ty}, actual first premise: {}; fix: make the function expression arrow-typed)",
                rule.name(),
                actual[0]
            ),
        ));
    };

    let expected = vec![
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: func.as_ref().clone(),
            ty: PolyTypingML4Type::Fun {
                param: param.clone(),
                ret: ret.clone(),
            },
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: arg.as_ref().clone(),
            ty: param.as_ref().clone(),
        },
    ];

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

fn check_t_let_rec(
    derivation: &PolyTypingML4Derivation,
) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::LetRec;
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::LetRec {
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
    let Some((first_env, first_expr, first_ret_ty)) = as_has_type(&actual[0]) else {
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

    if first_env.0.len() != env.0.len() + 2 {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise environment shape: Gamma, f : t1 -> t2, x : t1; fix: extend Gamma with exactly these two bindings)",
                rule.name(),
            ),
        ));
    }

    if first_env.0[..env.0.len()] != env.0[..] {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected first premise to keep Gamma as prefix; fix: preserve original bindings before f and x)",
                rule.name(),
            ),
        ));
    }

    let rec_binding = &first_env.0[env.0.len()];
    let param_binding = &first_env.0[env.0.len() + 1];
    if param_binding.name != *param {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected parameter binding name: {param}, actual: {}; fix: keep parameter name consistent)",
                rule.name(),
                param_binding.name,
            ),
        ));
    }
    if !param_binding.scheme.is_mono() {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected parameter binding to be monomorphic; fix: use x : t1 in the first premise)",
                rule.name(),
            ),
        ));
    }

    let param_ty = param_binding.scheme.ty.clone();
    let fun_ty = PolyTypingML4Type::Fun {
        param: Box::new(param_ty.clone()),
        ret: Box::new(first_ret_ty.clone()),
    };
    let rec_mono = PolyTypingML4TypeScheme::mono(fun_ty.clone());

    if rec_binding.name != *name || rec_binding.scheme != rec_mono {
        return Err(rule_violation(
            derivation,
            format!(
                "Wrong rule application: {} (expected recursive binding: {name} : {rec_mono}; actual: {} : {}; fix: type f as t1 -> t2 in the first premise)",
                rule.name(),
                rec_binding.name,
                rec_binding.scheme,
            ),
        ));
    }

    let expected_first = PolyTypingML4Judgment::HasType {
        env: push_binding(
            &push_binding(env, name, &rec_mono),
            param,
            &PolyTypingML4TypeScheme::mono(param_ty),
        ),
        expr: fun_body.as_ref().clone(),
        ty: first_ret_ty.clone(),
    };

    let generalized = generalize_type(env, &fun_ty);
    let expected_second = PolyTypingML4Judgment::HasType {
        env: push_binding(env, name, &generalized),
        expr: body.as_ref().clone(),
        ty: body_ty.clone(),
    };

    let expected = vec![expected_first, expected_second];

    if first_expr == fun_body.as_ref() && actual == expected {
        Ok(derivation.judgment.clone())
    } else {
        Err(rule_violation(
            derivation,
            wrong_rule_application_message(
                rule,
                &expected,
                &actual,
                "derive body of f under monomorphic f/x, then generalize f for the second premise",
            ),
        ))
    }
}

fn check_t_nil(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Nil;
    match &derivation.judgment {
        PolyTypingML4Judgment::HasType {
            expr: PolyTypingML4Expr::Nil,
            ty: PolyTypingML4Type::List(_),
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

fn check_t_cons(derivation: &PolyTypingML4Derivation) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Cons;
    let PolyTypingML4Judgment::HasType {
        env,
        expr: PolyTypingML4Expr::Cons { head, tail },
        ty: PolyTypingML4Type::List(elem_ty),
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: head.as_ref().clone(),
            ty: elem_ty.as_ref().clone(),
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: tail.as_ref().clone(),
            ty: PolyTypingML4Type::List(Box::new(elem_ty.as_ref().clone())),
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

fn check_t_match(
    derivation: &PolyTypingML4Derivation,
) -> Result<PolyTypingML4Judgment, CheckError> {
    let rule = PolyTypingML4DerivationRule::Match;
    let PolyTypingML4Judgment::HasType {
        env,
        expr:
            PolyTypingML4Expr::Match {
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

    let PolyTypingML4Type::List(elem_ty) = first_ty else {
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
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: scrutinee.as_ref().clone(),
            ty: PolyTypingML4Type::List(Box::new(elem_ty.as_ref().clone())),
        },
        PolyTypingML4Judgment::HasType {
            env: env.clone(),
            expr: nil_case.as_ref().clone(),
            ty: result_ty.clone(),
        },
        PolyTypingML4Judgment::HasType {
            env: push_binding(
                &push_binding(
                    env,
                    head_name,
                    &PolyTypingML4TypeScheme::mono(elem_ty.as_ref().clone()),
                ),
                tail_name,
                &PolyTypingML4TypeScheme::mono(PolyTypingML4Type::List(Box::new(
                    elem_ty.as_ref().clone(),
                ))),
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
    judgment: &PolyTypingML4Judgment,
) -> Option<(&PolyTypingML4Env, &PolyTypingML4Expr, &PolyTypingML4Type)> {
    let PolyTypingML4Judgment::HasType { env, expr, ty } = judgment;
    Some((env, expr, ty))
}

fn lookup_scheme<'a>(env: &'a PolyTypingML4Env, name: &str) -> Option<&'a PolyTypingML4TypeScheme> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.scheme)
}

fn push_binding(
    env: &PolyTypingML4Env,
    name: &str,
    scheme: &PolyTypingML4TypeScheme,
) -> PolyTypingML4Env {
    let mut bindings = env.0.clone();
    bindings.push(PolyTypingML4Binding {
        name: name.to_string(),
        scheme: scheme.clone(),
    });
    PolyTypingML4Env(bindings)
}

fn is_instance_of_scheme(scheme: &PolyTypingML4TypeScheme, target: &PolyTypingML4Type) -> bool {
    let quantified = scheme.quantified.iter().cloned().collect::<HashSet<_>>();
    let mut subst = HashMap::<String, PolyTypingML4Type>::new();
    match_type_instance(&scheme.ty, target, &quantified, &mut subst)
}

fn match_type_instance(
    pattern: &PolyTypingML4Type,
    target: &PolyTypingML4Type,
    quantified: &HashSet<String>,
    subst: &mut HashMap<String, PolyTypingML4Type>,
) -> bool {
    match pattern {
        PolyTypingML4Type::Int => matches!(target, PolyTypingML4Type::Int),
        PolyTypingML4Type::Bool => matches!(target, PolyTypingML4Type::Bool),
        PolyTypingML4Type::Var(name) => {
            if quantified.contains(name) {
                if let Some(bound) = subst.get(name) {
                    bound == target
                } else {
                    subst.insert(name.clone(), target.clone());
                    true
                }
            } else {
                matches!(target, PolyTypingML4Type::Var(other) if other == name)
            }
        }
        PolyTypingML4Type::List(inner) => {
            let PolyTypingML4Type::List(target_inner) = target else {
                return false;
            };
            match_type_instance(inner, target_inner, quantified, subst)
        }
        PolyTypingML4Type::Fun { param, ret } => {
            let PolyTypingML4Type::Fun {
                param: target_param,
                ret: target_ret,
            } = target
            else {
                return false;
            };
            match_type_instance(param, target_param, quantified, subst)
                && match_type_instance(ret, target_ret, quantified, subst)
        }
    }
}

fn generalize_type(env: &PolyTypingML4Env, ty: &PolyTypingML4Type) -> PolyTypingML4TypeScheme {
    let env_free = free_type_vars_in_env(env);
    let mut ordered = Vec::new();
    collect_type_vars_in_order(ty, &mut ordered);

    let mut quantified = Vec::new();
    for name in ordered {
        if env_free.contains(&name) || quantified.contains(&name) {
            continue;
        }
        quantified.push(name);
    }

    PolyTypingML4TypeScheme {
        quantified,
        ty: ty.clone(),
    }
}

fn free_type_vars_in_env(env: &PolyTypingML4Env) -> HashSet<String> {
    let mut set = HashSet::new();
    for binding in &env.0 {
        let mut scheme_set = HashSet::new();
        collect_type_vars_to_set(&binding.scheme.ty, &mut scheme_set);
        for quantified in &binding.scheme.quantified {
            scheme_set.remove(quantified);
        }
        set.extend(scheme_set);
    }
    set
}

fn collect_type_vars_in_order(ty: &PolyTypingML4Type, out: &mut Vec<String>) {
    match ty {
        PolyTypingML4Type::Int | PolyTypingML4Type::Bool => {}
        PolyTypingML4Type::Var(name) => out.push(name.clone()),
        PolyTypingML4Type::List(inner) => collect_type_vars_in_order(inner, out),
        PolyTypingML4Type::Fun { param, ret } => {
            collect_type_vars_in_order(param, out);
            collect_type_vars_in_order(ret, out);
        }
    }
}

fn collect_type_vars_to_set(ty: &PolyTypingML4Type, out: &mut HashSet<String>) {
    match ty {
        PolyTypingML4Type::Int | PolyTypingML4Type::Bool => {}
        PolyTypingML4Type::Var(name) => {
            out.insert(name.clone());
        }
        PolyTypingML4Type::List(inner) => collect_type_vars_to_set(inner, out),
        PolyTypingML4Type::Fun { param, ret } => {
            collect_type_vars_to_set(param, out);
            collect_type_vars_to_set(ret, out);
        }
    }
}

fn rule_violation(derivation: &PolyTypingML4Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::PolyTypingML4Game;

    #[test]
    fn reports_root_judgment_text_for_all_poly_typing_ml4_fixtures() {
        let game = PolyTypingML4Game;
        for source in [
            include_str!("../../../copl/107.copl"),
            include_str!("../../../copl/108.copl"),
            include_str!("../../../copl/109.copl"),
            include_str!("../../../copl/110.copl"),
            include_str!("../../../copl/111.copl"),
            include_str!("../../../copl/112.copl"),
            include_str!("../../../copl/113.copl"),
            include_str!("../../../copl/114.copl"),
            include_str!("../../../copl/115.copl"),
            include_str!("../../../copl/116.copl"),
            include_str!("../../../copl/117.copl"),
            include_str!("../../../copl/118.copl"),
            include_str!("../../../copl/119.copl"),
            include_str!("../../../copl/120.copl"),
            include_str!("../../../copl/121.copl"),
            include_str!("../../../copl/122.copl"),
            include_str!("../../../copl/123.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains(" : "));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 : int by T-Int { |- 2 : int by T-Int {} }";
        let err = PolyTypingML4Game
            .check(source)
            .expect_err("check should fail");
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
        let source = "f : 'a.'a -> 'a |- f : bool by T-Var {}";
        let err = PolyTypingML4Game
            .check(source)
            .expect_err("check should fail");
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
        let err = PolyTypingML4Game
            .check(source)
            .expect_err("check should fail");
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
        let err = PolyTypingML4Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
