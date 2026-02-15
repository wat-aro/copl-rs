use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{
    EvalContML4BinOp, EvalContML4Binding, EvalContML4ContFrame, EvalContML4Continuation,
    EvalContML4Derivation, EvalContML4Env, EvalContML4Expr, EvalContML4Judgment, EvalContML4Value,
};

#[derive(Debug, Clone, Copy)]
enum EvalContML4DerivationRule {
    EInt,
    EBool,
    EVar,
    EFun,
    ELet,
    ELetRec,
    ELetCc,
    EBinOp,
    EIf,
    EApp,
    ENil,
    ECons,
    EMatch,
    CRet,
    CEvalR,
    CPlus,
    CMinus,
    CTimes,
    CLt,
    CIfT,
    CIfF,
    CLetBody,
    CEvalArg,
    CEvalFun,
    CEvalFunR,
    CEvalFunC,
    CEvalConsR,
    CCons,
    CMatchNil,
    CMatchCons,
    BPlus,
    BMinus,
    BTimes,
    BLt,
}

impl EvalContML4DerivationRule {
    fn parse(rule_name: &str) -> Option<Self> {
        match rule_name {
            "E-Int" => Some(Self::EInt),
            "E-Bool" => Some(Self::EBool),
            "E-Var" => Some(Self::EVar),
            "E-Fun" => Some(Self::EFun),
            "E-Let" => Some(Self::ELet),
            "E-LetRec" => Some(Self::ELetRec),
            "E-LetCc" => Some(Self::ELetCc),
            "E-BinOp" => Some(Self::EBinOp),
            "E-If" => Some(Self::EIf),
            "E-App" => Some(Self::EApp),
            "E-Nil" => Some(Self::ENil),
            "E-Cons" => Some(Self::ECons),
            "E-Match" => Some(Self::EMatch),
            "C-Ret" => Some(Self::CRet),
            "C-EvalR" => Some(Self::CEvalR),
            "C-Plus" => Some(Self::CPlus),
            "C-Minus" => Some(Self::CMinus),
            "C-Times" => Some(Self::CTimes),
            "C-Lt" => Some(Self::CLt),
            "C-IfT" => Some(Self::CIfT),
            "C-IfF" => Some(Self::CIfF),
            "C-LetBody" => Some(Self::CLetBody),
            "C-EvalArg" => Some(Self::CEvalArg),
            "C-EvalFun" => Some(Self::CEvalFun),
            "C-EvalFunR" => Some(Self::CEvalFunR),
            "C-EvalFunC" => Some(Self::CEvalFunC),
            "C-EvalConsR" => Some(Self::CEvalConsR),
            "C-Cons" => Some(Self::CCons),
            "C-MatchNil" => Some(Self::CMatchNil),
            "C-MatchCons" => Some(Self::CMatchCons),
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
            Self::EFun => "E-Fun",
            Self::ELet => "E-Let",
            Self::ELetRec => "E-LetRec",
            Self::ELetCc => "E-LetCc",
            Self::EBinOp => "E-BinOp",
            Self::EIf => "E-If",
            Self::EApp => "E-App",
            Self::ENil => "E-Nil",
            Self::ECons => "E-Cons",
            Self::EMatch => "E-Match",
            Self::CRet => "C-Ret",
            Self::CEvalR => "C-EvalR",
            Self::CPlus => "C-Plus",
            Self::CMinus => "C-Minus",
            Self::CTimes => "C-Times",
            Self::CLt => "C-Lt",
            Self::CIfT => "C-IfT",
            Self::CIfF => "C-IfF",
            Self::CLetBody => "C-LetBody",
            Self::CEvalArg => "C-EvalArg",
            Self::CEvalFun => "C-EvalFun",
            Self::CEvalFunR => "C-EvalFunR",
            Self::CEvalFunC => "C-EvalFunC",
            Self::CEvalConsR => "C-EvalConsR",
            Self::CCons => "C-Cons",
            Self::CMatchNil => "C-MatchNil",
            Self::CMatchCons => "C-MatchCons",
            Self::BPlus => "B-Plus",
            Self::BMinus => "B-Minus",
            Self::BTimes => "B-Times",
            Self::BLt => "B-Lt",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalContML4Game;

impl Game for EvalContML4Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalContML4
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

fn infer_judgment(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let Some(rule) = EvalContML4DerivationRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };
    check_rule_application(derivation, rule)
}

fn ensure_error_has_span(err: CheckError, derivation: &EvalContML4Derivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_rule_application(
    derivation: &EvalContML4Derivation,
    rule: EvalContML4DerivationRule,
) -> Result<EvalContML4Judgment, CheckError> {
    match rule {
        EvalContML4DerivationRule::EInt => check_e_int(derivation),
        EvalContML4DerivationRule::EBool => check_e_bool(derivation),
        EvalContML4DerivationRule::EVar => check_e_var(derivation),
        EvalContML4DerivationRule::EFun => check_e_fun(derivation),
        EvalContML4DerivationRule::ELet => check_e_let(derivation),
        EvalContML4DerivationRule::ELetRec => check_e_let_rec(derivation),
        EvalContML4DerivationRule::ELetCc => check_e_let_cc(derivation),
        EvalContML4DerivationRule::EBinOp => check_e_binop(derivation),
        EvalContML4DerivationRule::EIf => check_e_if(derivation),
        EvalContML4DerivationRule::EApp => check_e_app(derivation),
        EvalContML4DerivationRule::ENil => check_e_nil(derivation),
        EvalContML4DerivationRule::ECons => check_e_cons(derivation),
        EvalContML4DerivationRule::EMatch => check_e_match(derivation),
        EvalContML4DerivationRule::CRet => check_c_ret(derivation),
        EvalContML4DerivationRule::CEvalR => check_c_eval_r(derivation),
        EvalContML4DerivationRule::CPlus => check_c_plus(derivation),
        EvalContML4DerivationRule::CMinus => check_c_minus(derivation),
        EvalContML4DerivationRule::CTimes => check_c_times(derivation),
        EvalContML4DerivationRule::CLt => check_c_lt(derivation),
        EvalContML4DerivationRule::CIfT => check_c_if_t(derivation),
        EvalContML4DerivationRule::CIfF => check_c_if_f(derivation),
        EvalContML4DerivationRule::CLetBody => check_c_let_body(derivation),
        EvalContML4DerivationRule::CEvalArg => check_c_eval_arg(derivation),
        EvalContML4DerivationRule::CEvalFun => check_c_eval_fun(derivation),
        EvalContML4DerivationRule::CEvalFunR => check_c_eval_fun_r(derivation),
        EvalContML4DerivationRule::CEvalFunC => check_c_eval_fun_c(derivation),
        EvalContML4DerivationRule::CEvalConsR => check_c_eval_cons_r(derivation),
        EvalContML4DerivationRule::CCons => check_c_cons(derivation),
        EvalContML4DerivationRule::CMatchNil => check_c_match_nil(derivation),
        EvalContML4DerivationRule::CMatchCons => check_c_match_cons(derivation),
        EvalContML4DerivationRule::BPlus => check_b_plus(derivation),
        EvalContML4DerivationRule::BMinus => check_b_minus(derivation),
        EvalContML4DerivationRule::BTimes => check_b_times(derivation),
        EvalContML4DerivationRule::BLt => check_b_lt(derivation),
    }
}

fn check_all_subderivations(subderivations: &[EvalContML4Derivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &EvalContML4Derivation,
    detail: String,
) -> Result<EvalContML4Judgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: E-Int, E-Bool, E-Var, E-Fun, E-Let, E-LetRec, E-LetCc, E-BinOp, E-If, E-App, E-Nil, E-Cons, E-Match, C-Ret, C-EvalR, C-Plus, C-Minus, C-Times, C-Lt, C-IfT, C-IfF, C-LetBody, C-EvalArg, C-EvalFun, C-EvalFunR, C-EvalFunC, C-EvalConsR, C-Cons, C-MatchNil, C-MatchCons, B-Plus, B-Minus, B-Times, B-Lt; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(
    rule: EvalContML4DerivationRule,
    expected: usize,
    actual: usize,
) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name()
    )
}

fn wrong_conclusion_form_message(
    rule: EvalContML4DerivationRule,
    expected: &'static str,
) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: EvalContML4DerivationRule,
    ordinal: &'static str,
    expected: &'static str,
    actual: &EvalContML4Judgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match the expected shape)",
        rule.name(),
    )
}

fn wrong_rule_application_message(
    rule: EvalContML4DerivationRule,
    expected: &[EvalContML4Judgment],
    actual: &[EvalContML4Judgment],
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

fn check_unary_value_intro(
    derivation: &EvalContML4Derivation,
    rule: EvalContML4DerivationRule,
    expected_expr: &EvalContML4Expr,
    expected_input: EvalContML4Value,
) -> Result<EvalContML4Judgment, CheckError> {
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e >> k evalto v"),
        );
    };

    if expr != expected_expr {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e >> k evalto v"),
        );
    }

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let Some((actual_input, actual_continuation, actual_value)) = as_cont_eval_to(&first)
            else {
                return Err(rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "v1 => k evalto v", &first),
                ));
            };

            let expected =
                cont_eval_to_judgment(expected_input, continuation.clone(), value.clone());
            if !value_semantic_eq(actual_input, judgment_cont_input(&expected))
                || !actual_continuation.semantic_eq(continuation)
                || !value_semantic_eq(actual_value, value)
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "ensure premise forwards the introduced value into continuation k",
                    ),
                ));
            }

            let _ = env;
            Ok(derivation.judgment.clone())
        }
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_premise_count_message(rule, 1, derivation.subderivations.len()),
        ),
    }
}

fn check_e_int(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EInt;
    let Some((_, expr, _, _)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- i >> k evalto v"),
        );
    };
    let EvalContML4Expr::Int(value) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- i >> k evalto v"),
        );
    };

    check_unary_value_intro(derivation, rule, expr, EvalContML4Value::Int(*value))
}

fn check_e_bool(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EBool;
    let Some((_, expr, _, _)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- b >> k evalto v"),
        );
    };
    let EvalContML4Expr::Bool(value) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- b >> k evalto v"),
        );
    };

    check_unary_value_intro(derivation, rule, expr, EvalContML4Value::Bool(*value))
}

fn check_e_var(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EVar;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x >> k evalto v"),
        );
    };

    let EvalContML4Expr::Var(name) = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- x >> k evalto v"),
        );
    };

    let Some(bound_value) = lookup_env(env, name) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_rule_application_message(rule, &[], &[], "ensure x is bound in Gamma"),
        );
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let expected =
                cont_eval_to_judgment(bound_value.clone(), continuation.clone(), value.clone());

            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "look up x in Gamma and pass that value to continuation k",
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

fn check_e_fun(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EFun;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun x -> e >> k evalto v"),
        );
    };

    let EvalContML4Expr::Fun { param, body } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- fun x -> e >> k evalto v"),
        );
    };

    let closure = EvalContML4Value::Closure {
        env: env.clone(),
        param: param.clone(),
        body: body.as_ref().clone(),
    };

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            let expected = cont_eval_to_judgment(closure, continuation.clone(), value.clone());
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "package closure value and forward it to continuation k",
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

fn check_e_let(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::ELet;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let x = e1 in e2 >> k evalto v"),
        );
    };

    let EvalContML4Expr::Let {
        name,
        bound_expr,
        body,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- let x = e1 in e2 >> k evalto v"),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::LetBody {
            env: Some(env.clone()),
            name: name.clone(),
            body: body.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        bound_expr.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate e1 under Gamma with continuation {Gamma |- let x = _ in e2} >> k",
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

fn check_e_let_rec(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::ELetRec;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- let rec f = fun x -> e1 in e2 >> k evalto v",
            ),
        );
    };

    let EvalContML4Expr::LetRec {
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
                "Gamma |- let rec f = fun x -> e1 in e2 >> k evalto v",
            ),
        );
    };

    let recursive = EvalContML4Value::RecClosure {
        env: env.clone(),
        name: name.clone(),
        param: param.clone(),
        body: fun_body.as_ref().clone(),
    };
    let extended = extend_env(env, name.clone(), recursive);
    let expected = eval_to_judgment(
        extended,
        body.as_ref().clone(),
        continuation.clone(),
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "extend Gamma with recursive closure f before evaluating e2",
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

fn check_e_let_cc(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::ELetCc;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- letcc k in e >> k0 evalto v"),
        );
    };

    let EvalContML4Expr::LetCc { name, body } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- letcc k in e >> k0 evalto v"),
        );
    };

    let captured = EvalContML4Value::Continuation(continuation.clone());
    let extended = extend_env(env, name.clone(), captured);
    let expected = eval_to_judgment(
        extended,
        body.as_ref().clone(),
        continuation.clone(),
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "bind k to current continuation and evaluate the body under that extended environment",
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

fn check_e_binop(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EBinOp;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 >> k evalto v"),
        );
    };

    let EvalContML4Expr::BinOp { op, left, right } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 op e2 >> k evalto v"),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::EvalR {
            env: Some(env.clone()),
            op: *op,
            right: right.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        left.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate e1 under continuation {Gamma |- _ op e2} >> k",
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

fn check_e_if(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EIf;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- if e1 then e2 else e3 >> k evalto v"),
        );
    };

    let EvalContML4Expr::If {
        condition,
        then_branch,
        else_branch,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- if e1 then e2 else e3 >> k evalto v"),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::If {
            env: Some(env.clone()),
            then_branch: then_branch.as_ref().clone(),
            else_branch: else_branch.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        condition.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate condition under continuation {Gamma |- if _ then e2 else e3} >> k",
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

fn check_e_app(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EApp;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 >> k evalto v"),
        );
    };

    let EvalContML4Expr::App { func, arg } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 e2 >> k evalto v"),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::EvalArg {
            env: Some(env.clone()),
            arg: arg.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        func.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate function expression under continuation {Gamma |- _ e2} >> k",
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

fn check_e_nil(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::ENil;
    let Some((_, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- [] >> k evalto v"),
        );
    };

    if !matches!(expr, EvalContML4Expr::Nil) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- [] >> k evalto v"),
        );
    }

    let expected =
        cont_eval_to_judgment(EvalContML4Value::Nil, continuation.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "forward [] into continuation k",
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

fn check_e_cons(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::ECons;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 :: e2 >> k evalto v"),
        );
    };

    let EvalContML4Expr::Cons { head, tail } = expr else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "Gamma |- e1 :: e2 >> k evalto v"),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::EvalConsR {
            env: Some(env.clone()),
            tail_expr: tail.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        head.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate e1 under continuation {Gamma |- _ :: e2} >> k",
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

fn check_e_match(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::EMatch;
    let Some((env, expr, continuation, value)) = as_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- match e with [] -> e1 | x :: y -> e2 >> k evalto v",
            ),
        );
    };

    let EvalContML4Expr::Match {
        scrutinee,
        nil_case,
        head_name,
        tail_name,
        cons_case,
    } = expr
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "Gamma |- match e with [] -> e1 | x :: y -> e2 >> k evalto v",
            ),
        );
    };

    let expected_cont = prepend_frame(
        EvalContML4ContFrame::Match {
            env: Some(env.clone()),
            nil_case: nil_case.as_ref().clone(),
            head_name: head_name.clone(),
            tail_name: tail_name.clone(),
            cons_case: cons_case.as_ref().clone(),
        },
        continuation,
    );
    let expected = eval_to_judgment(
        env.clone(),
        scrutinee.as_ref().clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate scrutinee under continuation {Gamma |- match _ with ...} >> k",
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

fn check_c_ret(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CRet;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => _ evalto v"),
        );
    };

    if !continuation.frames.is_empty() || !value_semantic_eq(input, value) {
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

fn check_c_eval_r(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalR;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {Gamma |- _ op e2} >> k evalto v"),
        );
    };

    let EvalContML4Value::Int(left_value) = input else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {Gamma |- _ op e2} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {Gamma |- _ op e2} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::EvalR { env, op, right } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i1 => {Gamma |- _ op e2} >> k evalto v"),
        );
    };

    let next_frame = match op {
        EvalContML4BinOp::Plus => EvalContML4ContFrame::Plus { left: *left_value },
        EvalContML4BinOp::Minus => EvalContML4ContFrame::Minus { left: *left_value },
        EvalContML4BinOp::Times => EvalContML4ContFrame::Times { left: *left_value },
        EvalContML4BinOp::Lt => EvalContML4ContFrame::Lt { left: *left_value },
    };
    let expected_cont = prepend_frame(next_frame, &tail);
    let expected_env = env.clone().unwrap_or_default();
    let expected = eval_to_judgment(expected_env, right.clone(), expected_cont, value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate e2 with continuation {i1 op _} >> k",
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

fn check_c_plus(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML4DerivationRule::CPlus)
}

fn check_c_minus(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML4DerivationRule::CMinus)
}

fn check_c_times(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    check_c_binop_int(derivation, EvalContML4DerivationRule::CTimes)
}

fn check_c_lt(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CLt;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };
    let EvalContML4Value::Int(right_value) = input else {
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

    let EvalContML4ContFrame::Lt { left } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 < _} >> k evalto v"),
        );
    };

    let bool_result = *left < *right_value;
    let expected_first = EvalContML4Judgment::LessThanIs {
        left: *left,
        right: *right_value,
        result: bool_result,
    };
    let expected_second = cont_eval_to_judgment(
        EvalContML4Value::Bool(bool_result),
        tail.clone(),
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            if !judgment_semantic_eq(&first, &expected_first)
                || !judgment_semantic_eq(&second, &expected_second)
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected_first, expected_second],
                        &[first, second],
                        "check arithmetic premise and pass boolean result to continuation k",
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

fn check_c_if_t(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    check_c_if_branch(derivation, true)
}

fn check_c_if_f(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    check_c_if_branch(derivation, false)
}

fn check_c_if_branch(
    derivation: &EvalContML4Derivation,
    truth: bool,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = if truth {
        EvalContML4DerivationRule::CIfT
    } else {
        EvalContML4DerivationRule::CIfF
    };

    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "b => {Gamma |- if _ then e1 else e2} >> k evalto v",
            ),
        );
    };

    if input != &EvalContML4Value::Bool(truth) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "b => {Gamma |- if _ then e1 else e2} >> k evalto v",
            ),
        );
    }

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "b => {Gamma |- if _ then e1 else e2} >> k evalto v",
            ),
        );
    };

    let EvalContML4ContFrame::If {
        env,
        then_branch,
        else_branch,
    } = head
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "b => {Gamma |- if _ then e1 else e2} >> k evalto v",
            ),
        );
    };

    let expected_env = env.clone().unwrap_or_default();
    let branch = if truth {
        then_branch.clone()
    } else {
        else_branch.clone()
    };
    let expected = eval_to_judgment(expected_env, branch, tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate selected branch under continuation k",
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

fn check_c_let_body(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CLetBody;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- let x = _ in e2} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- let x = _ in e2} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::LetBody { env, name, body } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- let x = _ in e2} >> k evalto v"),
        );
    };

    let expected_env = extend_env(
        &env.clone().unwrap_or_default(),
        name.clone(),
        input.clone(),
    );
    let expected = eval_to_judgment(expected_env, body.clone(), tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "extend Gamma with x = v1 and evaluate body under continuation k",
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

fn check_c_eval_arg(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalArg;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ e2} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ e2} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::EvalArg { env, arg } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ e2} >> k evalto v"),
        );
    };

    let expected_env = env.clone().unwrap_or_default();
    let expected_cont = prepend_frame(
        EvalContML4ContFrame::EvalFun {
            func: input.clone(),
        },
        &tail,
    );
    let expected = eval_to_judgment(expected_env, arg.clone(), expected_cont, value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate argument under continuation {v1 _} >> k",
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

fn check_c_eval_fun(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalFun;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {(Gamma')[fun x -> e] _} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {(Gamma')[fun x -> e] _} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::EvalFun { func } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {(Gamma')[fun x -> e] _} >> k evalto v"),
        );
    };

    let EvalContML4Value::Closure { env, param, body } = func else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {(Gamma')[fun x -> e] _} >> k evalto v"),
        );
    };

    let expected_env = extend_env(env, param.clone(), input.clone());
    let expected = eval_to_judgment(expected_env, body.clone(), tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "extend closure environment with x = v2 and evaluate body under k",
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

fn check_c_eval_fun_r(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalFunR;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v2 => {(Gamma')[rec f = fun x -> e] _} >> k evalto v",
            ),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v2 => {(Gamma')[rec f = fun x -> e] _} >> k evalto v",
            ),
        );
    };

    let EvalContML4ContFrame::EvalFun { func } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v2 => {(Gamma')[rec f = fun x -> e] _} >> k evalto v",
            ),
        );
    };

    let EvalContML4Value::RecClosure {
        env,
        name,
        param,
        body,
    } = func
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v2 => {(Gamma')[rec f = fun x -> e] _} >> k evalto v",
            ),
        );
    };

    let rec_value = func.clone();
    let with_rec = extend_env(env, name.clone(), rec_value);
    let expected_env = extend_env(&with_rec, param.clone(), input.clone());
    let expected = eval_to_judgment(expected_env, body.clone(), tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "extend recursive closure environment with f and x = v2, then evaluate body under k",
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

fn check_c_eval_fun_c(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalFunC;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => {[k0] _} >> k evalto v'"),
        );
    };

    let Some((head, _tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => {[k0] _} >> k evalto v'"),
        );
    };

    let EvalContML4ContFrame::EvalFun { func } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => {[k0] _} >> k evalto v'"),
        );
    };

    let EvalContML4Value::Continuation(captured) = func else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v => {[k0] _} >> k evalto v'"),
        );
    };

    let expected = cont_eval_to_judgment(input.clone(), captured.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "apply captured continuation directly (discarding the current tail)",
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

fn check_c_eval_cons_r(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CEvalConsR;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ :: e2} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ :: e2} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::EvalConsR { env, tail_expr } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v1 => {Gamma |- _ :: e2} >> k evalto v"),
        );
    };

    let expected_env = env.clone().unwrap_or_default();
    let expected_cont = prepend_frame(
        EvalContML4ContFrame::Cons {
            head: input.clone(),
        },
        &tail,
    );
    let expected = eval_to_judgment(
        expected_env,
        tail_expr.clone(),
        expected_cont,
        value.clone(),
    );

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "evaluate tail expression under continuation {v1 :: _} >> k",
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

fn check_c_cons(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CCons;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {v1 :: _} >> k evalto v"),
        );
    };

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {v1 :: _} >> k evalto v"),
        );
    };

    let EvalContML4ContFrame::Cons { head } = head else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "v2 => {v1 :: _} >> k evalto v"),
        );
    };

    let cons_value = EvalContML4Value::Cons {
        head: Box::new(head.clone()),
        tail: Box::new(input.clone()),
    };
    let expected = cont_eval_to_judgment(cons_value, tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "construct v1 :: v2 then pass it to continuation k",
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

fn check_c_match_nil(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CMatchNil;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "[] => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    if !matches!(input, EvalContML4Value::Nil) {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "[] => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    }

    let Some((head, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "[] => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let EvalContML4ContFrame::Match {
        env,
        nil_case,
        head_name: _,
        tail_name: _,
        cons_case: _,
    } = head
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "[] => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let expected_env = env.clone().unwrap_or_default();
    let expected = eval_to_judgment(expected_env, nil_case.clone(), tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "for [] scrutinee, evaluate nil-branch under continuation k",
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

fn check_c_match_cons(
    derivation: &EvalContML4Derivation,
) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::CMatchCons;
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v1 :: v2 => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let EvalContML4Value::Cons {
        head,
        tail: list_tail,
    } = input
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v1 :: v2 => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let Some((frame, tail)) = split_continuation(continuation) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v1 :: v2 => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let EvalContML4ContFrame::Match {
        env,
        nil_case: _,
        head_name,
        tail_name,
        cons_case,
    } = frame
    else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(
                rule,
                "v1 :: v2 => {Gamma |- match _ with [] -> e1 | x :: y -> e2} >> k evalto v",
            ),
        );
    };

    let env0 = env.clone().unwrap_or_default();
    let env1 = extend_env(&env0, head_name.clone(), head.as_ref().clone());
    let env2 = extend_env(&env1, tail_name.clone(), list_tail.as_ref().clone());
    let expected = eval_to_judgment(env2, cons_case.clone(), tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1] => {
            let first = infer_judgment(d1)?;
            if !judgment_semantic_eq(&first, &expected) {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[expected],
                        &[first],
                        "for v1 :: v2, evaluate cons-branch under Gamma extended with x = v1 and y = v2",
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
    derivation: &EvalContML4Derivation,
    rule: EvalContML4DerivationRule,
) -> Result<EvalContML4Judgment, CheckError> {
    let Some((input, continuation, value)) = as_cont_eval_to(&derivation.judgment) else {
        return fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
        );
    };

    let EvalContML4Value::Int(right_value) = input else {
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

    let (base_judgment, result_value) = match (rule, head) {
        (EvalContML4DerivationRule::CPlus, EvalContML4ContFrame::Plus { left }) => (
            EvalContML4Judgment::PlusIs {
                left: *left,
                right: *right_value,
                result: *left + *right_value,
            },
            EvalContML4Value::Int(*left + *right_value),
        ),
        (EvalContML4DerivationRule::CMinus, EvalContML4ContFrame::Minus { left }) => (
            EvalContML4Judgment::MinusIs {
                left: *left,
                right: *right_value,
                result: *left - *right_value,
            },
            EvalContML4Value::Int(*left - *right_value),
        ),
        (EvalContML4DerivationRule::CTimes, EvalContML4ContFrame::Times { left }) => (
            EvalContML4Judgment::TimesIs {
                left: *left,
                right: *right_value,
                result: *left * *right_value,
            },
            EvalContML4Value::Int(*left * *right_value),
        ),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "i2 => {i1 op _} >> k evalto v"),
            )
        }
    };

    let expected_second = cont_eval_to_judgment(result_value, tail.clone(), value.clone());

    match derivation.subderivations.as_slice() {
        [d1, d2] => {
            let first = infer_judgment(d1)?;
            let second = infer_judgment(d2)?;
            if !judgment_semantic_eq(&first, &base_judgment)
                || !judgment_semantic_eq(&second, &expected_second)
            {
                return Err(rule_violation(
                    derivation,
                    wrong_rule_application_message(
                        rule,
                        &[base_judgment, expected_second],
                        &[first, second],
                        "check arithmetic premise and pass computed result to continuation k",
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

fn check_b_plus(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::BPlus;
    match &derivation.judgment {
        EvalContML4Judgment::PlusIs {
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

fn check_b_minus(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::BMinus;
    match &derivation.judgment {
        EvalContML4Judgment::MinusIs {
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

fn check_b_times(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::BTimes;
    match &derivation.judgment {
        EvalContML4Judgment::TimesIs {
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

fn check_b_lt(derivation: &EvalContML4Derivation) -> Result<EvalContML4Judgment, CheckError> {
    let rule = EvalContML4DerivationRule::BLt;
    match &derivation.judgment {
        EvalContML4Judgment::LessThanIs {
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
    continuation: &EvalContML4Continuation,
) -> Option<(&EvalContML4ContFrame, EvalContML4Continuation)> {
    let head = continuation.frames.first()?;
    let tail = EvalContML4Continuation {
        frames: continuation.frames.iter().skip(1).cloned().collect(),
        explicit_ret: continuation.explicit_ret,
    };
    Some((head, tail))
}

fn prepend_frame(
    frame: EvalContML4ContFrame,
    continuation: &EvalContML4Continuation,
) -> EvalContML4Continuation {
    let mut frames = vec![frame];
    frames.extend(continuation.frames.iter().cloned());
    EvalContML4Continuation {
        frames,
        explicit_ret: continuation.explicit_ret,
    }
}

fn eval_to_judgment(
    env: EvalContML4Env,
    expr: EvalContML4Expr,
    continuation: EvalContML4Continuation,
    value: EvalContML4Value,
) -> EvalContML4Judgment {
    let has_continuation = !continuation.frames.is_empty() || continuation.explicit_ret;
    EvalContML4Judgment::EvalTo {
        env,
        expr,
        continuation,
        value,
        has_continuation,
    }
}

fn cont_eval_to_judgment(
    input: EvalContML4Value,
    continuation: EvalContML4Continuation,
    value: EvalContML4Value,
) -> EvalContML4Judgment {
    EvalContML4Judgment::ContEvalTo {
        input,
        continuation,
        value,
    }
}

fn judgment_cont_input(judgment: &EvalContML4Judgment) -> &EvalContML4Value {
    let EvalContML4Judgment::ContEvalTo { input, .. } = judgment else {
        panic!("internal error: expected continuation judgment");
    };
    input
}

fn judgment_semantic_eq(left: &EvalContML4Judgment, right: &EvalContML4Judgment) -> bool {
    match (left, right) {
        (
            EvalContML4Judgment::EvalTo {
                env: left_env,
                expr: left_expr,
                continuation: left_cont,
                value: left_value,
                ..
            },
            EvalContML4Judgment::EvalTo {
                env: right_env,
                expr: right_expr,
                continuation: right_cont,
                value: right_value,
                ..
            },
        ) => {
            env_semantic_eq(left_env, right_env)
                && left_expr == right_expr
                && value_semantic_eq(left_value, right_value)
                && left_cont.semantic_eq(right_cont)
        }
        (
            EvalContML4Judgment::ContEvalTo {
                input: left_input,
                continuation: left_cont,
                value: left_value,
            },
            EvalContML4Judgment::ContEvalTo {
                input: right_input,
                continuation: right_cont,
                value: right_value,
            },
        ) => {
            value_semantic_eq(left_input, right_input)
                && value_semantic_eq(left_value, right_value)
                && left_cont.semantic_eq(right_cont)
        }
        _ => left == right,
    }
}

fn env_semantic_eq(left: &EvalContML4Env, right: &EvalContML4Env) -> bool {
    left.0.len() == right.0.len()
        && left
            .0
            .iter()
            .zip(right.0.iter())
            .all(|(left_binding, right_binding)| {
                left_binding.name == right_binding.name
                    && value_semantic_eq(&left_binding.value, &right_binding.value)
            })
}

fn value_semantic_eq(left: &EvalContML4Value, right: &EvalContML4Value) -> bool {
    match (left, right) {
        (EvalContML4Value::Int(left), EvalContML4Value::Int(right)) => left == right,
        (EvalContML4Value::Bool(left), EvalContML4Value::Bool(right)) => left == right,
        (
            EvalContML4Value::Closure {
                env: left_env,
                param: left_param,
                body: left_body,
            },
            EvalContML4Value::Closure {
                env: right_env,
                param: right_param,
                body: right_body,
            },
        ) => {
            env_semantic_eq(left_env, right_env)
                && left_param == right_param
                && left_body == right_body
        }
        (
            EvalContML4Value::RecClosure {
                env: left_env,
                name: left_name,
                param: left_param,
                body: left_body,
            },
            EvalContML4Value::RecClosure {
                env: right_env,
                name: right_name,
                param: right_param,
                body: right_body,
            },
        ) => {
            env_semantic_eq(left_env, right_env)
                && left_name == right_name
                && left_param == right_param
                && left_body == right_body
        }
        (EvalContML4Value::Continuation(left), EvalContML4Value::Continuation(right)) => {
            left.semantic_eq(right)
        }
        (EvalContML4Value::Nil, EvalContML4Value::Nil) => true,
        (
            EvalContML4Value::Cons {
                head: left_head,
                tail: left_tail,
            },
            EvalContML4Value::Cons {
                head: right_head,
                tail: right_tail,
            },
        ) => value_semantic_eq(left_head, right_head) && value_semantic_eq(left_tail, right_tail),
        _ => false,
    }
}

fn lookup_env<'a>(env: &'a EvalContML4Env, name: &str) -> Option<&'a EvalContML4Value> {
    env.0
        .iter()
        .rev()
        .find(|binding| binding.name == name)
        .map(|binding| &binding.value)
}

fn extend_env(env: &EvalContML4Env, name: String, value: EvalContML4Value) -> EvalContML4Env {
    let mut bindings = env.0.clone();
    bindings.push(EvalContML4Binding { name, value });
    EvalContML4Env(bindings)
}

fn as_eval_to(
    judgment: &EvalContML4Judgment,
) -> Option<(
    &EvalContML4Env,
    &EvalContML4Expr,
    &EvalContML4Continuation,
    &EvalContML4Value,
)> {
    let EvalContML4Judgment::EvalTo {
        env,
        expr,
        continuation,
        value,
        ..
    } = judgment
    else {
        return None;
    };
    Some((env, expr, continuation, value))
}

fn as_cont_eval_to(
    judgment: &EvalContML4Judgment,
) -> Option<(
    &EvalContML4Value,
    &EvalContML4Continuation,
    &EvalContML4Value,
)> {
    let EvalContML4Judgment::ContEvalTo {
        input,
        continuation,
        value,
    } = judgment
    else {
        return None;
    };
    Some((input, continuation, value))
}

fn rule_violation(derivation: &EvalContML4Derivation, detail: impl Into<String>) -> CheckError {
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

    use super::EvalContML4Game;

    #[test]
    fn reports_root_judgment_text_for_all_eval_cont_ml4_fixtures() {
        let game = EvalContML4Game;
        for source in [
            include_str!("../../../copl/130.copl"),
            include_str!("../../../copl/131.copl"),
            include_str!("../../../copl/132.copl"),
            include_str!("../../../copl/133.copl"),
            include_str!("../../../copl/134.copl"),
            include_str!("../../../copl/135.copl"),
            include_str!("../../../copl/136.copl"),
            include_str!("../../../copl/137.copl"),
            include_str!("../../../copl/138.copl"),
            include_str!("../../../copl/139.copl"),
            include_str!("../../../copl/140.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            let root = super::parse_source(source)
                .expect("parse should succeed")
                .judgment
                .to_string();
            assert_eq!(report.summary, root);
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "|- 1 >> _ evalto 1 by E-Int {}";
        let err = EvalContML4Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err
            .message()
            .contains("The number of premises is wrong: E-Int"));
        assert!(err.message().contains("expected: 1, actual: 0"));
        assert!(err.message().contains("premise path: root"));
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "|- 1 >> _ evalto 1 by E-Unknown {}";
        let err = EvalContML4Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("No such rule"));
        assert!(err.message().contains("available: E-Int, E-Bool, E-Var"));
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
|- 1 + 2 evalto 3 by E-BinOp {
  |- 1 >> { |- _ + 2 } evalto 3 by E-Unknown {}
}
"#;
        let err = EvalContML4Game
            .check(source)
            .expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("premise path: 1"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
