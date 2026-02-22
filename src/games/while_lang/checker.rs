use crate::core::{
    annotate_rule_violation_with_premise_path, CheckError, CheckReport, Game, GameKind,
};

use super::parser::parse_source;
use super::syntax::{Store, WhileAExp, WhileBExp, WhileCom, WhileDerivation, WhileJudgment};

#[derive(Debug, Clone, Copy)]
enum WhileRule {
    AConst,
    AVar,
    APlus,
    AMinus,
    ATimes,
    BConst,
    BNot,
    BAnd,
    BOr,
    BLt,
    BEq,
    BLe,
    CSkip,
    CAssign,
    CSeq,
    CIfT,
    CIfF,
    CWhileT,
    CWhileF,
}

impl WhileRule {
    fn parse(name: &str) -> Option<Self> {
        match name {
            "A-Const" => Some(Self::AConst),
            "A-Var" => Some(Self::AVar),
            "A-Plus" => Some(Self::APlus),
            "A-Minus" => Some(Self::AMinus),
            "A-Times" => Some(Self::ATimes),
            "B-Const" => Some(Self::BConst),
            "B-Not" => Some(Self::BNot),
            "B-And" => Some(Self::BAnd),
            "B-Or" => Some(Self::BOr),
            "B-Lt" => Some(Self::BLt),
            "B-Eq" => Some(Self::BEq),
            "B-Le" => Some(Self::BLe),
            "C-Skip" => Some(Self::CSkip),
            "C-Assign" => Some(Self::CAssign),
            "C-Seq" => Some(Self::CSeq),
            "C-IfT" => Some(Self::CIfT),
            "C-IfF" => Some(Self::CIfF),
            "C-WhileT" => Some(Self::CWhileT),
            "C-WhileF" => Some(Self::CWhileF),
            _ => None,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::AConst => "A-Const",
            Self::AVar => "A-Var",
            Self::APlus => "A-Plus",
            Self::AMinus => "A-Minus",
            Self::ATimes => "A-Times",
            Self::BConst => "B-Const",
            Self::BNot => "B-Not",
            Self::BAnd => "B-And",
            Self::BOr => "B-Or",
            Self::BLt => "B-Lt",
            Self::BEq => "B-Eq",
            Self::BLe => "B-Le",
            Self::CSkip => "C-Skip",
            Self::CAssign => "C-Assign",
            Self::CSeq => "C-Seq",
            Self::CIfT => "C-IfT",
            Self::CIfF => "C-IfF",
            Self::CWhileT => "C-WhileT",
            Self::CWhileF => "C-WhileF",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct WhileGame;

impl Game for WhileGame {
    fn kind(&self) -> GameKind {
        GameKind::While
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

fn infer_judgment(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let Some(rule) = WhileRule::parse(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            unknown_rule_message(&derivation.rule_name),
        ));
    };

    check_rule_application(derivation, rule)
}

fn check_rule_application(
    derivation: &WhileDerivation,
    rule: WhileRule,
) -> Result<WhileJudgment, CheckError> {
    match rule {
        WhileRule::AConst => check_a_const(derivation),
        WhileRule::AVar => check_a_var(derivation),
        WhileRule::APlus => check_a_plus(derivation),
        WhileRule::AMinus => check_a_minus(derivation),
        WhileRule::ATimes => check_a_times(derivation),
        WhileRule::BConst => check_b_const(derivation),
        WhileRule::BNot => check_b_not(derivation),
        WhileRule::BAnd => check_b_and(derivation),
        WhileRule::BOr => check_b_or(derivation),
        WhileRule::BLt => check_b_lt(derivation),
        WhileRule::BEq => check_b_eq(derivation),
        WhileRule::BLe => check_b_le(derivation),
        WhileRule::CSkip => check_c_skip(derivation),
        WhileRule::CAssign => check_c_assign(derivation),
        WhileRule::CSeq => check_c_seq(derivation),
        WhileRule::CIfT => check_c_if_t(derivation),
        WhileRule::CIfF => check_c_if_f(derivation),
        WhileRule::CWhileT => check_c_while_t(derivation),
        WhileRule::CWhileF => check_c_while_f(derivation),
    }
}

fn check_a_const(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::AConst;
    match &derivation.judgment {
        WhileJudgment::AEval {
            expr: WhileAExp::Int(value),
            value: result,
            ..
        } if value == result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "sigma |- i evalto i"),
        ),
    }
}

fn check_a_var(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::AVar;
    match &derivation.judgment {
        WhileJudgment::AEval {
            store,
            expr: WhileAExp::Var(name),
            value,
        } if store.lookup(name) == Some(*value) => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        WhileJudgment::AEval {
            store,
            expr: WhileAExp::Var(name),
            value,
        } => fail_after_checking_subderivations(
            derivation,
            format!(
                "Wrong rule application: {} (expected: {} maps to {value} in store, actual store: {store}; fix: use a value consistent with the store binding)",
                rule.name(),
                name
            ),
        ),
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "sigma |- x evalto i"),
        ),
    }
}

fn check_a_plus(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_binary_arith(derivation, WhileRule::APlus)
}

fn check_a_minus(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_binary_arith(derivation, WhileRule::AMinus)
}

fn check_a_times(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_binary_arith(derivation, WhileRule::ATimes)
}

fn check_binary_arith(
    derivation: &WhileDerivation,
    rule: WhileRule,
) -> Result<WhileJudgment, CheckError> {
    let expected_shape = match rule {
        WhileRule::APlus => "sigma |- a1 + a2 evalto i3",
        WhileRule::AMinus => "sigma |- a1 - a2 evalto i3",
        WhileRule::ATimes => "sigma |- a1 * a2 evalto i3",
        _ => unreachable!(),
    };

    let (store, left_expr, right_expr, result_value) = match (&derivation.judgment, rule) {
        (
            WhileJudgment::AEval {
                store,
                expr: WhileAExp::Plus(left, right),
                value,
            },
            WhileRule::APlus,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        (
            WhileJudgment::AEval {
                store,
                expr: WhileAExp::Minus(left, right),
                value,
            },
            WhileRule::AMinus,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        (
            WhileJudgment::AEval {
                store,
                expr: WhileAExp::Times(left, right),
                value,
            },
            WhileRule::ATimes,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, expected_shape),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;

            let (first_store, first_expr, first_value) = as_a_eval(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "sigma |- a1 evalto i1", &first),
                )
            })?;
            let (second_store, second_expr, second_value) =
                as_a_eval(&second).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(
                            rule,
                            "second",
                            "sigma |- a2 evalto i2",
                            &second,
                        ),
                    )
                })?;

            let expected_result = match rule {
                WhileRule::APlus => first_value + second_value,
                WhileRule::AMinus => first_value - second_value,
                WhileRule::ATimes => first_value * second_value,
                _ => unreachable!(),
            };

            if first_store == store
                && second_store == store
                && first_expr == left_expr
                && second_expr == right_expr
                && result_value == expected_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::AEval {
                    store: store.clone(),
                    expr: left_expr.clone(),
                    value: *first_value,
                };
                let expected_second = WhileJudgment::AEval {
                    store: store.clone(),
                    expr: right_expr.clone(),
                    value: *second_value,
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], expected result: {expected_result}, actual premises: [{first}], [{second}], actual result: {result_value}; fix: keep stores/expressions aligned and recompute result)",
                        rule.name()
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

fn check_b_const(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::BConst;
    match &derivation.judgment {
        WhileJudgment::BEval {
            expr: WhileBExp::Bool(value),
            value: result,
            ..
        } if value == result => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "sigma |- bv evalto bv"),
        ),
    }
}

fn check_b_not(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::BNot;
    let (store, expr, result) = match &derivation.judgment {
        WhileJudgment::BEval {
            store,
            expr: WhileBExp::Not(expr),
            value,
        } => (store, expr.as_ref(), *value),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "sigma |- !b evalto bv2"),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [premise_derivation] => {
            let premise = infer_judgment(premise_derivation)?;
            let (premise_store, premise_expr, premise_value) =
                as_b_eval(&premise).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(
                            rule,
                            "first",
                            "sigma |- b evalto bv1",
                            &premise,
                        ),
                    )
                })?;

            if premise_store == store && premise_expr == expr && result != *premise_value {
                Ok(derivation.judgment.clone())
            } else {
                let expected_premise = WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value: *premise_value,
                };
                let expected_result = !premise_value;
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premise: [{expected_premise}], expected result: {expected_result}, actual premise: [{premise}], actual result: {result}; fix: keep store/expression consistent and negate premise result)",
                        rule.name()
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

fn check_b_and(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_binary_bool(derivation, WhileRule::BAnd)
}

fn check_b_or(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_binary_bool(derivation, WhileRule::BOr)
}

fn check_binary_bool(
    derivation: &WhileDerivation,
    rule: WhileRule,
) -> Result<WhileJudgment, CheckError> {
    let expected_shape = match rule {
        WhileRule::BAnd => "sigma |- b1 && b2 evalto bv3",
        WhileRule::BOr => "sigma |- b1 || b2 evalto bv3",
        _ => unreachable!(),
    };

    let (store, left_expr, right_expr, result_value) = match (&derivation.judgment, rule) {
        (
            WhileJudgment::BEval {
                store,
                expr: WhileBExp::And(left, right),
                value,
            },
            WhileRule::BAnd,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        (
            WhileJudgment::BEval {
                store,
                expr: WhileBExp::Or(left, right),
                value,
            },
            WhileRule::BOr,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, expected_shape),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;

            let (first_store, first_expr, first_value) = as_b_eval(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "sigma |- b1 evalto bv1", &first),
                )
            })?;
            let (second_store, second_expr, second_value) =
                as_b_eval(&second).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(
                            rule,
                            "second",
                            "sigma |- b2 evalto bv2",
                            &second,
                        ),
                    )
                })?;

            let expected_result = match rule {
                WhileRule::BAnd => *first_value && *second_value,
                WhileRule::BOr => *first_value || *second_value,
                _ => unreachable!(),
            };

            if first_store == store
                && second_store == store
                && first_expr == left_expr
                && second_expr == right_expr
                && result_value == expected_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::BEval {
                    store: store.clone(),
                    expr: left_expr.clone(),
                    value: *first_value,
                };
                let expected_second = WhileJudgment::BEval {
                    store: store.clone(),
                    expr: right_expr.clone(),
                    value: *second_value,
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], expected result: {expected_result}, actual premises: [{first}], [{second}], actual result: {result_value}; fix: keep stores/expressions aligned and recompute boolean result)",
                        rule.name()
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

fn check_b_lt(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_compare_bool(derivation, WhileRule::BLt)
}

fn check_b_eq(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_compare_bool(derivation, WhileRule::BEq)
}

fn check_b_le(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_compare_bool(derivation, WhileRule::BLe)
}

fn check_compare_bool(
    derivation: &WhileDerivation,
    rule: WhileRule,
) -> Result<WhileJudgment, CheckError> {
    let expected_shape = match rule {
        WhileRule::BLt => "sigma |- a1 < a2 evalto bv",
        WhileRule::BEq => "sigma |- a1 = a2 evalto bv",
        WhileRule::BLe => "sigma |- a1 <= a2 evalto bv",
        _ => unreachable!(),
    };

    let (store, left_expr, right_expr, result_value) = match (&derivation.judgment, rule) {
        (
            WhileJudgment::BEval {
                store,
                expr: WhileBExp::Lt(left, right),
                value,
            },
            WhileRule::BLt,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        (
            WhileJudgment::BEval {
                store,
                expr: WhileBExp::Eq(left, right),
                value,
            },
            WhileRule::BEq,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        (
            WhileJudgment::BEval {
                store,
                expr: WhileBExp::Le(left, right),
                value,
            },
            WhileRule::BLe,
        ) => (store, left.as_ref(), right.as_ref(), *value),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, expected_shape),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;

            let (first_store, first_expr, first_value) = as_a_eval(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "sigma |- a1 evalto i1", &first),
                )
            })?;
            let (second_store, second_expr, second_value) =
                as_a_eval(&second).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(
                            rule,
                            "second",
                            "sigma |- a2 evalto i2",
                            &second,
                        ),
                    )
                })?;

            let expected_result = match rule {
                WhileRule::BLt => first_value < second_value,
                WhileRule::BEq => first_value == second_value,
                WhileRule::BLe => first_value <= second_value,
                _ => unreachable!(),
            };

            if first_store == store
                && second_store == store
                && first_expr == left_expr
                && second_expr == right_expr
                && result_value == expected_result
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::AEval {
                    store: store.clone(),
                    expr: left_expr.clone(),
                    value: *first_value,
                };
                let expected_second = WhileJudgment::AEval {
                    store: store.clone(),
                    expr: right_expr.clone(),
                    value: *second_value,
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], expected result: {expected_result}, actual premises: [{first}], [{second}], actual result: {result_value}; fix: keep stores/expressions aligned and recompute comparison)",
                        rule.name()
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

fn check_c_skip(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::CSkip;
    match &derivation.judgment {
        WhileJudgment::Changes {
            command: WhileCom::Skip,
            from,
            to,
        } if from == to => match derivation.subderivations.as_slice() {
            [] => Ok(derivation.judgment.clone()),
            _ => fail_after_checking_subderivations(
                derivation,
                wrong_premise_count_message(rule, 0, derivation.subderivations.len()),
            ),
        },
        _ => fail_after_checking_subderivations(
            derivation,
            wrong_conclusion_form_message(rule, "skip changes sigma to sigma"),
        ),
    }
}

fn check_c_assign(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::CAssign;
    let (name, expr, from, to) = match &derivation.judgment {
        WhileJudgment::Changes {
            command: WhileCom::Assign { name, expr },
            from,
            to,
        } => (name, expr, from, to),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "x := a changes sigma1 to sigma2"),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [premise_derivation] => {
            let premise = infer_judgment(premise_derivation)?;
            let (premise_store, premise_expr, premise_value) =
                as_a_eval(&premise).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(rule, "first", "sigma1 |- a evalto i", &premise),
                    )
                })?;

            let expected_to = from.update(name, *premise_value);
            if premise_store == from && premise_expr == expr && &expected_to == to {
                Ok(derivation.judgment.clone())
            } else {
                let expected_premise = WhileJudgment::AEval {
                    store: from.clone(),
                    expr: expr.clone(),
                    value: *premise_value,
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premise: [{expected_premise}], expected target store: {expected_to}, actual premise: [{premise}], actual target store: {to}; fix: evaluate rhs under source store and update variable binding)",
                        rule.name()
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

fn check_c_seq(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::CSeq;
    let (first_command, second_command, from, to) = match &derivation.judgment {
        WhileJudgment::Changes {
            command: WhileCom::Seq(first, second),
            from,
            to,
        } => (first.as_ref(), second.as_ref(), from, to),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "c1; c2 changes sigma1 to sigma3"),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;

            let (first_cmd, first_from, first_to) = as_changes(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "first",
                        "c1 changes sigma1 to sigma2",
                        &first,
                    ),
                )
            })?;
            let (second_cmd, second_from, second_to) = as_changes(&second).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "c2 changes sigma2 to sigma3",
                        &second,
                    ),
                )
            })?;

            if first_cmd == first_command
                && second_cmd == second_command
                && first_from == from
                && second_from == first_to
                && second_to == to
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::Changes {
                    command: first_command.clone(),
                    from: from.clone(),
                    to: first_to.clone(),
                };
                let expected_second = WhileJudgment::Changes {
                    command: second_command.clone(),
                    from: first_to.clone(),
                    to: to.clone(),
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], actual premises: [{first}], [{second}]; fix: thread intermediate store from first command into second command)",
                        rule.name()
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

fn check_c_if_t(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_if_rule(derivation, WhileRule::CIfT)
}

fn check_c_if_f(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    check_if_rule(derivation, WhileRule::CIfF)
}

fn check_if_rule(
    derivation: &WhileDerivation,
    rule: WhileRule,
) -> Result<WhileJudgment, CheckError> {
    let (cond, chosen_command, from, to) = match (&derivation.judgment, rule) {
        (
            WhileJudgment::Changes {
                command:
                    WhileCom::If {
                        cond,
                        then_branch,
                        else_branch: _,
                    },
                from,
                to,
            },
            WhileRule::CIfT,
        ) => (cond, then_branch.as_ref(), from, to),
        (
            WhileJudgment::Changes {
                command:
                    WhileCom::If {
                        cond,
                        then_branch: _,
                        else_branch,
                    },
                from,
                to,
            },
            WhileRule::CIfF,
        ) => (cond, else_branch.as_ref(), from, to),
        _ => {
            let expected = match rule {
                WhileRule::CIfT => "if b then c1 else c2 changes sigma1 to sigma2",
                WhileRule::CIfF => "if b then c1 else c2 changes sigma1 to sigma2",
                _ => unreachable!(),
            };
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, expected),
            );
        }
    };

    let expected_bool = matches!(rule, WhileRule::CIfT);

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;

            let (first_store, first_expr, first_value) = as_b_eval(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "sigma1 |- b evalto bool", &first),
                )
            })?;
            let (second_cmd, second_from, second_to) = as_changes(&second).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "c changes sigma1 to sigma2",
                        &second,
                    ),
                )
            })?;

            if first_store == from
                && first_expr == cond
                && *first_value == expected_bool
                && second_cmd == chosen_command
                && second_from == from
                && second_to == to
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::BEval {
                    store: from.clone(),
                    expr: cond.clone(),
                    value: expected_bool,
                };
                let expected_second = WhileJudgment::Changes {
                    command: chosen_command.clone(),
                    from: from.clone(),
                    to: to.clone(),
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], actual premises: [{first}], [{second}]; fix: evaluate condition in source store and use the matching branch)",
                        rule.name()
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

fn check_c_while_t(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::CWhileT;
    let (cond, body, from, to) = match &derivation.judgment {
        WhileJudgment::Changes {
            command: WhileCom::While { cond, body },
            from,
            to,
        } => (cond, body.as_ref(), from, to),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "while (b) do c changes sigma1 to sigma3"),
            );
        }
    };

    match derivation.subderivations.as_slice() {
        [first_derivation, second_derivation, third_derivation] => {
            let first = infer_judgment(first_derivation)?;
            let second = infer_judgment(second_derivation)?;
            let third = infer_judgment(third_derivation)?;

            let (first_store, first_expr, first_value) = as_b_eval(&first).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(rule, "first", "sigma1 |- b evalto true", &first),
                )
            })?;
            let (second_cmd, second_from, second_to) = as_changes(&second).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "second",
                        "c changes sigma1 to sigma2",
                        &second,
                    ),
                )
            })?;
            let (third_cmd, third_from, third_to) = as_changes(&third).ok_or_else(|| {
                rule_violation(
                    derivation,
                    wrong_premise_form_message(
                        rule,
                        "third",
                        "while (b) do c changes sigma2 to sigma3",
                        &third,
                    ),
                )
            })?;

            let expected_third_command = WhileCom::While {
                cond: cond.clone(),
                body: Box::new(body.clone()),
            };

            if first_store == from
                && first_expr == cond
                && *first_value
                && second_cmd == body
                && second_from == from
                && third_cmd == &expected_third_command
                && third_from == second_to
                && third_to == to
            {
                Ok(derivation.judgment.clone())
            } else {
                let expected_first = WhileJudgment::BEval {
                    store: from.clone(),
                    expr: cond.clone(),
                    value: true,
                };
                let expected_second = WhileJudgment::Changes {
                    command: body.clone(),
                    from: from.clone(),
                    to: second_to.clone(),
                };
                let expected_third = WhileJudgment::Changes {
                    command: expected_third_command,
                    from: second_to.clone(),
                    to: to.clone(),
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premises: [{expected_first}], [{expected_second}], [{expected_third}], actual premises: [{first}], [{second}], [{third}]; fix: evaluate condition as true, run body once, then continue while from updated store)",
                        rule.name()
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

fn check_c_while_f(derivation: &WhileDerivation) -> Result<WhileJudgment, CheckError> {
    let rule = WhileRule::CWhileF;
    let (cond, from, to) = match &derivation.judgment {
        WhileJudgment::Changes {
            command: WhileCom::While { cond, .. },
            from,
            to,
        } => (cond, from, to),
        _ => {
            return fail_after_checking_subderivations(
                derivation,
                wrong_conclusion_form_message(rule, "while (b) do c changes sigma to sigma"),
            );
        }
    };

    if from != to {
        return fail_after_checking_subderivations(
            derivation,
            format!(
                "Wrong rule application: {} (expected target store: {from}, actual target store: {to}; fix: when condition is false, keep the store unchanged)",
                rule.name()
            ),
        );
    }

    match derivation.subderivations.as_slice() {
        [premise_derivation] => {
            let premise = infer_judgment(premise_derivation)?;
            let (premise_store, premise_expr, premise_value) =
                as_b_eval(&premise).ok_or_else(|| {
                    rule_violation(
                        derivation,
                        wrong_premise_form_message(
                            rule,
                            "first",
                            "sigma |- b evalto false",
                            &premise,
                        ),
                    )
                })?;

            if premise_store == from && premise_expr == cond && !premise_value {
                Ok(derivation.judgment.clone())
            } else {
                let expected_premise = WhileJudgment::BEval {
                    store: from.clone(),
                    expr: cond.clone(),
                    value: false,
                };
                Err(rule_violation(
                    derivation,
                    format!(
                        "Wrong rule application: {} (expected premise: [{expected_premise}], actual premise: [{premise}]; fix: use C-WhileF only when condition evaluates to false)",
                        rule.name()
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

fn as_a_eval(judgment: &WhileJudgment) -> Option<(&Store, &WhileAExp, &i64)> {
    let WhileJudgment::AEval { store, expr, value } = judgment else {
        return None;
    };
    Some((store, expr, value))
}

fn as_b_eval(judgment: &WhileJudgment) -> Option<(&Store, &WhileBExp, &bool)> {
    let WhileJudgment::BEval { store, expr, value } = judgment else {
        return None;
    };
    Some((store, expr, value))
}

fn as_changes(judgment: &WhileJudgment) -> Option<(&WhileCom, &Store, &Store)> {
    let WhileJudgment::Changes { command, from, to } = judgment else {
        return None;
    };
    Some((command, from, to))
}

fn ensure_error_has_span(err: CheckError, derivation: &WhileDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn check_all_subderivations(subderivations: &[WhileDerivation]) -> Result<(), CheckError> {
    for subderivation in subderivations {
        infer_judgment(subderivation)?;
    }
    Ok(())
}

fn fail_after_checking_subderivations(
    derivation: &WhileDerivation,
    detail: String,
) -> Result<WhileJudgment, CheckError> {
    check_all_subderivations(&derivation.subderivations)?;
    Err(rule_violation(derivation, detail))
}

fn unknown_rule_message(rule_name: &str) -> String {
    format!(
        "No such rule: {rule_name} (available: A-Const, A-Var, A-Plus, A-Minus, A-Times, B-Const, B-Not, B-And, B-Or, B-Lt, B-Eq, B-Le, C-Skip, C-Assign, C-Seq, C-IfT, C-IfF, C-WhileT, C-WhileF; fix: replace the rule name after 'by')"
    )
}

fn wrong_premise_count_message(rule: WhileRule, expected: usize, actual: usize) -> String {
    format!(
        "The number of premises is wrong: {} (expected: {expected}, actual: {actual}; fix: add/remove derivations inside '{{ ... }}')",
        rule.name(),
    )
}

fn wrong_conclusion_form_message(rule: WhileRule, expected: &str) -> String {
    format!(
        "The form of conclusion is wrong: {} (expected: {expected}; fix: rewrite the conclusion to match this shape)",
        rule.name(),
    )
}

fn wrong_premise_form_message(
    rule: WhileRule,
    ordinal: &str,
    expected: &str,
    actual: &WhileJudgment,
) -> String {
    format!(
        "The form of the {ordinal} premise is wrong: {} (expected: {expected}, actual: {actual}; fix: rewrite the {ordinal} premise to match expected shape)",
        rule.name(),
    )
}

fn rule_violation(derivation: &WhileDerivation, detail: String) -> CheckError {
    CheckError::rule_violation(detail).with_span(derivation.span.clone())
}

#[cfg(test)]
mod tests {
    use super::WhileGame;
    use crate::core::Game;

    #[test]
    fn checks_skip_derivation() {
        let source = "skip changes s = 0 to s = 0 by C-Skip {}";
        let report = WhileGame
            .check(source)
            .expect("checker should accept a valid derivation");

        assert_eq!(report.summary, "skip changes s = 0 to s = 0");
    }

    #[test]
    fn rejects_unknown_rule_name() {
        let source = "skip changes s = 0 to s = 0 by C-Unknown {}";
        let err = WhileGame
            .check(source)
            .expect_err("checker should reject unknown rule");

        assert!(err.message().contains("No such rule: C-Unknown"));
    }
}
