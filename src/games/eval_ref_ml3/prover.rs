use std::collections::HashMap;

use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalRefML3BinOp, EvalRefML3Binding, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr,
    EvalRefML3Judgment, EvalRefML3Store, EvalRefML3Value,
};

pub(super) fn prove_judgment(
    judgment: EvalRefML3Judgment,
) -> Result<EvalRefML3Derivation, CheckError> {
    if let Some(primitive) = prove_primitive_judgment(&judgment) {
        return Ok(primitive);
    }

    let EvalRefML3Judgment::EvalTo {
        env,
        expr,
        store,
        value: expected_value,
        result_store: expected_store,
    } = &judgment
    else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };

    let Some(actual) = prove_expr(env, expr, store) else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };

    let Some((_, _, _, actual_value, actual_store)) = as_eval_to(&actual.judgment) else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };

    if actual_value == expected_value && actual_store == expected_store {
        Ok(actual)
    } else if let Some(aligned) = align_derivation_locations(&actual, &judgment) {
        Ok(aligned)
    } else {
        Err(non_derivable_judgment_error(
            &judgment,
            Some(actual.judgment.clone()),
        ))
    }
}

fn prove_expr(
    env: &EvalRefML3Env,
    expr: &EvalRefML3Expr,
    store: &EvalRefML3Store,
) -> Option<EvalRefML3Derivation> {
    match expr {
        EvalRefML3Expr::Int(value) => Some(derivation(
            EvalRefML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                store: store.clone(),
                value: EvalRefML3Value::Int(*value),
                result_store: store.clone(),
            },
            "E-Int",
            Vec::new(),
        )),
        EvalRefML3Expr::Bool(value) => Some(derivation(
            EvalRefML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                store: store.clone(),
                value: EvalRefML3Value::Bool(*value),
                result_store: store.clone(),
            },
            "E-Bool",
            Vec::new(),
        )),
        EvalRefML3Expr::Unit => Some(derivation(
            EvalRefML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                store: store.clone(),
                value: EvalRefML3Value::Unit,
                result_store: store.clone(),
            },
            "E-Unit",
            Vec::new(),
        )),
        EvalRefML3Expr::Loc(location) => {
            if !store.contains(location) {
                return None;
            }
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Loc(location.clone()),
                    result_store: store.clone(),
                },
                "E-Loc",
                Vec::new(),
            ))
        }
        EvalRefML3Expr::Var(name) => {
            let binding = env.0.iter().rev().find(|binding| binding.name == *name)?;
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: binding.value.clone(),
                    result_store: store.clone(),
                },
                "E-Var",
                Vec::new(),
            ))
        }
        EvalRefML3Expr::Let {
            name,
            bound_expr,
            body,
        } => {
            let first = prove_expr(env, bound_expr, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let mut next_env = env.0.clone();
            next_env.push(EvalRefML3Binding {
                name: name.clone(),
                value: first_value.clone(),
            });
            let next_env = EvalRefML3Env(next_env);

            let second = prove_expr(&next_env, body, first_store)?;
            let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;

            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: second_value.clone(),
                    result_store: second_store.clone(),
                },
                "E-Let",
                vec![first, second],
            ))
        }
        EvalRefML3Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let recursive_value = EvalRefML3Value::RecClosure {
                env: env.clone(),
                name: name.clone(),
                param: param.clone(),
                body: fun_body.as_ref().clone(),
            };
            let next_env = extend_env(env, name, recursive_value);
            let first = prove_expr(&next_env, body, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: first_value.clone(),
                    result_store: first_store.clone(),
                },
                "E-LetRec",
                vec![first],
            ))
        }
        EvalRefML3Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let first = prove_expr(env, condition, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let EvalRefML3Value::Bool(condition_value) = first_value else {
                return None;
            };
            if *condition_value {
                let second = prove_expr(env, then_branch, first_store)?;
                let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;
                Some(derivation(
                    EvalRefML3Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        store: store.clone(),
                        value: second_value.clone(),
                        result_store: second_store.clone(),
                    },
                    "E-IfT",
                    vec![first, second],
                ))
            } else {
                let second = prove_expr(env, else_branch, first_store)?;
                let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;
                Some(derivation(
                    EvalRefML3Judgment::EvalTo {
                        env: env.clone(),
                        expr: expr.clone(),
                        store: store.clone(),
                        value: second_value.clone(),
                        result_store: second_store.clone(),
                    },
                    "E-IfF",
                    vec![first, second],
                ))
            }
        }
        EvalRefML3Expr::Fun { param, body } => Some(derivation(
            EvalRefML3Judgment::EvalTo {
                env: env.clone(),
                expr: expr.clone(),
                store: store.clone(),
                value: EvalRefML3Value::Closure {
                    env: env.clone(),
                    param: param.clone(),
                    body: body.as_ref().clone(),
                },
                result_store: store.clone(),
            },
            "E-Fun",
            Vec::new(),
        )),
        EvalRefML3Expr::App { func, arg } => {
            let first = prove_expr(env, func, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let second = prove_expr(env, arg, first_store)?;
            let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;

            match first_value {
                EvalRefML3Value::Closure {
                    env: closure_env,
                    param,
                    body,
                } => {
                    let third_env = extend_env(closure_env, param, second_value.clone());
                    let third = prove_expr(&third_env, body, second_store)?;
                    let (_, _, _, third_value, third_store) = as_eval_to(&third.judgment)?;
                    Some(derivation(
                        EvalRefML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            store: store.clone(),
                            value: third_value.clone(),
                            result_store: third_store.clone(),
                        },
                        "E-App",
                        vec![first, second, third],
                    ))
                }
                EvalRefML3Value::RecClosure {
                    env: closure_env,
                    name,
                    param,
                    body,
                } => {
                    let recursive_value = first_value.clone();
                    let third_env = extend_env(
                        &extend_env(closure_env, name, recursive_value),
                        param,
                        second_value.clone(),
                    );
                    let third = prove_expr(&third_env, body, second_store)?;
                    let (_, _, _, third_value, third_store) = as_eval_to(&third.judgment)?;
                    Some(derivation(
                        EvalRefML3Judgment::EvalTo {
                            env: env.clone(),
                            expr: expr.clone(),
                            store: store.clone(),
                            value: third_value.clone(),
                            result_store: third_store.clone(),
                        },
                        "E-AppRec",
                        vec![first, second, third],
                    ))
                }
                _ => None,
            }
        }
        EvalRefML3Expr::BinOp { op, left, right } => {
            prove_binop(env, expr, store, *op, left, right)
        }
        EvalRefML3Expr::Ref { expr: inner } => {
            let first = prove_expr(env, inner, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let new_location = next_location_name(first_store);
            let next_store = first_store.with_appended(new_location.clone(), first_value.clone());

            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Loc(new_location),
                    result_store: next_store,
                },
                "E-Ref",
                vec![first],
            ))
        }
        EvalRefML3Expr::Deref { expr: inner } => {
            let first = prove_expr(env, inner, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let EvalRefML3Value::Loc(location) = first_value else {
                return None;
            };
            let value = first_store.lookup(location)?.clone();

            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value,
                    result_store: first_store.clone(),
                },
                "E-Deref",
                vec![first],
            ))
        }
        EvalRefML3Expr::Assign { target, value } => {
            let first = prove_expr(env, target, store)?;
            let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
            let EvalRefML3Value::Loc(location) = first_value else {
                return None;
            };

            let second = prove_expr(env, value, first_store)?;
            let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;
            let updated_store = second_store.with_updated(location, second_value.clone())?;

            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: second_value.clone(),
                    result_store: updated_store,
                },
                "E-Assign",
                vec![first, second],
            ))
        }
    }
}

fn prove_binop(
    env: &EvalRefML3Env,
    expr: &EvalRefML3Expr,
    store: &EvalRefML3Store,
    op: EvalRefML3BinOp,
    left: &EvalRefML3Expr,
    right: &EvalRefML3Expr,
) -> Option<EvalRefML3Derivation> {
    let first = prove_expr(env, left, store)?;
    let (_, _, _, first_value, first_store) = as_eval_to(&first.judgment)?;
    let EvalRefML3Value::Int(left_int) = first_value else {
        return None;
    };

    let second = prove_expr(env, right, first_store)?;
    let (_, _, _, second_value, second_store) = as_eval_to(&second.judgment)?;
    let EvalRefML3Value::Int(right_int) = second_value else {
        return None;
    };

    match op {
        EvalRefML3BinOp::Plus => {
            let result = left_int.checked_add(*right_int)?;
            let third = derivation(
                EvalRefML3Judgment::PlusIs {
                    left: *left_int,
                    right: *right_int,
                    result,
                },
                "B-Plus",
                Vec::new(),
            );
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Int(result),
                    result_store: second_store.clone(),
                },
                "E-Plus",
                vec![first, second, third],
            ))
        }
        EvalRefML3BinOp::Minus => {
            let result = left_int.checked_sub(*right_int)?;
            let third = derivation(
                EvalRefML3Judgment::MinusIs {
                    left: *left_int,
                    right: *right_int,
                    result,
                },
                "B-Minus",
                Vec::new(),
            );
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Int(result),
                    result_store: second_store.clone(),
                },
                "E-Minus",
                vec![first, second, third],
            ))
        }
        EvalRefML3BinOp::Times => {
            let result = left_int.checked_mul(*right_int)?;
            let third = derivation(
                EvalRefML3Judgment::TimesIs {
                    left: *left_int,
                    right: *right_int,
                    result,
                },
                "B-Times",
                Vec::new(),
            );
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Int(result),
                    result_store: second_store.clone(),
                },
                "E-Times",
                vec![first, second, third],
            ))
        }
        EvalRefML3BinOp::Lt => {
            let result = left_int < right_int;
            let third = derivation(
                EvalRefML3Judgment::LessThanIs {
                    left: *left_int,
                    right: *right_int,
                    result,
                },
                "B-Lt",
                Vec::new(),
            );
            Some(derivation(
                EvalRefML3Judgment::EvalTo {
                    env: env.clone(),
                    expr: expr.clone(),
                    store: store.clone(),
                    value: EvalRefML3Value::Bool(result),
                    result_store: second_store.clone(),
                },
                "E-Lt",
                vec![first, second, third],
            ))
        }
    }
}

fn prove_primitive_judgment(judgment: &EvalRefML3Judgment) -> Option<EvalRefML3Derivation> {
    match judgment {
        EvalRefML3Judgment::PlusIs {
            left,
            right,
            result,
        } if *left + *right == *result => Some(derivation(judgment.clone(), "B-Plus", Vec::new())),
        EvalRefML3Judgment::MinusIs {
            left,
            right,
            result,
        } if *left - *right == *result => Some(derivation(judgment.clone(), "B-Minus", Vec::new())),
        EvalRefML3Judgment::TimesIs {
            left,
            right,
            result,
        } if *left * *right == *result => Some(derivation(judgment.clone(), "B-Times", Vec::new())),
        EvalRefML3Judgment::LessThanIs {
            left,
            right,
            result,
        } if (*left < *right) == *result => Some(derivation(judgment.clone(), "B-Lt", Vec::new())),
        _ => None,
    }
}

fn next_location_name(store: &EvalRefML3Store) -> String {
    let mut index = 0usize;
    loop {
        let candidate = format!("l{index}");
        if !store.contains(&candidate) {
            return candidate;
        }
        index += 1;
    }
}

fn extend_env(env: &EvalRefML3Env, name: &str, value: EvalRefML3Value) -> EvalRefML3Env {
    let mut next_env = env.0.clone();
    next_env.push(EvalRefML3Binding {
        name: name.to_string(),
        value,
    });
    EvalRefML3Env(next_env)
}

fn align_derivation_locations(
    derivation: &EvalRefML3Derivation,
    expected: &EvalRefML3Judgment,
) -> Option<EvalRefML3Derivation> {
    let mut forward = HashMap::<String, String>::new();
    let mut reverse = HashMap::<String, String>::new();
    if !build_location_mapping_judgment(&derivation.judgment, expected, &mut forward, &mut reverse)
    {
        return None;
    }
    let aligned = rename_derivation_locations(derivation, &forward);
    if aligned.judgment == *expected {
        Some(aligned)
    } else {
        None
    }
}

fn build_location_mapping_judgment(
    actual: &EvalRefML3Judgment,
    expected: &EvalRefML3Judgment,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    match (actual, expected) {
        (
            EvalRefML3Judgment::EvalTo {
                env: actual_env,
                expr: actual_expr,
                store: actual_store,
                value: actual_value,
                result_store: actual_result_store,
            },
            EvalRefML3Judgment::EvalTo {
                env: expected_env,
                expr: expected_expr,
                store: expected_store,
                value: expected_value,
                result_store: expected_result_store,
            },
        ) => {
            build_location_mapping_env(actual_env, expected_env, forward, reverse)
                && build_location_mapping_expr(actual_expr, expected_expr, forward, reverse)
                && build_location_mapping_store(actual_store, expected_store, forward, reverse)
                && build_location_mapping_value(actual_value, expected_value, forward, reverse)
                && build_location_mapping_store(
                    actual_result_store,
                    expected_result_store,
                    forward,
                    reverse,
                )
        }
        (
            EvalRefML3Judgment::PlusIs {
                left: actual_left,
                right: actual_right,
                result: actual_result,
            },
            EvalRefML3Judgment::PlusIs {
                left: expected_left,
                right: expected_right,
                result: expected_result,
            },
        ) => {
            actual_left == expected_left
                && actual_right == expected_right
                && actual_result == expected_result
        }
        (
            EvalRefML3Judgment::MinusIs {
                left: actual_left,
                right: actual_right,
                result: actual_result,
            },
            EvalRefML3Judgment::MinusIs {
                left: expected_left,
                right: expected_right,
                result: expected_result,
            },
        ) => {
            actual_left == expected_left
                && actual_right == expected_right
                && actual_result == expected_result
        }
        (
            EvalRefML3Judgment::TimesIs {
                left: actual_left,
                right: actual_right,
                result: actual_result,
            },
            EvalRefML3Judgment::TimesIs {
                left: expected_left,
                right: expected_right,
                result: expected_result,
            },
        ) => {
            actual_left == expected_left
                && actual_right == expected_right
                && actual_result == expected_result
        }
        (
            EvalRefML3Judgment::LessThanIs {
                left: actual_left,
                right: actual_right,
                result: actual_result,
            },
            EvalRefML3Judgment::LessThanIs {
                left: expected_left,
                right: expected_right,
                result: expected_result,
            },
        ) => {
            actual_left == expected_left
                && actual_right == expected_right
                && actual_result == expected_result
        }
        _ => false,
    }
}

fn build_location_mapping_env(
    actual: &EvalRefML3Env,
    expected: &EvalRefML3Env,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    actual.0.len() == expected.0.len()
        && actual
            .0
            .iter()
            .zip(expected.0.iter())
            .all(|(actual_binding, expected_binding)| {
                actual_binding.name == expected_binding.name
                    && build_location_mapping_value(
                        &actual_binding.value,
                        &expected_binding.value,
                        forward,
                        reverse,
                    )
            })
}

fn build_location_mapping_store(
    actual: &EvalRefML3Store,
    expected: &EvalRefML3Store,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    actual.0.len() == expected.0.len()
        && actual
            .0
            .iter()
            .zip(expected.0.iter())
            .all(|(actual_entry, expected_entry)| {
                register_location_mapping(
                    &actual_entry.location,
                    &expected_entry.location,
                    forward,
                    reverse,
                ) && build_location_mapping_value(
                    &actual_entry.value,
                    &expected_entry.value,
                    forward,
                    reverse,
                )
            })
}

fn build_location_mapping_expr(
    actual: &EvalRefML3Expr,
    expected: &EvalRefML3Expr,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    match (actual, expected) {
        (EvalRefML3Expr::Int(actual_int), EvalRefML3Expr::Int(expected_int)) => {
            actual_int == expected_int
        }
        (EvalRefML3Expr::Bool(actual_bool), EvalRefML3Expr::Bool(expected_bool)) => {
            actual_bool == expected_bool
        }
        (EvalRefML3Expr::Unit, EvalRefML3Expr::Unit) => true,
        (EvalRefML3Expr::Var(actual_name), EvalRefML3Expr::Var(expected_name)) => {
            actual_name == expected_name
        }
        (EvalRefML3Expr::Loc(actual_loc), EvalRefML3Expr::Loc(expected_loc)) => {
            register_location_mapping(actual_loc, expected_loc, forward, reverse)
        }
        (
            EvalRefML3Expr::BinOp {
                op: actual_op,
                left: actual_left,
                right: actual_right,
            },
            EvalRefML3Expr::BinOp {
                op: expected_op,
                left: expected_left,
                right: expected_right,
            },
        ) => {
            actual_op == expected_op
                && build_location_mapping_expr(actual_left, expected_left, forward, reverse)
                && build_location_mapping_expr(actual_right, expected_right, forward, reverse)
        }
        (
            EvalRefML3Expr::If {
                condition: actual_condition,
                then_branch: actual_then_branch,
                else_branch: actual_else_branch,
            },
            EvalRefML3Expr::If {
                condition: expected_condition,
                then_branch: expected_then_branch,
                else_branch: expected_else_branch,
            },
        ) => {
            build_location_mapping_expr(actual_condition, expected_condition, forward, reverse)
                && build_location_mapping_expr(
                    actual_then_branch,
                    expected_then_branch,
                    forward,
                    reverse,
                )
                && build_location_mapping_expr(
                    actual_else_branch,
                    expected_else_branch,
                    forward,
                    reverse,
                )
        }
        (
            EvalRefML3Expr::Let {
                name: actual_name,
                bound_expr: actual_bound_expr,
                body: actual_body,
            },
            EvalRefML3Expr::Let {
                name: expected_name,
                bound_expr: expected_bound_expr,
                body: expected_body,
            },
        ) => {
            actual_name == expected_name
                && build_location_mapping_expr(
                    actual_bound_expr,
                    expected_bound_expr,
                    forward,
                    reverse,
                )
                && build_location_mapping_expr(actual_body, expected_body, forward, reverse)
        }
        (
            EvalRefML3Expr::LetRec {
                name: actual_name,
                param: actual_param,
                fun_body: actual_fun_body,
                body: actual_body,
            },
            EvalRefML3Expr::LetRec {
                name: expected_name,
                param: expected_param,
                fun_body: expected_fun_body,
                body: expected_body,
            },
        ) => {
            actual_name == expected_name
                && actual_param == expected_param
                && build_location_mapping_expr(actual_fun_body, expected_fun_body, forward, reverse)
                && build_location_mapping_expr(actual_body, expected_body, forward, reverse)
        }
        (
            EvalRefML3Expr::Fun {
                param: actual_param,
                body: actual_body,
            },
            EvalRefML3Expr::Fun {
                param: expected_param,
                body: expected_body,
            },
        ) => {
            actual_param == expected_param
                && build_location_mapping_expr(actual_body, expected_body, forward, reverse)
        }
        (
            EvalRefML3Expr::App {
                func: actual_func,
                arg: actual_arg,
            },
            EvalRefML3Expr::App {
                func: expected_func,
                arg: expected_arg,
            },
        ) => {
            build_location_mapping_expr(actual_func, expected_func, forward, reverse)
                && build_location_mapping_expr(actual_arg, expected_arg, forward, reverse)
        }
        (
            EvalRefML3Expr::Ref { expr: actual_expr },
            EvalRefML3Expr::Ref {
                expr: expected_expr,
            },
        ) => build_location_mapping_expr(actual_expr, expected_expr, forward, reverse),
        (
            EvalRefML3Expr::Deref { expr: actual_expr },
            EvalRefML3Expr::Deref {
                expr: expected_expr,
            },
        ) => build_location_mapping_expr(actual_expr, expected_expr, forward, reverse),
        (
            EvalRefML3Expr::Assign {
                target: actual_target,
                value: actual_value,
            },
            EvalRefML3Expr::Assign {
                target: expected_target,
                value: expected_value,
            },
        ) => {
            build_location_mapping_expr(actual_target, expected_target, forward, reverse)
                && build_location_mapping_expr(actual_value, expected_value, forward, reverse)
        }
        _ => false,
    }
}

fn build_location_mapping_value(
    actual: &EvalRefML3Value,
    expected: &EvalRefML3Value,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    match (actual, expected) {
        (EvalRefML3Value::Int(actual_int), EvalRefML3Value::Int(expected_int)) => {
            actual_int == expected_int
        }
        (EvalRefML3Value::Bool(actual_bool), EvalRefML3Value::Bool(expected_bool)) => {
            actual_bool == expected_bool
        }
        (EvalRefML3Value::Unit, EvalRefML3Value::Unit) => true,
        (EvalRefML3Value::Loc(actual_loc), EvalRefML3Value::Loc(expected_loc)) => {
            register_location_mapping(actual_loc, expected_loc, forward, reverse)
        }
        (
            EvalRefML3Value::Closure {
                env: actual_env,
                param: actual_param,
                body: actual_body,
            },
            EvalRefML3Value::Closure {
                env: expected_env,
                param: expected_param,
                body: expected_body,
            },
        ) => {
            actual_param == expected_param
                && build_location_mapping_env(actual_env, expected_env, forward, reverse)
                && build_location_mapping_expr(actual_body, expected_body, forward, reverse)
        }
        (
            EvalRefML3Value::RecClosure {
                env: actual_env,
                name: actual_name,
                param: actual_param,
                body: actual_body,
            },
            EvalRefML3Value::RecClosure {
                env: expected_env,
                name: expected_name,
                param: expected_param,
                body: expected_body,
            },
        ) => {
            actual_name == expected_name
                && actual_param == expected_param
                && build_location_mapping_env(actual_env, expected_env, forward, reverse)
                && build_location_mapping_expr(actual_body, expected_body, forward, reverse)
        }
        _ => false,
    }
}

fn register_location_mapping(
    actual: &str,
    expected: &str,
    forward: &mut HashMap<String, String>,
    reverse: &mut HashMap<String, String>,
) -> bool {
    match (forward.get(actual), reverse.get(expected)) {
        (Some(mapped), _) if mapped != expected => false,
        (_, Some(mapped)) if mapped != actual => false,
        (Some(_), Some(_)) => true,
        (Some(_), None) => {
            reverse.insert(expected.to_string(), actual.to_string());
            true
        }
        (None, Some(_)) => {
            forward.insert(actual.to_string(), expected.to_string());
            true
        }
        (None, None) => {
            forward.insert(actual.to_string(), expected.to_string());
            reverse.insert(expected.to_string(), actual.to_string());
            true
        }
    }
}

fn rename_derivation_locations(
    derivation: &EvalRefML3Derivation,
    mapping: &HashMap<String, String>,
) -> EvalRefML3Derivation {
    EvalRefML3Derivation {
        span: derivation.span.clone(),
        judgment: rename_judgment_locations(&derivation.judgment, mapping),
        rule_name: derivation.rule_name.clone(),
        subderivations: derivation
            .subderivations
            .iter()
            .map(|sub| rename_derivation_locations(sub, mapping))
            .collect(),
    }
}

fn rename_judgment_locations(
    judgment: &EvalRefML3Judgment,
    mapping: &HashMap<String, String>,
) -> EvalRefML3Judgment {
    match judgment {
        EvalRefML3Judgment::EvalTo {
            env,
            expr,
            store,
            value,
            result_store,
        } => EvalRefML3Judgment::EvalTo {
            env: rename_env_locations(env, mapping),
            expr: rename_expr_locations(expr, mapping),
            store: rename_store_locations(store, mapping),
            value: rename_value_locations(value, mapping),
            result_store: rename_store_locations(result_store, mapping),
        },
        EvalRefML3Judgment::PlusIs {
            left,
            right,
            result,
        } => EvalRefML3Judgment::PlusIs {
            left: *left,
            right: *right,
            result: *result,
        },
        EvalRefML3Judgment::MinusIs {
            left,
            right,
            result,
        } => EvalRefML3Judgment::MinusIs {
            left: *left,
            right: *right,
            result: *result,
        },
        EvalRefML3Judgment::TimesIs {
            left,
            right,
            result,
        } => EvalRefML3Judgment::TimesIs {
            left: *left,
            right: *right,
            result: *result,
        },
        EvalRefML3Judgment::LessThanIs {
            left,
            right,
            result,
        } => EvalRefML3Judgment::LessThanIs {
            left: *left,
            right: *right,
            result: *result,
        },
    }
}

fn rename_env_locations(env: &EvalRefML3Env, mapping: &HashMap<String, String>) -> EvalRefML3Env {
    EvalRefML3Env(
        env.0
            .iter()
            .map(|binding| EvalRefML3Binding {
                name: binding.name.clone(),
                value: rename_value_locations(&binding.value, mapping),
            })
            .collect(),
    )
}

fn rename_store_locations(
    store: &EvalRefML3Store,
    mapping: &HashMap<String, String>,
) -> EvalRefML3Store {
    EvalRefML3Store(
        store
            .0
            .iter()
            .map(|entry| super::syntax::EvalRefML3StoreEntry {
                location: rename_location(&entry.location, mapping),
                value: rename_value_locations(&entry.value, mapping),
            })
            .collect(),
    )
}

fn rename_expr_locations(
    expr: &EvalRefML3Expr,
    mapping: &HashMap<String, String>,
) -> EvalRefML3Expr {
    match expr {
        EvalRefML3Expr::Int(value) => EvalRefML3Expr::Int(*value),
        EvalRefML3Expr::Bool(value) => EvalRefML3Expr::Bool(*value),
        EvalRefML3Expr::Unit => EvalRefML3Expr::Unit,
        EvalRefML3Expr::Var(name) => EvalRefML3Expr::Var(name.clone()),
        EvalRefML3Expr::Loc(location) => EvalRefML3Expr::Loc(rename_location(location, mapping)),
        EvalRefML3Expr::BinOp { op, left, right } => EvalRefML3Expr::BinOp {
            op: *op,
            left: Box::new(rename_expr_locations(left, mapping)),
            right: Box::new(rename_expr_locations(right, mapping)),
        },
        EvalRefML3Expr::If {
            condition,
            then_branch,
            else_branch,
        } => EvalRefML3Expr::If {
            condition: Box::new(rename_expr_locations(condition, mapping)),
            then_branch: Box::new(rename_expr_locations(then_branch, mapping)),
            else_branch: Box::new(rename_expr_locations(else_branch, mapping)),
        },
        EvalRefML3Expr::Let {
            name,
            bound_expr,
            body,
        } => EvalRefML3Expr::Let {
            name: name.clone(),
            bound_expr: Box::new(rename_expr_locations(bound_expr, mapping)),
            body: Box::new(rename_expr_locations(body, mapping)),
        },
        EvalRefML3Expr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => EvalRefML3Expr::LetRec {
            name: name.clone(),
            param: param.clone(),
            fun_body: Box::new(rename_expr_locations(fun_body, mapping)),
            body: Box::new(rename_expr_locations(body, mapping)),
        },
        EvalRefML3Expr::Fun { param, body } => EvalRefML3Expr::Fun {
            param: param.clone(),
            body: Box::new(rename_expr_locations(body, mapping)),
        },
        EvalRefML3Expr::App { func, arg } => EvalRefML3Expr::App {
            func: Box::new(rename_expr_locations(func, mapping)),
            arg: Box::new(rename_expr_locations(arg, mapping)),
        },
        EvalRefML3Expr::Ref { expr } => EvalRefML3Expr::Ref {
            expr: Box::new(rename_expr_locations(expr, mapping)),
        },
        EvalRefML3Expr::Deref { expr } => EvalRefML3Expr::Deref {
            expr: Box::new(rename_expr_locations(expr, mapping)),
        },
        EvalRefML3Expr::Assign { target, value } => EvalRefML3Expr::Assign {
            target: Box::new(rename_expr_locations(target, mapping)),
            value: Box::new(rename_expr_locations(value, mapping)),
        },
    }
}

fn rename_value_locations(
    value: &EvalRefML3Value,
    mapping: &HashMap<String, String>,
) -> EvalRefML3Value {
    match value {
        EvalRefML3Value::Int(value_int) => EvalRefML3Value::Int(*value_int),
        EvalRefML3Value::Bool(value_bool) => EvalRefML3Value::Bool(*value_bool),
        EvalRefML3Value::Unit => EvalRefML3Value::Unit,
        EvalRefML3Value::Loc(location) => EvalRefML3Value::Loc(rename_location(location, mapping)),
        EvalRefML3Value::Closure { env, param, body } => EvalRefML3Value::Closure {
            env: rename_env_locations(env, mapping),
            param: param.clone(),
            body: rename_expr_locations(body, mapping),
        },
        EvalRefML3Value::RecClosure {
            env,
            name,
            param,
            body,
        } => EvalRefML3Value::RecClosure {
            env: rename_env_locations(env, mapping),
            name: name.clone(),
            param: param.clone(),
            body: rename_expr_locations(body, mapping),
        },
    }
}

fn rename_location(location: &str, mapping: &HashMap<String, String>) -> String {
    mapping
        .get(location)
        .cloned()
        .unwrap_or_else(|| location.to_string())
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

fn derivation(
    judgment: EvalRefML3Judgment,
    rule_name: &str,
    subderivations: Vec<EvalRefML3Derivation>,
) -> EvalRefML3Derivation {
    EvalRefML3Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

fn non_derivable_judgment_error(
    actual: &EvalRefML3Judgment,
    expected: Option<EvalRefML3Judgment>,
) -> CheckError {
    let detail = match expected {
        Some(expected) => {
            format!("expected: {expected}, actual: {actual}; fix: replace value/store")
        }
        None => "fix: rewrite the judgment into a derivable form".to_string(),
    };
    CheckError::rule_violation(format!(
        "judgment is not derivable in EvalRefML3 ({detail})"
    ))
}

#[cfg(test)]
mod tests {
    use super::prove_judgment;
    use crate::core::Game;
    use crate::games::eval_ref_ml3::parser::parse_judgment_source;
    use crate::games::eval_ref_ml3::EvalRefML3Game;

    #[test]
    fn proves_ref_judgment() {
        let judgment = parse_judgment_source("|- ref 1 / () evalto @l0 / @l0 = 1")
            .expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "E-Ref");
    }

    #[test]
    fn rejects_non_derivable_store() {
        let judgment =
            parse_judgment_source("|- ref 1 / () evalto @l0 / ()").expect("judgment should parse");
        let err = prove_judgment(judgment).expect_err("judgment should not be derivable");
        assert!(err
            .message()
            .contains("judgment is not derivable in EvalRefML3"));
    }

    #[test]
    fn proves_if_judgment() {
        let source =
            "@l = 2 / x = @l, y = false |- if y then x := !x + 1 else !x evalto 2 / @l = 2";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "E-IfF");
    }

    #[test]
    fn proves_fun_application_judgment() {
        let source = "@l = 0 / incr = ()[fun x -> x := !x + 1], x = @l |- incr x evalto 1 / @l = 1";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "E-App");
    }

    #[test]
    fn proves_let_rec_judgment() {
        let source = "|- let rec f = fun x -> x in f 1 evalto 1";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "E-LetRec");
        assert_eq!(derivation.subderivations[0].rule_name, "E-AppRec");
    }

    #[test]
    fn proves_if_with_lt_times_and_minus_judgment() {
        let source = "|- if 1 < 2 then 3 * 4 - 1 else 0 evalto 11";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "E-IfT");
        assert_eq!(derivation.subderivations[0].rule_name, "E-Lt");
        assert_eq!(derivation.subderivations[1].rule_name, "E-Minus");
    }

    #[test]
    fn proves_b_plus_judgment() {
        let judgment =
            parse_judgment_source("2 plus 3 is 5").expect("judgment should parse in B-* mode");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        assert_eq!(derivation.rule_name, "B-Plus");
    }

    #[test]
    fn keeps_parenthesized_negative_application_argument_in_output() {
        let judgment = parse_judgment_source("|- let f = fun x -> x in f (-2) evalto -2")
            .expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        let text = derivation.to_string();

        assert!(text.contains("f (-2)"));
        assert!(!text.contains("f -2"));
    }

    #[test]
    fn keeps_parenthesized_binop_application_argument_in_output() {
        let judgment = parse_judgment_source("|- let f = fun x -> x in f (1 + 2) evalto 3")
            .expect("judgment should parse");
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        let text = derivation.to_string();

        assert!(text.contains("f (1 + 2)"));
        assert!(!text.contains("f 1 + 2"));
    }

    #[test]
    fn prover_output_round_trips_with_store_effects() {
        let source = "|- let c = let x = ref 0 in fun y -> if y then x := !x + 1 else !x in let y = c true in let y = c true in c false evalto 2 / @l = 2";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        let expected_root = judgment.to_string();
        let derivation = prove_judgment(judgment).expect("judgment should be derivable");
        let report = EvalRefML3Game
            .check(&derivation.to_string())
            .expect("prover output should be checker-valid");
        assert_eq!(report.summary, expected_root);
    }
}
