use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    EvalRefML3Binding, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr, EvalRefML3Judgment,
    EvalRefML3Store, EvalRefML3Value,
};

pub(super) fn prove_judgment(
    judgment: EvalRefML3Judgment,
) -> Result<EvalRefML3Derivation, CheckError> {
    let EvalRefML3Judgment::EvalTo {
        env,
        expr,
        store,
        value: expected_value,
        result_store: expected_store,
    } = &judgment;

    let Some(actual) = prove_expr(env, expr, store) else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };

    let Some((_, _, _, actual_value, actual_store)) = as_eval_to(&actual.judgment) else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };

    if actual_value == expected_value && actual_store == expected_store {
        Ok(actual)
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
                    value: EvalRefML3Value::Unit,
                    result_store: updated_store,
                },
                "E-Assign",
                vec![first, second],
            ))
        }
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
    use crate::games::eval_ref_ml3::parser::parse_judgment_source;

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
}
