use crate::core::{CheckError, SourceSpan};

use super::syntax::{
    NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Derivation, NamelessML3Env,
    NamelessML3Judgment,
};

pub(super) fn prove_judgment(
    judgment: NamelessML3Judgment,
) -> Result<NamelessML3Derivation, CheckError> {
    let NamelessML3Judgment::Translates {
        env,
        named,
        nameless: expected_nameless,
    } = &judgment;

    let Some(actual) = prove_translates(env, named) else {
        return Err(non_derivable_judgment_error(&judgment, None));
    };
    let actual_nameless = as_nameless(&actual.judgment);

    if actual_nameless == expected_nameless {
        Ok(actual)
    } else {
        Err(non_derivable_judgment_error(
            &judgment,
            Some(actual.judgment.clone()),
        ))
    }
}

fn prove_translates(env: &NamelessML3Env, named: &NamedExpr) -> Option<NamelessML3Derivation> {
    match named {
        NamedExpr::Int(value) => Some(derivation(
            translates_judgment(env.clone(), named.clone(), NamelessExpr::Int(*value)),
            "Tr-Int",
            Vec::new(),
        )),
        NamedExpr::Bool(value) => Some(derivation(
            translates_judgment(env.clone(), named.clone(), NamelessExpr::Bool(*value)),
            "Tr-Bool",
            Vec::new(),
        )),
        NamedExpr::Var(name) => prove_var(env, name),
        NamedExpr::BinOp { op, left, right } => {
            let left_derivation = prove_translates(env, left.as_ref())?;
            let right_derivation = prove_translates(env, right.as_ref())?;
            let left_nameless = as_nameless(&left_derivation.judgment).clone();
            let right_nameless = as_nameless(&right_derivation.judgment).clone();
            let nameless = NamelessExpr::BinOp {
                op: *op,
                left: Box::new(left_nameless),
                right: Box::new(right_nameless),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                binop_rule_name(*op),
                vec![left_derivation, right_derivation],
            ))
        }
        NamedExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_derivation = prove_translates(env, condition.as_ref())?;
            let then_derivation = prove_translates(env, then_branch.as_ref())?;
            let else_derivation = prove_translates(env, else_branch.as_ref())?;
            let nameless = NamelessExpr::If {
                condition: Box::new(as_nameless(&condition_derivation.judgment).clone()),
                then_branch: Box::new(as_nameless(&then_derivation.judgment).clone()),
                else_branch: Box::new(as_nameless(&else_derivation.judgment).clone()),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                "Tr-If",
                vec![condition_derivation, then_derivation, else_derivation],
            ))
        }
        NamedExpr::Let {
            name,
            bound_expr,
            body,
        } => {
            let bound_derivation = prove_translates(env, bound_expr.as_ref())?;
            let extended_env = push_var(env, name);
            let body_derivation = prove_translates(&extended_env, body.as_ref())?;
            let nameless = NamelessExpr::Let {
                bound_expr: Box::new(as_nameless(&bound_derivation.judgment).clone()),
                body: Box::new(as_nameless(&body_derivation.judgment).clone()),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                "Tr-Let",
                vec![bound_derivation, body_derivation],
            ))
        }
        NamedExpr::Fun { param, body } => {
            let extended_env = push_var(env, param);
            let body_derivation = prove_translates(&extended_env, body.as_ref())?;
            let nameless = NamelessExpr::Fun {
                body: Box::new(as_nameless(&body_derivation.judgment).clone()),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                "Tr-Fun",
                vec![body_derivation],
            ))
        }
        NamedExpr::App { func, arg } => {
            let func_derivation = prove_translates(env, func.as_ref())?;
            let arg_derivation = prove_translates(env, arg.as_ref())?;
            let nameless = NamelessExpr::App {
                func: Box::new(as_nameless(&func_derivation.judgment).clone()),
                arg: Box::new(as_nameless(&arg_derivation.judgment).clone()),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                "Tr-App",
                vec![func_derivation, arg_derivation],
            ))
        }
        NamedExpr::LetRec {
            name,
            param,
            fun_body,
            body,
        } => {
            let fun_env = push_var(&push_var(env, name), param);
            let fun_derivation = prove_translates(&fun_env, fun_body.as_ref())?;
            let body_env = push_var(env, name);
            let body_derivation = prove_translates(&body_env, body.as_ref())?;
            let nameless = NamelessExpr::LetRec {
                fun_body: Box::new(as_nameless(&fun_derivation.judgment).clone()),
                body: Box::new(as_nameless(&body_derivation.judgment).clone()),
            };
            Some(derivation(
                translates_judgment(env.clone(), named.clone(), nameless),
                "Tr-LetRec",
                vec![fun_derivation, body_derivation],
            ))
        }
    }
}

fn binop_rule_name(op: NamelessML3BinOp) -> &'static str {
    match op {
        NamelessML3BinOp::Plus => "Tr-Plus",
        NamelessML3BinOp::Minus => "Tr-Minus",
        NamelessML3BinOp::Times => "Tr-Times",
        NamelessML3BinOp::Lt => "Tr-Lt",
    }
}

fn prove_var(env: &NamelessML3Env, name: &str) -> Option<NamelessML3Derivation> {
    let (prefix, nearest) = split_last_var(env)?;

    if nearest == name {
        return Some(derivation(
            translates_judgment(
                env.clone(),
                NamedExpr::Var(name.to_string()),
                NamelessExpr::Index(1),
            ),
            "Tr-Var1",
            Vec::new(),
        ));
    }

    let prefix_env = NamelessML3Env(prefix.to_vec());
    let premise = prove_translates(&prefix_env, &NamedExpr::Var(name.to_string()))?;
    let NamelessExpr::Index(index) = as_nameless(&premise.judgment) else {
        return None;
    };
    let next_index = index.checked_add(1)?;
    Some(derivation(
        translates_judgment(
            env.clone(),
            NamedExpr::Var(name.to_string()),
            NamelessExpr::Index(next_index),
        ),
        "Tr-Var2",
        vec![premise],
    ))
}

fn split_last_var(env: &NamelessML3Env) -> Option<(&[String], &str)> {
    let (last, prefix) = env.0.split_last()?;
    Some((prefix, last.as_str()))
}

fn push_var(env: &NamelessML3Env, name: &str) -> NamelessML3Env {
    let mut vars = env.0.clone();
    vars.push(name.to_string());
    NamelessML3Env(vars)
}

fn translates_judgment(
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

fn as_nameless(judgment: &NamelessML3Judgment) -> &NamelessExpr {
    let NamelessML3Judgment::Translates { nameless, .. } = judgment;
    nameless
}

fn non_derivable_judgment_error(
    actual: &NamelessML3Judgment,
    expected: Option<NamelessML3Judgment>,
) -> CheckError {
    match expected {
        Some(expected) => CheckError::rule_violation(format!(
            "judgment is not derivable in NamelessML3 (expected: {expected}, actual: {actual}; {})",
            fix_message(actual, &expected)
        )),
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in NamelessML3 (actual: {actual}; fix: check environment bindings and translation target consistency)"
        )),
    }
}

fn fix_message(actual: &NamelessML3Judgment, expected: &NamelessML3Judgment) -> String {
    let (
        NamelessML3Judgment::Translates {
            env: actual_env,
            named: actual_named,
            nameless: actual_nameless,
        },
        NamelessML3Judgment::Translates {
            env: expected_env,
            named: expected_named,
            nameless: expected_nameless,
        },
    ) = (actual, expected);

    if actual_env == expected_env && actual_named == expected_named {
        if actual_nameless == expected_nameless {
            "fix: check environment bindings and translation target consistency".to_string()
        } else {
            format!("fix: replace nameless expression with {expected_nameless}")
        }
    } else {
        "fix: check environment bindings and translation target consistency".to_string()
    }
}

fn derivation(
    judgment: NamelessML3Judgment,
    rule_name: &str,
    subderivations: Vec<NamelessML3Derivation>,
) -> NamelessML3Derivation {
    NamelessML3Derivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::{prove_judgment, NamelessML3Derivation};
    use crate::games::nameless_ml3::syntax::{
        NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Env, NamelessML3Judgment,
    };

    #[test]
    fn proves_let_judgment_with_tr_let() {
        let derivation = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env::default(),
            named: NamedExpr::Let {
                name: "x".to_string(),
                bound_expr: Box::new(NamedExpr::Int(3)),
                body: Box::new(NamedExpr::Var("x".to_string())),
            },
            nameless: NamelessExpr::Let {
                bound_expr: Box::new(NamelessExpr::Int(3)),
                body: Box::new(NamelessExpr::Index(1)),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "Tr-Let");
        assert_eq!(derivation.subderivations.len(), 2);
        assert!(contains_rule(&derivation, "Tr-Var1"));
    }

    #[test]
    fn proves_var_with_nearest_binding_by_tr_var1() {
        let derivation = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env(vec!["x".to_string(), "x".to_string()]),
            named: NamedExpr::Var("x".to_string()),
            nameless: NamelessExpr::Index(1),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "Tr-Var1");
        assert!(derivation.subderivations.is_empty());
    }

    #[test]
    fn proves_var_with_outer_binding_by_tr_var2() {
        let derivation = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env(vec!["x".to_string(), "y".to_string()]),
            named: NamedExpr::Var("x".to_string()),
            nameless: NamelessExpr::Index(2),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "Tr-Var2");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "Tr-Var1");
    }

    #[test]
    fn proves_let_rec_judgment() {
        let derivation = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env::default(),
            named: NamedExpr::LetRec {
                name: "f".to_string(),
                param: "x".to_string(),
                fun_body: Box::new(NamedExpr::Var("f".to_string())),
                body: Box::new(NamedExpr::Var("f".to_string())),
            },
            nameless: NamelessExpr::LetRec {
                fun_body: Box::new(NamelessExpr::Index(2)),
                body: Box::new(NamelessExpr::Index(1)),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "Tr-LetRec");
        assert_eq!(derivation.subderivations.len(), 2);
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let err = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env::default(),
            named: NamedExpr::Let {
                name: "x".to_string(),
                bound_expr: Box::new(NamedExpr::Int(3)),
                body: Box::new(NamedExpr::Var("x".to_string())),
            },
            nameless: NamelessExpr::Let {
                bound_expr: Box::new(NamelessExpr::Int(3)),
                body: Box::new(NamelessExpr::Index(2)),
            },
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in NamelessML3"));
        assert!(err
            .message()
            .contains("expected: |- let x = 3 in x ==> let . = 3 in #1, actual: |- let x = 3 in x ==> let . = 3 in #2"));
        assert!(err
            .message()
            .contains("fix: replace nameless expression with let . = 3 in #1"));
    }

    #[test]
    fn rejects_unbound_variable_judgment() {
        let err = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env::default(),
            named: NamedExpr::Var("x".to_string()),
            nameless: NamelessExpr::Index(1),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in NamelessML3"));
        assert!(err.message().contains("actual: |- x ==> #1"));
        assert!(err
            .message()
            .contains("fix: check environment bindings and translation target consistency"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_062() {
        let expected =
            parse_source(include_str!("../../../copl/062.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    #[test]
    fn proves_if_and_binop_judgment() {
        let derivation = prove_judgment(NamelessML3Judgment::Translates {
            env: NamelessML3Env(vec!["x".to_string(), "y".to_string()]),
            named: NamedExpr::If {
                condition: Box::new(NamedExpr::Var("x".to_string())),
                then_branch: Box::new(NamedExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(NamedExpr::Var("y".to_string())),
                    right: Box::new(NamedExpr::Int(1)),
                }),
                else_branch: Box::new(NamedExpr::BinOp {
                    op: NamelessML3BinOp::Minus,
                    left: Box::new(NamedExpr::Var("y".to_string())),
                    right: Box::new(NamedExpr::Int(1)),
                }),
            },
            nameless: NamelessExpr::If {
                condition: Box::new(NamelessExpr::Index(2)),
                then_branch: Box::new(NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(NamelessExpr::Index(1)),
                    right: Box::new(NamelessExpr::Int(1)),
                }),
                else_branch: Box::new(NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Minus,
                    left: Box::new(NamelessExpr::Index(1)),
                    right: Box::new(NamelessExpr::Int(1)),
                }),
            },
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "Tr-If");
        assert!(contains_rule(&derivation, "Tr-Plus"));
        assert!(contains_rule(&derivation, "Tr-Minus"));
    }

    fn contains_rule(derivation: &NamelessML3Derivation, rule_name: &str) -> bool {
        derivation.rule_name == rule_name
            || derivation
                .subderivations
                .iter()
                .any(|sub| contains_rule(sub, rule_name))
    }

    fn assert_same_shape(actual: &NamelessML3Derivation, expected: &NamelessML3Derivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
