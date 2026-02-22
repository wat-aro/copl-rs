use crate::core::{CheckError, SourceSpan};

use super::syntax::{Store, WhileAExp, WhileBExp, WhileCom, WhileDerivation, WhileJudgment};

const MAX_COMMAND_STEPS: usize = 20_000;

pub(super) fn prove_judgment(judgment: WhileJudgment) -> Result<WhileDerivation, CheckError> {
    let mut fuel = MAX_COMMAND_STEPS;
    let derivation = match &judgment {
        WhileJudgment::AEval { store, expr, .. } => prove_aexp(store, expr)?,
        WhileJudgment::BEval { store, expr, .. } => prove_bexp(store, expr)?,
        WhileJudgment::Changes { command, from, .. } => prove_command(command, from, &mut fuel)?,
    };

    if derivation.judgment == judgment {
        Ok(derivation)
    } else {
        Err(non_derivable_judgment_error(
            &judgment,
            &derivation.judgment,
        ))
    }
}

fn prove_aexp(store: &Store, expr: &WhileAExp) -> Result<WhileDerivation, CheckError> {
    match expr {
        WhileAExp::Int(value) => Ok(derivation(
            WhileJudgment::AEval {
                store: store.clone(),
                expr: expr.clone(),
                value: *value,
            },
            "A-Const",
            Vec::new(),
        )),
        WhileAExp::Var(name) => {
            let Some(value) = store.lookup(name) else {
                return Err(CheckError::rule_violation(format!(
                    "judgment is not derivable in While (actual: {store} |- {expr} evalto ?; reason: variable '{name}' is unbound; fix: add '{name}' binding to the store)"
                )));
            };
            Ok(derivation(
                WhileJudgment::AEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "A-Var",
                Vec::new(),
            ))
        }
        WhileAExp::Plus(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let i1 = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let i2 = as_a_value(&second.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let result = i1 + i2;
            Ok(derivation(
                WhileJudgment::AEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value: result,
                },
                "A-Plus",
                vec![first, second],
            ))
        }
        WhileAExp::Minus(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let i1 = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let i2 = as_a_value(&second.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let result = i1 - i2;
            Ok(derivation(
                WhileJudgment::AEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value: result,
                },
                "A-Minus",
                vec![first, second],
            ))
        }
        WhileAExp::Times(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let i1 = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let i2 = as_a_value(&second.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let result = i1 * i2;
            Ok(derivation(
                WhileJudgment::AEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value: result,
                },
                "A-Times",
                vec![first, second],
            ))
        }
    }
}

fn prove_bexp(store: &Store, expr: &WhileBExp) -> Result<WhileDerivation, CheckError> {
    match expr {
        WhileBExp::Bool(value) => Ok(derivation(
            WhileJudgment::BEval {
                store: store.clone(),
                expr: expr.clone(),
                value: *value,
            },
            "B-Const",
            Vec::new(),
        )),
        WhileBExp::Not(inner) => {
            let premise = prove_bexp(store, inner)?;
            let value = !as_b_value(&premise.judgment)
                .expect("prove_bexp should always produce a boolean judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-Not",
                vec![premise],
            ))
        }
        WhileBExp::And(left, right) => {
            let first = prove_bexp(store, left)?;
            let second = prove_bexp(store, right)?;
            let value = as_b_value(&first.judgment)
                .expect("prove_bexp should always produce a boolean judgment")
                && as_b_value(&second.judgment)
                    .expect("prove_bexp should always produce a boolean judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-And",
                vec![first, second],
            ))
        }
        WhileBExp::Or(left, right) => {
            let first = prove_bexp(store, left)?;
            let second = prove_bexp(store, right)?;
            let value = as_b_value(&first.judgment)
                .expect("prove_bexp should always produce a boolean judgment")
                || as_b_value(&second.judgment)
                    .expect("prove_bexp should always produce a boolean judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-Or",
                vec![first, second],
            ))
        }
        WhileBExp::Lt(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let value = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment")
                < as_a_value(&second.judgment)
                    .expect("prove_aexp should always produce an arithmetic judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-Lt",
                vec![first, second],
            ))
        }
        WhileBExp::Eq(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let value = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment")
                == as_a_value(&second.judgment)
                    .expect("prove_aexp should always produce an arithmetic judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-Eq",
                vec![first, second],
            ))
        }
        WhileBExp::Le(left, right) => {
            let first = prove_aexp(store, left)?;
            let second = prove_aexp(store, right)?;
            let value = as_a_value(&first.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment")
                <= as_a_value(&second.judgment)
                    .expect("prove_aexp should always produce an arithmetic judgment");
            Ok(derivation(
                WhileJudgment::BEval {
                    store: store.clone(),
                    expr: expr.clone(),
                    value,
                },
                "B-Le",
                vec![first, second],
            ))
        }
    }
}

fn prove_command(
    command: &WhileCom,
    from: &Store,
    fuel: &mut usize,
) -> Result<WhileDerivation, CheckError> {
    if *fuel == 0 {
        return Err(CheckError::rule_violation(format!(
            "judgment is not derivable in While (actual: {command} changes {from} to ?; reason: command evaluation did not terminate within {MAX_COMMAND_STEPS} steps; fix: check loop guard/body or use a terminating judgment)"
        )));
    }
    *fuel -= 1;

    match command {
        WhileCom::Skip => Ok(derivation(
            WhileJudgment::Changes {
                command: command.clone(),
                from: from.clone(),
                to: from.clone(),
            },
            "C-Skip",
            Vec::new(),
        )),
        WhileCom::Assign { name, expr } => {
            let premise = prove_aexp(from, expr)?;
            let value = as_a_value(&premise.judgment)
                .expect("prove_aexp should always produce an arithmetic judgment");
            let to = from.update(name, value);
            Ok(derivation(
                WhileJudgment::Changes {
                    command: command.clone(),
                    from: from.clone(),
                    to,
                },
                "C-Assign",
                vec![premise],
            ))
        }
        WhileCom::Seq(first_command, second_command) => {
            let first = prove_command(first_command, from, fuel)?;
            let mid = as_changes_to(&first.judgment)
                .expect("prove_command should always produce a command judgment")
                .clone();
            let second = prove_command(second_command, &mid, fuel)?;
            let to = as_changes_to(&second.judgment)
                .expect("prove_command should always produce a command judgment")
                .clone();
            Ok(derivation(
                WhileJudgment::Changes {
                    command: command.clone(),
                    from: from.clone(),
                    to,
                },
                "C-Seq",
                vec![first, second],
            ))
        }
        WhileCom::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_derivation = prove_bexp(from, cond)?;
            let cond_value = as_b_value(&cond_derivation.judgment)
                .expect("prove_bexp should always produce a boolean judgment");
            if cond_value {
                let then_derivation = prove_command(then_branch, from, fuel)?;
                let to = as_changes_to(&then_derivation.judgment)
                    .expect("prove_command should always produce a command judgment")
                    .clone();
                Ok(derivation(
                    WhileJudgment::Changes {
                        command: command.clone(),
                        from: from.clone(),
                        to,
                    },
                    "C-IfT",
                    vec![cond_derivation, then_derivation],
                ))
            } else {
                let else_derivation = prove_command(else_branch, from, fuel)?;
                let to = as_changes_to(&else_derivation.judgment)
                    .expect("prove_command should always produce a command judgment")
                    .clone();
                Ok(derivation(
                    WhileJudgment::Changes {
                        command: command.clone(),
                        from: from.clone(),
                        to,
                    },
                    "C-IfF",
                    vec![cond_derivation, else_derivation],
                ))
            }
        }
        WhileCom::While { cond, body } => {
            let cond_derivation = prove_bexp(from, cond)?;
            let cond_value = as_b_value(&cond_derivation.judgment)
                .expect("prove_bexp should always produce a boolean judgment");

            if cond_value {
                let body_derivation = prove_command(body, from, fuel)?;
                let mid = as_changes_to(&body_derivation.judgment)
                    .expect("prove_command should always produce a command judgment")
                    .clone();
                let loop_derivation = prove_command(command, &mid, fuel)?;
                let to = as_changes_to(&loop_derivation.judgment)
                    .expect("prove_command should always produce a command judgment")
                    .clone();
                Ok(derivation(
                    WhileJudgment::Changes {
                        command: command.clone(),
                        from: from.clone(),
                        to,
                    },
                    "C-WhileT",
                    vec![cond_derivation, body_derivation, loop_derivation],
                ))
            } else {
                Ok(derivation(
                    WhileJudgment::Changes {
                        command: command.clone(),
                        from: from.clone(),
                        to: from.clone(),
                    },
                    "C-WhileF",
                    vec![cond_derivation],
                ))
            }
        }
    }
}

fn as_a_value(judgment: &WhileJudgment) -> Option<i64> {
    let WhileJudgment::AEval { value, .. } = judgment else {
        return None;
    };
    Some(*value)
}

fn as_b_value(judgment: &WhileJudgment) -> Option<bool> {
    let WhileJudgment::BEval { value, .. } = judgment else {
        return None;
    };
    Some(*value)
}

fn as_changes_to(judgment: &WhileJudgment) -> Option<&Store> {
    let WhileJudgment::Changes { to, .. } = judgment else {
        return None;
    };
    Some(to)
}

fn non_derivable_judgment_error(expected: &WhileJudgment, actual: &WhileJudgment) -> CheckError {
    let fix = match (expected, actual) {
        (
            WhileJudgment::AEval {
                store: expected_store,
                expr: expected_expr,
                value: expected_value,
            },
            WhileJudgment::AEval {
                store: actual_store,
                expr: actual_expr,
                value: actual_value,
            },
        ) if expected_store == actual_store && expected_expr == actual_expr => {
            if expected_value == actual_value {
                "fix: check the arithmetic judgment form".to_string()
            } else {
                format!("fix: replace value with {actual_value}")
            }
        }
        (
            WhileJudgment::BEval {
                store: expected_store,
                expr: expected_expr,
                value: expected_value,
            },
            WhileJudgment::BEval {
                store: actual_store,
                expr: actual_expr,
                value: actual_value,
            },
        ) if expected_store == actual_store && expected_expr == actual_expr => {
            if expected_value == actual_value {
                "fix: check the boolean judgment form".to_string()
            } else {
                format!("fix: replace value with {actual_value}")
            }
        }
        (
            WhileJudgment::Changes {
                command: expected_command,
                from: expected_from,
                to: expected_to,
            },
            WhileJudgment::Changes {
                command: actual_command,
                from: actual_from,
                to: actual_to,
            },
        ) if expected_command == actual_command && expected_from == actual_from => {
            if expected_to == actual_to {
                "fix: check command judgment form".to_string()
            } else {
                format!("fix: replace target store with {actual_to}")
            }
        }
        _ => "fix: rewrite the judgment to match derivation semantics".to_string(),
    };

    CheckError::rule_violation(format!(
        "judgment is not derivable in While (expected: {actual}, actual: {expected}; {fix})"
    ))
}

fn derivation(
    judgment: WhileJudgment,
    rule_name: &str,
    subderivations: Vec<WhileDerivation>,
) -> WhileDerivation {
    WhileDerivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::prove_judgment;
    use crate::core::Game;
    use crate::games::while_lang::checker::WhileGame;
    use crate::games::while_lang::parser::{parse_judgment_source, parse_source};

    #[test]
    fn proves_exam_judgments_151_to_160() {
        for fixture in [
            include_str!("../../../copl/151.exam.copl"),
            include_str!("../../../copl/152.exam.copl"),
            include_str!("../../../copl/153.exam.copl"),
            include_str!("../../../copl/154.exam.copl"),
            include_str!("../../../copl/155.exam.copl"),
            include_str!("../../../copl/156.exam.copl"),
            include_str!("../../../copl/157.exam.copl"),
            include_str!("../../../copl/158.exam.copl"),
            include_str!("../../../copl/159.exam.copl"),
            include_str!("../../../copl/160.exam.copl"),
        ] {
            let judgment = parse_judgment_source(fixture).expect("fixture should parse");
            let derivation =
                prove_judgment(judgment.clone()).expect("judgment should be derivable");

            assert_eq!(derivation.judgment, judgment);

            let rendered = derivation.to_string();
            let reparsed = parse_source(&rendered).expect("rendered derivation should parse");
            assert_eq!(reparsed.judgment, judgment);

            let report = WhileGame
                .check(&rendered)
                .expect("checker should accept prover output");
            assert_eq!(report.summary, judgment.to_string());
        }
    }

    #[test]
    fn rejects_non_derivable_judgment() {
        let judgment =
            parse_judgment_source("x := 1 changes x = 0 to x = 0").expect("judgment should parse");

        let err = prove_judgment(judgment).expect_err("judgment should be rejected");

        assert!(err.message().contains("judgment is not derivable in While"));
        assert!(err.message().contains(
            "expected: x := 1 changes x = 0 to x = 1, actual: x := 1 changes x = 0 to x = 0"
        ));
        assert!(err
            .message()
            .contains("fix: replace target store with x = 1"));
    }
}
