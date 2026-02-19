use std::collections::VecDeque;

use crate::core::{CheckError, SourceSpan};

use super::syntax::{NatTerm, ReduceNatExpDerivation, ReduceNatExpExpr, ReduceNatExpJudgment};

pub(super) fn prove_judgment(
    judgment: ReduceNatExpJudgment,
) -> Result<ReduceNatExpDerivation, CheckError> {
    let derivation = match &judgment {
        ReduceNatExpJudgment::PlusIs {
            left,
            right,
            result,
        } => {
            let Some(derivation) = prove_plus(left, right, result) else {
                return Err(non_derivable_judgment_error(&judgment));
            };
            derivation
        }
        ReduceNatExpJudgment::TimesIs {
            left,
            right,
            result,
        } => {
            let Some(derivation) = prove_times(left, right, result) else {
                return Err(non_derivable_judgment_error(&judgment));
            };
            derivation
        }
        ReduceNatExpJudgment::ReducesTo { from, to } => {
            let candidates = one_step_reductions(from);
            if let Some(derivation) = find_matching_one_step_derivation(&candidates, to) {
                derivation
            } else {
                return Err(non_derivable_reduces_error(
                    &judgment,
                    from,
                    to,
                    &candidates,
                ));
            }
        }
        ReduceNatExpJudgment::DeterministicReducesTo { from, to } => {
            let expected_step = deterministic_step(from);
            if let Some(derivation) = expected_step {
                let actual_to = one_step_to(&derivation)
                    .expect("deterministic_step should always build a one-step judgment");
                if actual_to == to {
                    derivation
                } else {
                    return Err(non_derivable_deterministic_error(
                        &judgment,
                        from,
                        to,
                        Some(actual_to),
                    ));
                }
            } else {
                return Err(non_derivable_deterministic_error(&judgment, from, to, None));
            }
        }
        ReduceNatExpJudgment::MultiReducesTo { from, to } => {
            let Some(path) = find_multi_reduction_path(from, to) else {
                return Err(non_derivable_multi_error(&judgment, from, to));
            };
            if path.is_empty() {
                derivation(judgment.clone(), "MR-Zero", Vec::new())
            } else {
                multi_derivation_from_steps(&path)
            }
        }
    };

    Ok(derivation)
}

fn prove_plus(left: &NatTerm, right: &NatTerm, result: &NatTerm) -> Option<ReduceNatExpDerivation> {
    match left {
        NatTerm::Z if right == result => Some(derivation(
            ReduceNatExpJudgment::PlusIs {
                left: left.clone(),
                right: right.clone(),
                result: result.clone(),
            },
            "P-Zero",
            Vec::new(),
        )),
        NatTerm::S(left_inner) => {
            let NatTerm::S(result_inner) = result else {
                return None;
            };
            let premise = prove_plus(left_inner, right, result_inner)?;
            Some(derivation(
                ReduceNatExpJudgment::PlusIs {
                    left: left.clone(),
                    right: right.clone(),
                    result: result.clone(),
                },
                "P-Succ",
                vec![premise],
            ))
        }
        _ => None,
    }
}

fn prove_times(
    left: &NatTerm,
    right: &NatTerm,
    result: &NatTerm,
) -> Option<ReduceNatExpDerivation> {
    match left {
        NatTerm::Z if result == &NatTerm::Z => Some(derivation(
            ReduceNatExpJudgment::TimesIs {
                left: left.clone(),
                right: right.clone(),
                result: result.clone(),
            },
            "T-Zero",
            Vec::new(),
        )),
        NatTerm::S(left_inner) => {
            let first_result = eval_times(left_inner, right);
            let first = prove_times(left_inner, right, &first_result)?;
            let second = prove_plus(right, &first_result, result)?;
            Some(derivation(
                ReduceNatExpJudgment::TimesIs {
                    left: left.clone(),
                    right: right.clone(),
                    result: result.clone(),
                },
                "T-Succ",
                vec![first, second],
            ))
        }
        _ => None,
    }
}

fn one_step_reductions(expr: &ReduceNatExpExpr) -> Vec<ReduceNatExpDerivation> {
    let mut derivations = Vec::new();

    match expr {
        ReduceNatExpExpr::Nat(_) => {}
        ReduceNatExpExpr::Plus(left, right) => {
            if let (Some(left_nat), Some(right_nat)) = (as_nat_expr(left), as_nat_expr(right)) {
                let result_nat = eval_plus(left_nat, right_nat);
                let premise = prove_plus(left_nat, right_nat, &result_nat)
                    .expect("eval_plus result must be derivable");
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Nat(result_nat),
                    },
                    "R-Plus",
                    vec![premise],
                ));
            }

            for sub in one_step_reductions(left) {
                let to_left = one_step_to(&sub)
                    .expect("one_step_reductions should build one-step judgments")
                    .clone();
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Plus(
                            Box::new(to_left),
                            Box::new(right.as_ref().clone()),
                        ),
                    },
                    "R-PlusL",
                    vec![sub],
                ));
            }

            for sub in one_step_reductions(right) {
                let to_right = one_step_to(&sub)
                    .expect("one_step_reductions should build one-step judgments")
                    .clone();
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Plus(
                            Box::new(left.as_ref().clone()),
                            Box::new(to_right),
                        ),
                    },
                    "R-PlusR",
                    vec![sub],
                ));
            }
        }
        ReduceNatExpExpr::Times(left, right) => {
            if let (Some(left_nat), Some(right_nat)) = (as_nat_expr(left), as_nat_expr(right)) {
                let result_nat = eval_times(left_nat, right_nat);
                let premise = prove_times(left_nat, right_nat, &result_nat)
                    .expect("eval_times result must be derivable");
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Nat(result_nat),
                    },
                    "R-Times",
                    vec![premise],
                ));
            }

            for sub in one_step_reductions(left) {
                let to_left = one_step_to(&sub)
                    .expect("one_step_reductions should build one-step judgments")
                    .clone();
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Times(
                            Box::new(to_left),
                            Box::new(right.as_ref().clone()),
                        ),
                    },
                    "R-TimesL",
                    vec![sub],
                ));
            }

            for sub in one_step_reductions(right) {
                let to_right = one_step_to(&sub)
                    .expect("one_step_reductions should build one-step judgments")
                    .clone();
                derivations.push(derivation(
                    ReduceNatExpJudgment::ReducesTo {
                        from: expr.clone(),
                        to: ReduceNatExpExpr::Times(
                            Box::new(left.as_ref().clone()),
                            Box::new(to_right),
                        ),
                    },
                    "R-TimesR",
                    vec![sub],
                ));
            }
        }
    }

    derivations
}

fn deterministic_step(expr: &ReduceNatExpExpr) -> Option<ReduceNatExpDerivation> {
    match expr {
        ReduceNatExpExpr::Nat(_) => None,
        ReduceNatExpExpr::Plus(left, right) => {
            if let Some(left_nat) = as_nat_expr(left) {
                if let Some(right_nat) = as_nat_expr(right) {
                    let result_nat = eval_plus(left_nat, right_nat);
                    let premise = prove_plus(left_nat, right_nat, &result_nat)
                        .expect("eval_plus result must be derivable");
                    return Some(derivation(
                        ReduceNatExpJudgment::DeterministicReducesTo {
                            from: expr.clone(),
                            to: ReduceNatExpExpr::Nat(result_nat),
                        },
                        "DR-Plus",
                        vec![premise],
                    ));
                }

                if let Some(sub) = deterministic_step(right) {
                    let to_right = one_step_to(&sub)
                        .expect("deterministic_step should always build one-step judgments")
                        .clone();
                    return Some(derivation(
                        ReduceNatExpJudgment::DeterministicReducesTo {
                            from: expr.clone(),
                            to: ReduceNatExpExpr::Plus(
                                Box::new(left_nat_expr(left_nat)),
                                Box::new(to_right),
                            ),
                        },
                        "DR-PlusR",
                        vec![sub],
                    ));
                }

                return None;
            }

            let sub = deterministic_step(left)?;
            let to_left = one_step_to(&sub)
                .expect("deterministic_step should always build one-step judgments")
                .clone();
            Some(derivation(
                ReduceNatExpJudgment::DeterministicReducesTo {
                    from: expr.clone(),
                    to: ReduceNatExpExpr::Plus(Box::new(to_left), Box::new(right.as_ref().clone())),
                },
                "DR-PlusL",
                vec![sub],
            ))
        }
        ReduceNatExpExpr::Times(left, right) => {
            if let Some(left_nat) = as_nat_expr(left) {
                if let Some(right_nat) = as_nat_expr(right) {
                    let result_nat = eval_times(left_nat, right_nat);
                    let premise = prove_times(left_nat, right_nat, &result_nat)
                        .expect("eval_times result must be derivable");
                    return Some(derivation(
                        ReduceNatExpJudgment::DeterministicReducesTo {
                            from: expr.clone(),
                            to: ReduceNatExpExpr::Nat(result_nat),
                        },
                        "DR-Times",
                        vec![premise],
                    ));
                }

                if let Some(sub) = deterministic_step(right) {
                    let to_right = one_step_to(&sub)
                        .expect("deterministic_step should always build one-step judgments")
                        .clone();
                    return Some(derivation(
                        ReduceNatExpJudgment::DeterministicReducesTo {
                            from: expr.clone(),
                            to: ReduceNatExpExpr::Times(
                                Box::new(left_nat_expr(left_nat)),
                                Box::new(to_right),
                            ),
                        },
                        "DR-TimesR",
                        vec![sub],
                    ));
                }

                return None;
            }

            let sub = deterministic_step(left)?;
            let to_left = one_step_to(&sub)
                .expect("deterministic_step should always build one-step judgments")
                .clone();
            Some(derivation(
                ReduceNatExpJudgment::DeterministicReducesTo {
                    from: expr.clone(),
                    to: ReduceNatExpExpr::Times(
                        Box::new(to_left),
                        Box::new(right.as_ref().clone()),
                    ),
                },
                "DR-TimesL",
                vec![sub],
            ))
        }
    }
}

fn find_multi_reduction_path(
    from: &ReduceNatExpExpr,
    to: &ReduceNatExpExpr,
) -> Option<Vec<ReduceNatExpDerivation>> {
    if from == to {
        return Some(Vec::new());
    }

    let mut visited = vec![from.clone()];
    let mut queue = VecDeque::new();
    queue.push_back((from.clone(), Vec::<ReduceNatExpDerivation>::new()));

    while let Some((expr, path)) = queue.pop_front() {
        for step in one_step_reductions(&expr) {
            let next = one_step_to(&step)
                .expect("one_step_reductions should build one-step judgments")
                .clone();
            if visited.iter().any(|seen| seen == &next) {
                continue;
            }

            let mut next_path = path.clone();
            next_path.push(step);

            if &next == to {
                return Some(next_path);
            }

            visited.push(next.clone());
            queue.push_back((next, next_path));
        }
    }

    None
}

fn collect_reachable_targets(from: &ReduceNatExpExpr) -> Vec<ReduceNatExpExpr> {
    let mut visited = vec![from.clone()];
    let mut queue = VecDeque::new();
    queue.push_back(from.clone());

    while let Some(expr) = queue.pop_front() {
        for step in one_step_reductions(&expr) {
            let next = one_step_to(&step)
                .expect("one_step_reductions should build one-step judgments")
                .clone();
            if visited.iter().any(|seen| seen == &next) {
                continue;
            }
            visited.push(next.clone());
            queue.push_back(next);
        }
    }

    visited
}

fn multi_derivation_from_steps(steps: &[ReduceNatExpDerivation]) -> ReduceNatExpDerivation {
    debug_assert!(!steps.is_empty());

    let from = one_step_from(steps.first().expect("steps should not be empty"))
        .expect("multi_derivation_from_steps expects one-step derivations")
        .clone();
    let to = one_step_to(steps.last().expect("steps should not be empty"))
        .expect("multi_derivation_from_steps expects one-step derivations")
        .clone();

    if steps.len() == 1 {
        return derivation(
            ReduceNatExpJudgment::MultiReducesTo { from, to },
            "MR-One",
            vec![steps[0].clone()],
        );
    }

    let left = multi_derivation_from_steps(&steps[..steps.len() - 1]);
    let right = multi_derivation_from_steps(&steps[steps.len() - 1..]);
    derivation(
        ReduceNatExpJudgment::MultiReducesTo { from, to },
        "MR-Multi",
        vec![left, right],
    )
}

fn find_matching_one_step_derivation(
    derivations: &[ReduceNatExpDerivation],
    expected_to: &ReduceNatExpExpr,
) -> Option<ReduceNatExpDerivation> {
    derivations.iter().find_map(|derivation| {
        let actual_to = one_step_to(derivation)?;
        if actual_to == expected_to {
            Some(derivation.clone())
        } else {
            None
        }
    })
}

fn one_step_from(judgment: &ReduceNatExpDerivation) -> Option<&ReduceNatExpExpr> {
    let (ReduceNatExpJudgment::ReducesTo { from, .. }
    | ReduceNatExpJudgment::DeterministicReducesTo { from, .. }) = &judgment.judgment
    else {
        return None;
    };
    Some(from)
}

fn one_step_to(judgment: &ReduceNatExpDerivation) -> Option<&ReduceNatExpExpr> {
    let (ReduceNatExpJudgment::ReducesTo { to, .. }
    | ReduceNatExpJudgment::DeterministicReducesTo { to, .. }) = &judgment.judgment
    else {
        return None;
    };
    Some(to)
}

fn as_nat_expr(expr: &ReduceNatExpExpr) -> Option<&NatTerm> {
    let ReduceNatExpExpr::Nat(nat) = expr else {
        return None;
    };
    Some(nat)
}

fn left_nat_expr(nat: &NatTerm) -> ReduceNatExpExpr {
    ReduceNatExpExpr::Nat(nat.clone())
}

fn eval_plus(left: &NatTerm, right: &NatTerm) -> NatTerm {
    match left {
        NatTerm::Z => right.clone(),
        NatTerm::S(inner) => NatTerm::S(Box::new(eval_plus(inner, right))),
    }
}

fn eval_times(left: &NatTerm, right: &NatTerm) -> NatTerm {
    match left {
        NatTerm::Z => NatTerm::Z,
        NatTerm::S(inner) => {
            let partial = eval_times(inner, right);
            eval_plus(right, &partial)
        }
    }
}

fn non_derivable_judgment_error(judgment: &ReduceNatExpJudgment) -> CheckError {
    match judgment {
        ReduceNatExpJudgment::PlusIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_plus(left, right);
            let expected = ReduceNatExpJudgment::PlusIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            CheckError::rule_violation(format!(
                "judgment is not derivable in ReduceNatExp (expected: {expected}, actual: {judgment}; {fix})"
            ))
        }
        ReduceNatExpJudgment::TimesIs {
            left,
            right,
            result,
        } => {
            let expected_result = eval_times(left, right);
            let expected = ReduceNatExpJudgment::TimesIs {
                left: left.clone(),
                right: right.clone(),
                result: expected_result.clone(),
            };
            let fix = result_fix_message(result, &expected_result);
            CheckError::rule_violation(format!(
                "judgment is not derivable in ReduceNatExp (expected: {expected}, actual: {judgment}; {fix})"
            ))
        }
        _ => CheckError::rule_violation(format!(
            "judgment is not derivable in ReduceNatExp (actual: {judgment}; fix: check the judgment form and target expression)"
        )),
    }
}

fn non_derivable_reduces_error(
    judgment: &ReduceNatExpJudgment,
    from: &ReduceNatExpExpr,
    to: &ReduceNatExpExpr,
    candidates: &[ReduceNatExpDerivation],
) -> CheckError {
    if candidates.is_empty() {
        return CheckError::rule_violation(format!(
            "judgment is not derivable in ReduceNatExp (expected: {from} has no one-step reduction, actual: {judgment}; fix: use '-*->' with {from} as both sides or choose a reducible source expression)"
        ));
    }

    let expected_targets = candidates
        .iter()
        .filter_map(one_step_to)
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ");

    let fix = if candidates
        .iter()
        .filter_map(one_step_to)
        .any(|candidate| candidate == to)
    {
        "fix: check the judgment operator and source expression".to_string()
    } else {
        format!("fix: replace target with one of [{expected_targets}]")
    };

    CheckError::rule_violation(format!(
        "judgment is not derivable in ReduceNatExp (expected: one-step targets from {from} are [{expected_targets}], actual: {judgment}; {fix})"
    ))
}

fn non_derivable_deterministic_error(
    judgment: &ReduceNatExpJudgment,
    from: &ReduceNatExpExpr,
    to: &ReduceNatExpExpr,
    expected_to: Option<&ReduceNatExpExpr>,
) -> CheckError {
    match expected_to {
        Some(expected_to) => {
            let expected = ReduceNatExpJudgment::DeterministicReducesTo {
                from: from.clone(),
                to: expected_to.clone(),
            };
            let fix = if expected_to == to {
                "fix: check the source expression and operator".to_string()
            } else {
                format!("fix: replace target with {expected_to}")
            };
            CheckError::rule_violation(format!(
                "judgment is not derivable in ReduceNatExp (expected: {expected}, actual: {judgment}; {fix})"
            ))
        }
        None => CheckError::rule_violation(format!(
            "judgment is not derivable in ReduceNatExp (expected: {from} has no deterministic one-step reduction, actual: {judgment}; fix: use '-*->' with {from} as both sides or choose a reducible source expression)"
        )),
    }
}

fn non_derivable_multi_error(
    judgment: &ReduceNatExpJudgment,
    from: &ReduceNatExpExpr,
    to: &ReduceNatExpExpr,
) -> CheckError {
    let reachable_targets = collect_reachable_targets(from);
    let reachable_text = reachable_targets
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ");

    let fix = if reachable_targets.iter().any(|candidate| candidate == to) {
        "fix: check the judgment operator and source expression".to_string()
    } else {
        format!("fix: replace target with one of [{reachable_text}]")
    };

    CheckError::rule_violation(format!(
        "judgment is not derivable in ReduceNatExp (expected: multi-step targets from {from} are [{reachable_text}], actual: {judgment}; {fix})"
    ))
}

fn result_fix_message(actual_result: &NatTerm, expected_result: &NatTerm) -> String {
    if actual_result == expected_result {
        "fix: check the judgment terms and operator".to_string()
    } else {
        format!("fix: replace result term with {expected_result}")
    }
}

fn derivation(
    judgment: ReduceNatExpJudgment,
    rule_name: &str,
    subderivations: Vec<ReduceNatExpDerivation>,
) -> ReduceNatExpDerivation {
    ReduceNatExpDerivation {
        span: SourceSpan { line: 1, column: 1 },
        judgment,
        rule_name: rule_name.to_string(),
        subderivations,
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_source;
    use super::super::syntax::ReduceNatExpDerivation;
    use super::prove_judgment;
    use crate::games::reduce_nat_exp::syntax::{NatTerm, ReduceNatExpExpr, ReduceNatExpJudgment};

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    fn nat(term: NatTerm) -> ReduceNatExpExpr {
        ReduceNatExpExpr::Nat(term)
    }

    #[test]
    fn proves_reduces_to_by_r_plus() {
        let derivation = prove_judgment(ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Plus(Box::new(nat(s(z()))), Box::new(nat(s(z())))),
            to: nat(s(s(z()))),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "R-Plus");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "P-Succ");
    }

    #[test]
    fn proves_deterministic_reduces_to_by_dr_plus_l() {
        let derivation = prove_judgment(ReduceNatExpJudgment::DeterministicReducesTo {
            from: ReduceNatExpExpr::Plus(
                Box::new(ReduceNatExpExpr::Times(
                    Box::new(nat(s(z()))),
                    Box::new(nat(s(z()))),
                )),
                Box::new(ReduceNatExpExpr::Times(
                    Box::new(nat(s(z()))),
                    Box::new(nat(s(z()))),
                )),
            ),
            to: ReduceNatExpExpr::Plus(
                Box::new(nat(s(z()))),
                Box::new(ReduceNatExpExpr::Times(
                    Box::new(nat(s(z()))),
                    Box::new(nat(s(z()))),
                )),
            ),
        })
        .expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "DR-PlusL");
        assert_eq!(derivation.subderivations.len(), 1);
        assert_eq!(derivation.subderivations[0].rule_name, "DR-Times");
    }

    #[test]
    fn proves_multi_reduces_to_by_mr_multi_chain() {
        let judgment = parse_source(include_str!("../../../copl/024.copl"))
            .expect("fixture should parse")
            .judgment;

        let derivation = prove_judgment(judgment.clone()).expect("judgment should be derivable");

        assert_eq!(derivation.rule_name, "MR-Multi");
        assert_eq!(derivation.judgment, judgment);
    }

    #[test]
    fn rejects_non_derivable_reduces_judgment() {
        let err = prove_judgment(ReduceNatExpJudgment::ReducesTo {
            from: ReduceNatExpExpr::Plus(Box::new(nat(s(z()))), Box::new(nat(s(z())))),
            to: nat(z()),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in ReduceNatExp"));
        assert!(err.message().contains("one-step targets"));
        assert!(err
            .message()
            .contains("fix: replace target with one of [S(S(Z))]"));
    }

    #[test]
    fn rejects_non_derivable_multi_judgment() {
        let err = prove_judgment(ReduceNatExpJudgment::MultiReducesTo {
            from: ReduceNatExpExpr::Plus(Box::new(nat(z())), Box::new(nat(s(z())))),
            to: ReduceNatExpExpr::Plus(Box::new(nat(s(z()))), Box::new(nat(z()))),
        })
        .expect_err("judgment should be rejected");

        assert!(err
            .message()
            .contains("judgment is not derivable in ReduceNatExp"));
        assert!(err.message().contains("multi-step targets"));
    }

    #[test]
    fn builds_same_derivation_shape_as_fixture_024() {
        let expected =
            parse_source(include_str!("../../../copl/024.copl")).expect("fixture should parse");
        let actual =
            prove_judgment(expected.judgment.clone()).expect("fixture judgment should be provable");

        assert_same_shape(&actual, &expected);
    }

    fn assert_same_shape(actual: &ReduceNatExpDerivation, expected: &ReduceNatExpDerivation) {
        assert_eq!(actual.judgment, expected.judgment);
        assert_eq!(actual.rule_name, expected.rule_name);
        assert_eq!(actual.subderivations.len(), expected.subderivations.len());
        for (actual_sub, expected_sub) in actual.subderivations.iter().zip(&expected.subderivations)
        {
            assert_same_shape(actual_sub, expected_sub);
        }
    }
}
