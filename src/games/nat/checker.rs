use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::syntax::{NatDerivation, NatJudgment, NatOperator, NatTerm};

#[derive(Debug, Clone, Copy)]
struct NatDerivationRule {
    name: &'static str,
    premise_arity: usize,
    infer_conclusion: fn(&NatJudgment, &[NatJudgment]) -> Result<NatJudgment, String>,
}

const P_ZERO_RULE: NatDerivationRule = NatDerivationRule {
    name: "P-Zero",
    premise_arity: 0,
    infer_conclusion: infer_p_zero,
};

const P_SUCC_RULE: NatDerivationRule = NatDerivationRule {
    name: "P-Succ",
    premise_arity: 1,
    infer_conclusion: infer_p_succ,
};

const T_ZERO_RULE: NatDerivationRule = NatDerivationRule {
    name: "T-Zero",
    premise_arity: 0,
    infer_conclusion: infer_t_zero,
};

const T_SUCC_RULE: NatDerivationRule = NatDerivationRule {
    name: "T-Succ",
    premise_arity: 2,
    infer_conclusion: infer_t_succ,
};

fn rule_definition(rule_name: &str) -> Option<&'static NatDerivationRule> {
    match rule_name {
        "P-Zero" => Some(&P_ZERO_RULE),
        "P-Succ" => Some(&P_SUCC_RULE),
        "T-Zero" => Some(&T_ZERO_RULE),
        "T-Succ" => Some(&T_SUCC_RULE),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NatGame;

impl Game for NatGame {
    fn kind(&self) -> GameKind {
        GameKind::Nat
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let parsed = parse_source(source)?;
        infer_judgment(&parsed)?;
        Ok(CheckReport {
            game: self.kind(),
            summary: format!("parsed root rule {}", parsed.rule_name),
        })
    }
}

fn infer_judgment(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    infer_judgment_impl(derivation).map_err(|err| ensure_error_has_span(err, derivation))
}

fn infer_judgment_impl(derivation: &NatDerivation) -> Result<NatJudgment, CheckError> {
    let inferred_subderivations = derivation
        .subderivations
        .iter()
        .map(infer_judgment)
        .collect::<Result<Vec<_>, _>>()?;

    let Some(rule) = rule_definition(&derivation.rule_name) else {
        return Err(rule_violation(
            derivation,
            format!("unknown rule name: {}", derivation.rule_name),
        ));
    };
    expect_subderivation_count(derivation, rule)?;

    let inferred = (rule.infer_conclusion)(&derivation.judgment, &inferred_subderivations)
        .map_err(|detail| rule_violation(derivation, detail))?;

    if inferred != derivation.judgment {
        return Err(rule_violation(
            derivation,
            format!(
                "inferred judgment '{}' does not match declared judgment '{}'",
                format_judgment(&inferred),
                format_judgment(&derivation.judgment)
            ),
        ));
    }

    Ok(inferred)
}

fn ensure_error_has_span(err: CheckError, derivation: &NatDerivation) -> CheckError {
    if err.span().is_some() {
        err
    } else {
        err.with_span(derivation.span.clone())
    }
}

fn infer_p_zero(declared: &NatJudgment, _premises: &[NatJudgment]) -> Result<NatJudgment, String> {
    Ok(NatJudgment {
        left: NatTerm::Z,
        operator: NatOperator::Plus,
        right: declared.right.clone(),
        result: declared.right.clone(),
    })
}

fn infer_p_succ(_declared: &NatJudgment, premises: &[NatJudgment]) -> Result<NatJudgment, String> {
    let premise = &premises[0];
    if premise.operator != NatOperator::Plus {
        return Err("P-Succ premise must be plus".to_string());
    }

    Ok(NatJudgment {
        left: NatTerm::S(Box::new(premise.left.clone())),
        operator: NatOperator::Plus,
        right: premise.right.clone(),
        result: NatTerm::S(Box::new(premise.result.clone())),
    })
}

fn infer_t_zero(declared: &NatJudgment, _premises: &[NatJudgment]) -> Result<NatJudgment, String> {
    Ok(NatJudgment {
        left: NatTerm::Z,
        operator: NatOperator::Times,
        right: declared.right.clone(),
        result: NatTerm::Z,
    })
}

fn infer_t_succ(_declared: &NatJudgment, premises: &[NatJudgment]) -> Result<NatJudgment, String> {
    let first_premise = &premises[0];
    if first_premise.operator != NatOperator::Times {
        return Err("T-Succ first premise must be times".to_string());
    }

    let second_premise = &premises[1];
    if second_premise.operator != NatOperator::Plus {
        return Err("T-Succ second premise must be plus".to_string());
    }
    if second_premise.left != first_premise.right || second_premise.right != first_premise.result {
        return Err(
            "T-Succ premises must connect as 'n1 times n2 is n3' and 'n2 plus n3 is n4'"
                .to_string(),
        );
    }

    Ok(NatJudgment {
        left: NatTerm::S(Box::new(first_premise.left.clone())),
        operator: NatOperator::Times,
        right: first_premise.right.clone(),
        result: second_premise.result.clone(),
    })
}

fn expect_subderivation_count(
    derivation: &NatDerivation,
    rule: &NatDerivationRule,
) -> Result<(), CheckError> {
    let actual = derivation.subderivations.len();
    if actual == rule.premise_arity {
        Ok(())
    } else {
        Err(rule_violation(
            derivation,
            format!(
                "{} expects {} premise(s), but got {actual}",
                rule.name, rule.premise_arity
            ),
        ))
    }
}

fn rule_violation(derivation: &NatDerivation, detail: impl Into<String>) -> CheckError {
    let detail = detail.into();
    CheckError::rule_violation(format!(
        "{detail}: {} by {}",
        format_judgment(&derivation.judgment),
        derivation.rule_name
    ))
    .with_span(derivation.span.clone())
}

fn format_judgment(judgment: &NatJudgment) -> String {
    format!(
        "{} {} {} is {}",
        format_term(&judgment.left),
        format_operator(judgment.operator),
        format_term(&judgment.right),
        format_term(&judgment.result)
    )
}

fn format_operator(operator: NatOperator) -> &'static str {
    match operator {
        NatOperator::Plus => "plus",
        NatOperator::Times => "times",
    }
}

fn format_term(term: &NatTerm) -> String {
    match term {
        NatTerm::Z => "Z".to_string(),
        NatTerm::S(inner) => format!("S({})", format_term(inner)),
    }
}

#[cfg(test)]
mod tests {
    use crate::core::{CheckErrorKind, Game};

    use super::NatGame;

    #[test]
    fn accepts_all_ascii_fixtures_001_to_008() {
        let game = NatGame;
        for source in [
            include_str!("../../../copl/001.copl"),
            include_str!("../../../copl/002.copl"),
            include_str!("../../../copl/003.copl"),
            include_str!("../../../copl/004.copl"),
            include_str!("../../../copl/005.copl"),
            include_str!("../../../copl/006.copl"),
            include_str!("../../../copl/007.copl"),
            include_str!("../../../copl/008.copl"),
        ] {
            let report = game.check(source).expect("fixture should be valid");
            assert!(report.summary.contains("parsed root rule"));
        }
    }

    #[test]
    fn reports_rule_violation_for_premise_arity_mismatch() {
        let source = "Z plus Z is Z by P-Zero { Z plus Z is Z by P-Zero {} }";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("expects 0 premise(s), but got 1"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_p_succ_premise() {
        let source = "S(Z) plus Z is S(Z) by P-Succ { Z plus S(Z) is S(Z) by P-Zero {} }";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("does not match declared judgment"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_result_term() {
        let source = "Z plus S(Z) is Z by P-Zero {}";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("does not match declared judgment"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_inconsistent_t_succ_premise_link() {
        let source = r#"
S(Z) times Z is Z by T-Succ {
  Z times Z is Z by T-Zero {};
  S(Z) plus Z is S(Z) by P-Succ {
    Z plus Z is Z by P-Zero {}
  }
}
"#;
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("T-Succ premises must connect as"));
        let span = err
            .span()
            .expect("checker inconsistency should have source span");
        assert_eq!(span.line, 2);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_for_unknown_rule_name() {
        let source = "Z plus Z is Z by P-Unknown {}";
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        assert!(err.message().contains("unknown rule name"));
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn reports_rule_violation_at_failing_subderivation_location() {
        let source = r#"
S(Z) plus Z is S(Z) by P-Succ {
  Z plus Z is Z by P-Unknown {}
}
"#;
        let err = NatGame.check(source).expect_err("check should fail");
        assert_eq!(err.kind(), CheckErrorKind::RuleViolation);
        let span = err.span().expect("rule violation should have source span");
        assert_eq!(span.line, 3);
        assert_eq!(span.column, 3);
    }
}
