use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;
use super::prover::prove_judgment;
use super::syntax::EvalRefML3Derivation;

#[derive(Debug, Clone, Copy, Default)]
pub struct EvalRefML3Game;

impl Game for EvalRefML3Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalRefML3
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let parsed = parse_source(source)?;
        validate_rule_names(&parsed)?;
        let expected = prove_judgment(parsed.judgment.clone()).map_err(|err| {
            CheckError::rule_violation(err.message().to_string()).with_span(parsed.span.clone())
        })?;

        if derivations_equivalent(&expected, &parsed) {
            return Ok(CheckReport {
                game: self.kind(),
                summary: parsed.judgment.to_string(),
            });
        }

        let mismatch_span =
            find_first_mismatch_span(&expected, &parsed).unwrap_or(parsed.span.clone());
        Err(CheckError::rule_violation(format!(
            "Wrong rule application: expected derivation `{expected}`, actual derivation `{parsed}`; fix: rewrite rule names or premises to match the canonical derivation"
        ))
        .with_span(mismatch_span))
    }
}

fn derivations_equivalent(left: &EvalRefML3Derivation, right: &EvalRefML3Derivation) -> bool {
    left.judgment == right.judgment
        && left.rule_name == right.rule_name
        && left.subderivations.len() == right.subderivations.len()
        && left
            .subderivations
            .iter()
            .zip(right.subderivations.iter())
            .all(|(lhs, rhs)| derivations_equivalent(lhs, rhs))
}

fn find_first_mismatch_span(
    expected: &EvalRefML3Derivation,
    actual: &EvalRefML3Derivation,
) -> Option<crate::core::SourceSpan> {
    if expected.judgment != actual.judgment
        || expected.rule_name != actual.rule_name
        || expected.subderivations.len() != actual.subderivations.len()
    {
        return Some(actual.span.clone());
    }

    for (lhs, rhs) in expected
        .subderivations
        .iter()
        .zip(actual.subderivations.iter())
    {
        if let Some(span) = find_first_mismatch_span(lhs, rhs) {
            return Some(span);
        }
    }

    None
}

fn validate_rule_names(derivation: &EvalRefML3Derivation) -> Result<(), CheckError> {
    if !matches!(
        derivation.rule_name.as_str(),
        "E-Int" | "E-Unit" | "E-Loc" | "E-Var" | "E-Let" | "E-Ref" | "E-Deref" | "E-Assign"
    ) {
        return Err(CheckError::rule_violation(format!(
            "No such rule: {} (available: E-Int, E-Unit, E-Loc, E-Var, E-Let, E-Ref, E-Deref, E-Assign; fix: replace the rule name after 'by')",
            derivation.rule_name
        ))
        .with_span(derivation.span.clone()));
    }

    for subderivation in &derivation.subderivations {
        validate_rule_names(subderivation)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::EvalRefML3Game;
    use crate::core::Game;

    #[test]
    fn accepts_valid_leaf_derivation() {
        let source = "|- 1 / () evalto 1 / () by E-Int {}";
        let report = EvalRefML3Game
            .check(source)
            .expect("derivation should be valid");
        assert_eq!(report.summary, "|- 1 / () evalto 1 / ()");
    }

    #[test]
    fn rejects_unknown_rule_name() {
        let source = "|- 1 / () evalto 1 / () by E-Unknown {}";
        let err = EvalRefML3Game
            .check(source)
            .expect_err("derivation should be invalid");
        assert!(err.message().contains("No such rule"));
    }
}
