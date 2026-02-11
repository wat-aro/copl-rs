use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::parser::parse_source;

#[derive(Debug, Clone, Copy, Default)]
pub struct NatGame;

impl Game for NatGame {
    fn kind(&self) -> GameKind {
        GameKind::Nat
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let parsed = parse_source(source)?;
        Ok(CheckReport {
            game: self.kind(),
            summary: format!("parsed root rule {}", parsed.rule.as_str()),
        })
    }
}
