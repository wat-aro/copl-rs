use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::typing_ml4::{self, TypingML4Game};

pub struct TypingML2Game;

impl Game for TypingML2Game {
    fn kind(&self) -> GameKind {
        GameKind::TypingML2
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let mut report = TypingML4Game.check(source).map_err(remap_error)?;
        report.game = GameKind::TypingML2;
        Ok(report)
    }
}

pub(crate) fn prove(source: &str) -> Result<String, CheckError> {
    typing_ml4::prove(source).map_err(remap_error)
}

fn remap_error(err: CheckError) -> CheckError {
    let mapped = err.message().replace("TypingML4", "TypingML2");
    if mapped == err.message() {
        err
    } else {
        err.with_message(mapped)
    }
}
