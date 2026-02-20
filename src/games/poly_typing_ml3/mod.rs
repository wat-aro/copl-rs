use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::poly_typing_ml4::{self, PolyTypingML4Game};

pub struct PolyTypingML3Game;

impl Game for PolyTypingML3Game {
    fn kind(&self) -> GameKind {
        GameKind::PolyTypingML3
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let mut report = PolyTypingML4Game.check(source).map_err(remap_error)?;
        report.game = GameKind::PolyTypingML3;
        Ok(report)
    }
}

pub(crate) fn prove(source: &str) -> Result<String, CheckError> {
    poly_typing_ml4::prove(source).map_err(remap_error)
}

fn remap_error(err: CheckError) -> CheckError {
    let mapped = err.message().replace("PolyTypingML4", "PolyTypingML3");
    if mapped == err.message() {
        err
    } else {
        err.with_message(mapped)
    }
}
