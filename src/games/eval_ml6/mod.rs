use crate::core::{CheckError, CheckReport, Game, GameKind};

use super::eval_ml5::{self, EvalML5Game};

pub struct EvalML6Game;

impl Game for EvalML6Game {
    fn kind(&self) -> GameKind {
        GameKind::EvalML6
    }

    fn check(&self, source: &str) -> Result<CheckReport, CheckError> {
        let mut report = EvalML5Game.check(source).map_err(remap_error)?;
        report.game = GameKind::EvalML6;
        Ok(report)
    }
}

pub(crate) fn prove(source: &str) -> Result<String, CheckError> {
    eval_ml5::prove(source).map_err(remap_error)
}

fn remap_error(err: CheckError) -> CheckError {
    let mapped = err.message().replace("EvalML5", "EvalML6");
    if mapped == err.message() {
        err
    } else {
        err.with_message(mapped)
    }
}
