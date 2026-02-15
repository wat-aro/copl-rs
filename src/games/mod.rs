pub mod compare_nat1;
pub mod compare_nat2;
pub mod compare_nat3;
pub mod eval_nat_exp;
pub mod nat;
pub mod reduce_nat_exp;

use crate::core::{CheckError, CheckReport, Game, GameKind};

pub fn run_checker(game: GameKind, source: &str) -> Result<CheckReport, CheckError> {
    match game {
        GameKind::Nat => nat::NatGame.check(source),
        GameKind::CompareNat1 => compare_nat1::CompareNat1Game.check(source),
        GameKind::CompareNat2 => compare_nat2::CompareNat2Game.check(source),
        GameKind::CompareNat3 => compare_nat3::CompareNat3Game.check(source),
        GameKind::EvalNatExp => eval_nat_exp::EvalNatExpGame.check(source),
        GameKind::ReduceNatExp => reduce_nat_exp::ReduceNatExpGame.check(source),
    }
}
