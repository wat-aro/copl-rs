mod nat_arith;

pub mod compare_nat1;
pub mod compare_nat2;
pub mod compare_nat3;
pub mod eval_cont_ml1;
pub mod eval_cont_ml4;
pub mod eval_ml1;
pub mod eval_ml1_err;
pub mod eval_ml2;
pub mod eval_ml3;
pub mod eval_ml4;
pub mod eval_ml5;
pub mod eval_ml6;
pub mod eval_nameless_ml3;
pub mod eval_nat_exp;
pub mod nameless_ml3;
pub mod nat;
pub mod poly_typing_ml3;
pub mod poly_typing_ml4;
pub mod reduce_nat_exp;
pub mod typing_ml2;
pub mod typing_ml3;
pub mod typing_ml4;
pub mod typing_ml5;
pub mod typing_ml6;

use crate::core::{CheckError, CheckReport, Game, GameKind};

pub fn run_checker(game: GameKind, source: &str) -> Result<CheckReport, CheckError> {
    match game {
        GameKind::Nat => nat::NatGame.check(source),
        GameKind::CompareNat1 => compare_nat1::CompareNat1Game.check(source),
        GameKind::CompareNat2 => compare_nat2::CompareNat2Game.check(source),
        GameKind::CompareNat3 => compare_nat3::CompareNat3Game.check(source),
        GameKind::EvalML1 => eval_ml1::EvalML1Game.check(source),
        GameKind::EvalML1Err => eval_ml1_err::EvalML1ErrGame.check(source),
        GameKind::EvalML2 => eval_ml2::EvalML2Game.check(source),
        GameKind::EvalML3 => eval_ml3::EvalML3Game.check(source),
        GameKind::EvalML4 => eval_ml4::EvalML4Game.check(source),
        GameKind::EvalML5 => eval_ml5::EvalML5Game.check(source),
        GameKind::EvalML6 => eval_ml6::EvalML6Game.check(source),
        GameKind::EvalContML1 => eval_cont_ml1::EvalContML1Game.check(source),
        GameKind::EvalContML4 => eval_cont_ml4::EvalContML4Game.check(source),
        GameKind::TypingML2 => typing_ml2::TypingML2Game.check(source),
        GameKind::TypingML3 => typing_ml3::TypingML3Game.check(source),
        GameKind::TypingML4 => typing_ml4::TypingML4Game.check(source),
        GameKind::TypingML5 => typing_ml5::TypingML5Game.check(source),
        GameKind::TypingML6 => typing_ml6::TypingML6Game.check(source),
        GameKind::PolyTypingML3 => poly_typing_ml3::PolyTypingML3Game.check(source),
        GameKind::PolyTypingML4 => poly_typing_ml4::PolyTypingML4Game.check(source),
        GameKind::NamelessML3 => nameless_ml3::NamelessML3Game.check(source),
        GameKind::EvalNamelessML3 => eval_nameless_ml3::EvalNamelessML3Game.check(source),
        GameKind::EvalNatExp => eval_nat_exp::EvalNatExpGame.check(source),
        GameKind::ReduceNatExp => reduce_nat_exp::ReduceNatExpGame.check(source),
    }
}
