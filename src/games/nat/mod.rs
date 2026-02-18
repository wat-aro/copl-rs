use self::syntax::NatDerivation;
use crate::core::CheckError;

mod checker;
mod lexer;
mod parser;
mod prover;
mod syntax;

pub use checker::NatGame;

pub(crate) fn prove(source: &str) -> Result<NatDerivation, CheckError> {
    let judgment = parser::parse_judgment_source(source)?;
    prover::prove_judgment(judgment)
}
