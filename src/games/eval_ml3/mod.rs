mod checker;
mod lexer;
mod parser;
mod prover;
mod syntax;

pub use checker::EvalML3Game;

use crate::core::CheckError;

pub(crate) fn prove(source: &str) -> Result<String, CheckError> {
    let judgment = parser::parse_judgment_source(source)?;
    let derivation = prover::prove_judgment(judgment)?;
    Ok(derivation.to_string())
}
