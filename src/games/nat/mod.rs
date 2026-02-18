use crate::core::CheckError;

mod checker;
mod lexer;
mod parser;
mod syntax;

pub use checker::NatGame;

pub(crate) fn validate_prover_input(source: &str) -> Result<(), CheckError> {
    parser::parse_judgment_source(source)?;
    Ok(())
}
