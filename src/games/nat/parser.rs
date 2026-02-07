use crate::core::CheckError;

use super::syntax::NatSource;

pub fn parse_source(source: &str) -> Result<NatSource<'_>, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::rule_violation("input is empty"));
    }
    Ok(NatSource::new(source))
}
