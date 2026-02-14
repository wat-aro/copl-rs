use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat2Term {
    Z,
    S(Box<CompareNat2Term>),
}

impl fmt::Display for CompareNat2Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat2Judgment {
    pub left: CompareNat2Term,
    pub right: CompareNat2Term,
}

impl fmt::Display for CompareNat2Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat2Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat2Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat2Derivation>,
}
