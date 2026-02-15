use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat3Term {
    Z,
    S(Box<CompareNat3Term>),
}

impl fmt::Display for CompareNat3Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat3Judgment {
    pub left: CompareNat3Term,
    pub right: CompareNat3Term,
}

impl fmt::Display for CompareNat3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat3Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat3Derivation>,
}
