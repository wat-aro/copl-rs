use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat1Term {
    Z,
    S(Box<CompareNat1Term>),
}

impl fmt::Display for CompareNat1Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat1Judgment {
    pub left: CompareNat1Term,
    pub right: CompareNat1Term,
}

impl fmt::Display for CompareNat1Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat1Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat1Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat1Derivation>,
}
