use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatTerm {
    Z,
    S(Box<NatTerm>),
}

impl fmt::Display for NatTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatJudgment {
    PlusIs {
        left: NatTerm,
        right: NatTerm,
        result: NatTerm,
    },
    TimesIs {
        left: NatTerm,
        right: NatTerm,
        result: NatTerm,
    },
}

impl fmt::Display for NatJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PlusIs {
                left,
                right,
                result,
            } => write!(f, "{left} plus {right} is {result}"),
            Self::TimesIs {
                left,
                right,
                result,
            } => write!(f, "{left} times {right} is {result}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatDerivation {
    pub span: SourceSpan,
    pub judgment: NatJudgment,
    pub rule_name: String,
    pub subderivations: Vec<NatDerivation>,
}
