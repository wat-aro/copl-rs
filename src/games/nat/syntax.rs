#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatTerm {
    Z,
    S(Box<NatTerm>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NatOperator {
    Plus,
    Times,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatJudgment {
    pub left: NatTerm,
    pub operator: NatOperator,
    pub right: NatTerm,
    pub result: NatTerm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NatRule {
    PZero,
    PSucc,
    TZero,
    TSucc,
}

impl NatRule {
    pub fn parse(raw: &str) -> Option<Self> {
        match raw {
            "P-Zero" => Some(Self::PZero),
            "P-Succ" => Some(Self::PSucc),
            "T-Zero" => Some(Self::TZero),
            "T-Succ" => Some(Self::TSucc),
            _ => None,
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::PZero => "P-Zero",
            Self::PSucc => "P-Succ",
            Self::TZero => "T-Zero",
            Self::TSucc => "T-Succ",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatDerivation {
    pub judgment: NatJudgment,
    pub rule: NatRule,
    pub premises: Vec<NatDerivation>,
}
