use crate::core::SourceSpan;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatDerivation {
    pub span: SourceSpan,
    pub judgment: NatJudgment,
    pub rule_name: String,
    pub subderivations: Vec<NatDerivation>,
}
