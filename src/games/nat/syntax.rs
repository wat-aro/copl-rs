use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatTerm {
    Z,
    S(Box<NatTerm>),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatDerivation {
    pub span: SourceSpan,
    pub judgment: NatJudgment,
    pub rule_name: String,
    pub subderivations: Vec<NatDerivation>,
}
