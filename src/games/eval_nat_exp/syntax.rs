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
pub enum EvalNatExpExpr {
    Nat(NatTerm),
    Plus(Box<EvalNatExpExpr>, Box<EvalNatExpExpr>),
    Times(Box<EvalNatExpExpr>, Box<EvalNatExpExpr>),
}

impl EvalNatExpExpr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Nat(_) => 3,
            Self::Times(_, _) => 2,
            Self::Plus(_, _) => 1,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Nat(term) => write!(f, "{term}")?,
            Self::Plus(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " + ")?;
                right.fmt_with_precedence(f, self.precedence())?;
            }
            Self::Times(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " * ")?;
                right.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalNatExpExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalNatExpJudgment {
    EvalTo {
        expr: EvalNatExpExpr,
        value: NatTerm,
    },
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

impl fmt::Display for EvalNatExpJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { expr, value } => write!(f, "{expr} evalto {value}"),
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
pub struct EvalNatExpDerivation {
    pub span: SourceSpan,
    pub judgment: EvalNatExpJudgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalNatExpDerivation>,
}
