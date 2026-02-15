use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1ErrValue {
    Int(i64),
    Bool(bool),
    Error,
}

impl fmt::Display for EvalML1ErrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalML1ErrBinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML1ErrBinOp {
    const fn symbol(self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Lt => "<",
        }
    }

    const fn precedence(self) -> u8 {
        match self {
            Self::Lt => 1,
            Self::Plus | Self::Minus => 2,
            Self::Times => 3,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1ErrExpr {
    Int(i64),
    Bool(bool),
    BinOp {
        op: EvalML1ErrBinOp,
        left: Box<EvalML1ErrExpr>,
        right: Box<EvalML1ErrExpr>,
    },
    If {
        condition: Box<EvalML1ErrExpr>,
        then_branch: Box<EvalML1ErrExpr>,
        else_branch: Box<EvalML1ErrExpr>,
    },
}

impl EvalML1ErrExpr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } => 0,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int(value) => write!(f, "{value}")?,
            Self::Bool(value) => write!(f, "{value}")?,
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                // At top level, `e + if ... then ... else ...` is readable without extra parens.
                if parent == 0 && matches!(right.as_ref(), Self::If { .. }) {
                    right.fmt_with_precedence(f, 0)?;
                } else {
                    right.fmt_with_precedence(f, op.precedence())?;
                }
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if ")?;
                condition.fmt_with_precedence(f, 0)?;
                write!(f, " then ")?;
                then_branch.fmt_with_precedence(f, 0)?;
                write!(f, " else ")?;
                else_branch.fmt_with_precedence(f, 0)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML1ErrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1ErrJudgment {
    EvalTo {
        expr: EvalML1ErrExpr,
        value: EvalML1ErrValue,
    },
    PlusIs {
        left: i64,
        right: i64,
        result: i64,
    },
    MinusIs {
        left: i64,
        right: i64,
        result: i64,
    },
    TimesIs {
        left: i64,
        right: i64,
        result: i64,
    },
    LessThanIs {
        left: i64,
        right: i64,
        result: bool,
    },
}

impl fmt::Display for EvalML1ErrJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { expr, value } => write!(f, "{expr} evalto {value}"),
            Self::PlusIs {
                left,
                right,
                result,
            } => write!(f, "{left} plus {right} is {result}"),
            Self::MinusIs {
                left,
                right,
                result,
            } => write!(f, "{left} minus {right} is {result}"),
            Self::TimesIs {
                left,
                right,
                result,
            } => write!(f, "{left} times {right} is {result}"),
            Self::LessThanIs {
                left,
                right,
                result,
            } => write!(f, "{left} less than {right} is {result}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML1ErrDerivation {
    pub span: SourceSpan,
    pub judgment: EvalML1ErrJudgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML1ErrDerivation>,
}
