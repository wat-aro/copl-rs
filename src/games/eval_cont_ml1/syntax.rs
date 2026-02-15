use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1Value {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for EvalContML1Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalContML1BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalContML1BinOp {
    pub const fn symbol(self) -> &'static str {
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
pub enum EvalContML1Expr {
    Int(i64),
    Bool(bool),
    BinOp {
        op: EvalContML1BinOp,
        left: Box<EvalContML1Expr>,
        right: Box<EvalContML1Expr>,
    },
    If {
        condition: Box<EvalContML1Expr>,
        then_branch: Box<EvalContML1Expr>,
        else_branch: Box<EvalContML1Expr>,
    },
}

impl EvalContML1Expr {
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

impl fmt::Display for EvalContML1Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1ContFrame {
    EvalR {
        op: EvalContML1BinOp,
        right: EvalContML1Expr,
    },
    Plus {
        left: i64,
    },
    Minus {
        left: i64,
    },
    Times {
        left: i64,
    },
    Lt {
        left: i64,
    },
    If {
        then_branch: EvalContML1Expr,
        else_branch: EvalContML1Expr,
    },
}

impl fmt::Display for EvalContML1ContFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalR { op, right } => write!(f, "{{_ {} {right}}}", op.symbol()),
            Self::Plus { left } => write!(f, "{{{left} + _}}"),
            Self::Minus { left } => write!(f, "{{{left} - _}}"),
            Self::Times { left } => write!(f, "{{{left} * _}}"),
            Self::Lt { left } => write!(f, "{{{left} < _}}"),
            Self::If {
                then_branch,
                else_branch,
            } => write!(f, "{{if _ then {then_branch} else {else_branch}}}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalContML1Continuation {
    pub frames: Vec<EvalContML1ContFrame>,
    pub explicit_ret: bool,
}

impl EvalContML1Continuation {
    pub const fn hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: true,
        }
    }

    pub const fn implicit_hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: false,
        }
    }

    pub fn semantic_eq(&self, other: &Self) -> bool {
        self.frames == other.frames
    }
}

impl fmt::Display for EvalContML1Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.frames.is_empty() {
            return write!(f, "_");
        }

        for (index, frame) in self.frames.iter().enumerate() {
            if index > 0 {
                write!(f, " >> ")?;
            }
            write!(f, "{frame}")?;
        }
        if self.explicit_ret {
            write!(f, " >> _")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1Judgment {
    EvalTo {
        expr: EvalContML1Expr,
        continuation: EvalContML1Continuation,
        value: EvalContML1Value,
        has_continuation: bool,
    },
    ContEvalTo {
        input: EvalContML1Value,
        continuation: EvalContML1Continuation,
        value: EvalContML1Value,
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

impl fmt::Display for EvalContML1Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo {
                expr,
                continuation,
                value,
                has_continuation,
            } => {
                if *has_continuation || !continuation.frames.is_empty() || continuation.explicit_ret
                {
                    write!(f, "{expr} >> {continuation} evalto {value}")
                } else {
                    write!(f, "{expr} evalto {value}")
                }
            }
            Self::ContEvalTo {
                input,
                continuation,
                value,
            } => write!(f, "{input} => {continuation} evalto {value}"),
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
pub struct EvalContML1Derivation {
    pub span: SourceSpan,
    pub judgment: EvalContML1Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalContML1Derivation>,
}
