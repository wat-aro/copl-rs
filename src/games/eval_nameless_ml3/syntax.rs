use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalNamelessML3Value {
    Int(i64),
    Bool(bool),
    Closure {
        env: EvalNamelessML3Env,
        body: EvalNamelessML3Expr,
    },
    RecClosure {
        env: EvalNamelessML3Env,
        body: EvalNamelessML3Expr,
    },
}

impl fmt::Display for EvalNamelessML3Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Closure { env, body } => write!(f, "({env})[fun . -> {body}]"),
            Self::RecClosure { env, body } => write!(f, "({env})[rec . = fun . -> {body}]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalNamelessML3Env(pub Vec<EvalNamelessML3Value>);

impl fmt::Display for EvalNamelessML3Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalNamelessML3BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalNamelessML3BinOp {
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
pub enum EvalNamelessML3Expr {
    Int(i64),
    Bool(bool),
    Index(usize),
    BinOp {
        op: EvalNamelessML3BinOp,
        left: Box<EvalNamelessML3Expr>,
        right: Box<EvalNamelessML3Expr>,
    },
    If {
        condition: Box<EvalNamelessML3Expr>,
        then_branch: Box<EvalNamelessML3Expr>,
        else_branch: Box<EvalNamelessML3Expr>,
    },
    Let {
        bound_expr: Box<EvalNamelessML3Expr>,
        body: Box<EvalNamelessML3Expr>,
    },
    LetRec {
        fun_body: Box<EvalNamelessML3Expr>,
        body: Box<EvalNamelessML3Expr>,
    },
    Fun {
        body: Box<EvalNamelessML3Expr>,
    },
    App {
        func: Box<EvalNamelessML3Expr>,
        arg: Box<EvalNamelessML3Expr>,
    },
}

impl EvalNamelessML3Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Index(_) => 5,
            Self::App { .. } => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } | Self::Let { .. } | Self::LetRec { .. } | Self::Fun { .. } => 0,
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
            Self::Index(value) => write!(f, "#{value}")?,
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                if parent == 0
                    && (matches!(right.as_ref(), Self::If { .. })
                        || matches!(right.as_ref(), Self::Let { .. })
                        || matches!(right.as_ref(), Self::LetRec { .. })
                        || matches!(right.as_ref(), Self::Fun { .. }))
                {
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
            Self::Let { bound_expr, body } => {
                write!(f, "let . = ")?;
                bound_expr.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::LetRec { fun_body, body } => {
                write!(f, "let rec . = fun . -> ")?;
                fun_body.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::Fun { body } => {
                write!(f, "fun . -> ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::App { func, arg } => {
                func.fmt_with_precedence(f, self.precedence())?;
                write!(f, " ")?;
                arg.fmt_with_precedence(f, self.precedence() + 1)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalNamelessML3Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalNamelessML3Judgment {
    EvalTo {
        env: EvalNamelessML3Env,
        expr: EvalNamelessML3Expr,
        value: EvalNamelessML3Value,
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

impl fmt::Display for EvalNamelessML3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { env, expr, value } => {
                if env.0.is_empty() {
                    write!(f, "|- {expr} evalto {value}")
                } else {
                    write!(f, "{env} |- {expr} evalto {value}")
                }
            }
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
pub struct EvalNamelessML3Derivation {
    pub span: SourceSpan,
    pub judgment: EvalNamelessML3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalNamelessML3Derivation>,
}
