use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML3Value {
    Int(i64),
    Bool(bool),
    Closure {
        env: EvalML3Env,
        param: String,
        body: EvalML3Expr,
    },
    RecClosure {
        env: EvalML3Env,
        name: String,
        param: String,
        body: EvalML3Expr,
    },
}

impl fmt::Display for EvalML3Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Closure { env, param, body } => {
                write!(f, "({env})[fun {param} -> {body}]")
            }
            Self::RecClosure {
                env,
                name,
                param,
                body,
            } => {
                write!(f, "({env})[rec {name} = fun {param} -> {body}]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML3Binding {
    pub name: String,
    pub value: EvalML3Value,
}

impl fmt::Display for EvalML3Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalML3Env(pub Vec<EvalML3Binding>);

impl fmt::Display for EvalML3Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = self
            .0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{text}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalML3BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML3BinOp {
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
pub enum EvalML3Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    BinOp {
        op: EvalML3BinOp,
        left: Box<EvalML3Expr>,
        right: Box<EvalML3Expr>,
    },
    If {
        condition: Box<EvalML3Expr>,
        then_branch: Box<EvalML3Expr>,
        else_branch: Box<EvalML3Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalML3Expr>,
        body: Box<EvalML3Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<EvalML3Expr>,
        body: Box<EvalML3Expr>,
    },
    Fun {
        param: String,
        body: Box<EvalML3Expr>,
    },
    App {
        func: Box<EvalML3Expr>,
        arg: Box<EvalML3Expr>,
    },
}

impl EvalML3Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Var(_) => 5,
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
            Self::Var(name) => write!(f, "{name}")?,
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
            Self::Let {
                name,
                bound_expr,
                body,
            } => {
                write!(f, "let {name} = ")?;
                bound_expr.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::LetRec {
                name,
                param,
                fun_body,
                body,
            } => {
                write!(f, "let rec {name} = fun {param} -> ")?;
                fun_body.fmt_with_precedence(f, 0)?;
                write!(f, " in ")?;
                body.fmt_with_precedence(f, 0)?;
            }
            Self::Fun { param, body } => {
                write!(f, "fun {param} -> ")?;
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

impl fmt::Display for EvalML3Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML3Judgment {
    EvalTo {
        env: EvalML3Env,
        expr: EvalML3Expr,
        value: EvalML3Value,
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

impl fmt::Display for EvalML3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { expr, value, env } => {
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
pub struct EvalML3Derivation {
    pub span: SourceSpan,
    pub judgment: EvalML3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML3Derivation>,
}
