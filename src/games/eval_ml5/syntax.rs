use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML5Value {
    Int(i64),
    Bool(bool),
    Closure {
        env: EvalML5Env,
        param: String,
        body: EvalML5Expr,
    },
    RecClosure {
        env: EvalML5Env,
        name: String,
        param: String,
        body: EvalML5Expr,
    },
    Nil,
    Cons {
        head: Box<EvalML5Value>,
        tail: Box<EvalML5Value>,
    },
}

impl EvalML5Value {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Cons { .. } => 1,
            _ => 2,
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
            Self::Closure { env, param, body } => {
                write!(f, "({env})[fun {param} -> {body}]")?;
            }
            Self::RecClosure {
                env,
                name,
                param,
                body,
            } => {
                write!(f, "({env})[rec {name} = fun {param} -> {body}]")?;
            }
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML5Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML5Binding {
    pub name: String,
    pub value: EvalML5Value,
}

impl fmt::Display for EvalML5Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalML5Env(pub Vec<EvalML5Binding>);

impl fmt::Display for EvalML5Env {
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
pub enum EvalML5BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML5BinOp {
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
            Self::Lt => 2,
            Self::Plus | Self::Minus => 3,
            Self::Times => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML5Pattern {
    Var(String),
    Wildcard,
    Nil,
    Cons {
        head: Box<EvalML5Pattern>,
        tail: Box<EvalML5Pattern>,
    },
}

impl EvalML5Pattern {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Cons { .. } => 1,
            _ => 2,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Var(name) => write!(f, "{name}")?,
            Self::Wildcard => write!(f, "_")?,
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML5Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML5MatchClause {
    pub pattern: EvalML5Pattern,
    pub body: EvalML5Expr,
}

impl fmt::Display for EvalML5MatchClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML5Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Nil,
    Cons {
        head: Box<EvalML5Expr>,
        tail: Box<EvalML5Expr>,
    },
    BinOp {
        op: EvalML5BinOp,
        left: Box<EvalML5Expr>,
        right: Box<EvalML5Expr>,
    },
    If {
        condition: Box<EvalML5Expr>,
        then_branch: Box<EvalML5Expr>,
        else_branch: Box<EvalML5Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalML5Expr>,
        body: Box<EvalML5Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<EvalML5Expr>,
        body: Box<EvalML5Expr>,
    },
    Fun {
        param: String,
        body: Box<EvalML5Expr>,
    },
    App {
        func: Box<EvalML5Expr>,
        arg: Box<EvalML5Expr>,
    },
    Match {
        scrutinee: Box<EvalML5Expr>,
        clauses: Vec<EvalML5MatchClause>,
    },
}

impl EvalML5Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Var(_) | Self::Nil => 6,
            Self::App { .. } => 5,
            Self::BinOp { op, .. } => op.precedence(),
            Self::Cons { .. } => 1,
            Self::If { .. } | Self::Let { .. } | Self::LetRec { .. } | Self::Fun { .. } => 0,
            Self::Match { .. } => 0,
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
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                if parent == 0
                    && (matches!(right.as_ref(), Self::If { .. })
                        || matches!(right.as_ref(), Self::Let { .. })
                        || matches!(right.as_ref(), Self::LetRec { .. })
                        || matches!(right.as_ref(), Self::Fun { .. })
                        || matches!(right.as_ref(), Self::Match { .. }))
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
            Self::Match { scrutinee, clauses } => {
                write!(f, "match ")?;
                scrutinee.fmt_with_precedence(f, 0)?;
                write!(f, " with ")?;
                let text = clauses
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{text}")?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML5Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML5Judgment {
    EvalTo {
        env: EvalML5Env,
        expr: EvalML5Expr,
        value: EvalML5Value,
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
    Matches {
        pattern: EvalML5Pattern,
        value: EvalML5Value,
        bindings: EvalML5Env,
    },
    NotMatch {
        pattern: EvalML5Pattern,
        value: EvalML5Value,
    },
}

impl fmt::Display for EvalML5Judgment {
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
            Self::Matches {
                pattern,
                value,
                bindings,
            } => {
                if bindings.0.is_empty() {
                    write!(f, "{pattern} matches {value} when ()")
                } else {
                    write!(f, "{pattern} matches {value} when ({bindings})")
                }
            }
            Self::NotMatch { pattern, value } => {
                write!(f, "{pattern} doesn't match {value}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML5Derivation {
    pub span: SourceSpan,
    pub judgment: EvalML5Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML5Derivation>,
}
