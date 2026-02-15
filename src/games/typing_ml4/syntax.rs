use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypingML4Type {
    Int,
    Bool,
    List(Box<TypingML4Type>),
    Fun {
        param: Box<TypingML4Type>,
        ret: Box<TypingML4Type>,
    },
}

impl TypingML4Type {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Fun { .. } => 0,
            Self::List(_) => 2,
            Self::Int | Self::Bool => 3,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int => write!(f, "int")?,
            Self::Bool => write!(f, "bool")?,
            Self::List(inner) => {
                inner.fmt_with_precedence(f, self.precedence())?;
                write!(f, " list")?;
            }
            Self::Fun { param, ret } => {
                param.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " -> ")?;
                ret.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for TypingML4Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypingML4Binding {
    pub name: String,
    pub ty: TypingML4Type,
}

impl fmt::Display for TypingML4Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypingML4Env(pub Vec<TypingML4Binding>);

impl fmt::Display for TypingML4Env {
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
pub enum TypingML4BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl TypingML4BinOp {
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
pub enum TypingML4Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Nil,
    Cons {
        head: Box<TypingML4Expr>,
        tail: Box<TypingML4Expr>,
    },
    BinOp {
        op: TypingML4BinOp,
        left: Box<TypingML4Expr>,
        right: Box<TypingML4Expr>,
    },
    If {
        condition: Box<TypingML4Expr>,
        then_branch: Box<TypingML4Expr>,
        else_branch: Box<TypingML4Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<TypingML4Expr>,
        body: Box<TypingML4Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<TypingML4Expr>,
        body: Box<TypingML4Expr>,
    },
    Fun {
        param: String,
        body: Box<TypingML4Expr>,
    },
    App {
        func: Box<TypingML4Expr>,
        arg: Box<TypingML4Expr>,
    },
    Match {
        scrutinee: Box<TypingML4Expr>,
        nil_case: Box<TypingML4Expr>,
        head_name: String,
        tail_name: String,
        cons_case: Box<TypingML4Expr>,
    },
}

impl TypingML4Expr {
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
            Self::Match {
                scrutinee,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            } => {
                write!(f, "match ")?;
                scrutinee.fmt_with_precedence(f, 0)?;
                write!(f, " with [] -> ")?;
                nil_case.fmt_with_precedence(f, 0)?;
                write!(f, " | {head_name} :: {tail_name} -> ")?;
                cons_case.fmt_with_precedence(f, 0)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for TypingML4Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypingML4Judgment {
    HasType {
        env: TypingML4Env,
        expr: TypingML4Expr,
        ty: TypingML4Type,
    },
}

impl fmt::Display for TypingML4Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::HasType { env, expr, ty } => {
                if env.0.is_empty() {
                    write!(f, "|- {expr} : {ty}")
                } else {
                    write!(f, "{env} |- {expr} : {ty}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypingML4Derivation {
    pub span: SourceSpan,
    pub judgment: TypingML4Judgment,
    pub rule_name: String,
    pub subderivations: Vec<TypingML4Derivation>,
}
