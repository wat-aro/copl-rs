use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyTypingML4Type {
    Int,
    Bool,
    Var(String),
    List(Box<PolyTypingML4Type>),
    Fun {
        param: Box<PolyTypingML4Type>,
        ret: Box<PolyTypingML4Type>,
    },
}

impl PolyTypingML4Type {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Fun { .. } => 0,
            Self::List(_) => 2,
            Self::Int | Self::Bool | Self::Var(_) => 3,
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
            Self::Var(name) => write!(f, "'{name}")?,
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

impl fmt::Display for PolyTypingML4Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyTypingML4TypeScheme {
    pub quantified: Vec<String>,
    pub ty: PolyTypingML4Type,
}

impl PolyTypingML4TypeScheme {
    pub fn mono(ty: PolyTypingML4Type) -> Self {
        Self {
            quantified: Vec::new(),
            ty,
        }
    }

    pub fn is_mono(&self) -> bool {
        self.quantified.is_empty()
    }
}

impl fmt::Display for PolyTypingML4TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.quantified.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            let vars = self
                .quantified
                .iter()
                .map(|name| format!("'{name}"))
                .collect::<Vec<_>>()
                .join(" ");
            write!(f, "{vars}.{}", self.ty)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyTypingML4Binding {
    pub name: String,
    pub scheme: PolyTypingML4TypeScheme,
}

impl fmt::Display for PolyTypingML4Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.scheme)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PolyTypingML4Env(pub Vec<PolyTypingML4Binding>);

impl fmt::Display for PolyTypingML4Env {
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
pub enum PolyTypingML4BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl PolyTypingML4BinOp {
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
pub enum PolyTypingML4Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Nil,
    Cons {
        head: Box<PolyTypingML4Expr>,
        tail: Box<PolyTypingML4Expr>,
    },
    BinOp {
        op: PolyTypingML4BinOp,
        left: Box<PolyTypingML4Expr>,
        right: Box<PolyTypingML4Expr>,
    },
    If {
        condition: Box<PolyTypingML4Expr>,
        then_branch: Box<PolyTypingML4Expr>,
        else_branch: Box<PolyTypingML4Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<PolyTypingML4Expr>,
        body: Box<PolyTypingML4Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<PolyTypingML4Expr>,
        body: Box<PolyTypingML4Expr>,
    },
    Fun {
        param: String,
        body: Box<PolyTypingML4Expr>,
    },
    App {
        func: Box<PolyTypingML4Expr>,
        arg: Box<PolyTypingML4Expr>,
    },
    Match {
        scrutinee: Box<PolyTypingML4Expr>,
        nil_case: Box<PolyTypingML4Expr>,
        head_name: String,
        tail_name: String,
        cons_case: Box<PolyTypingML4Expr>,
    },
}

impl PolyTypingML4Expr {
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
                    right.fmt_with_precedence(f, op.precedence() + 1)?;
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

impl fmt::Display for PolyTypingML4Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyTypingML4Judgment {
    HasType {
        env: PolyTypingML4Env,
        expr: PolyTypingML4Expr,
        ty: PolyTypingML4Type,
    },
}

impl fmt::Display for PolyTypingML4Judgment {
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
pub struct PolyTypingML4Derivation {
    pub span: SourceSpan,
    pub judgment: PolyTypingML4Judgment,
    pub rule_name: String,
    pub subderivations: Vec<PolyTypingML4Derivation>,
}

impl fmt::Display for PolyTypingML4Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &PolyTypingML4Derivation,
    f: &mut fmt::Formatter<'_>,
    indent: usize,
) -> fmt::Result {
    f.write_str(&"  ".repeat(indent))?;
    write!(f, "{} by {}", derivation.judgment, derivation.rule_name)?;
    if derivation.subderivations.is_empty() {
        return write!(f, " {{}}");
    }

    writeln!(f, " {{")?;
    for (index, subderivation) in derivation.subderivations.iter().enumerate() {
        format_derivation(subderivation, f, indent + 1)?;
        if index + 1 < derivation.subderivations.len() {
            writeln!(f, ";")?;
        } else {
            writeln!(f)?;
        }
    }
    f.write_str(&"  ".repeat(indent))?;
    write!(f, "}}")
}

#[cfg(test)]
mod tests {
    use super::{
        PolyTypingML4BinOp, PolyTypingML4Derivation, PolyTypingML4Env, PolyTypingML4Expr,
        PolyTypingML4Judgment, PolyTypingML4Type,
    };
    use crate::core::SourceSpan;
    use crate::games::poly_typing_ml4::parser::parse_source;

    fn derivation(
        judgment: PolyTypingML4Judgment,
        rule_name: &str,
        subderivations: Vec<PolyTypingML4Derivation>,
    ) -> PolyTypingML4Derivation {
        PolyTypingML4Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            PolyTypingML4Judgment::HasType {
                env: PolyTypingML4Env::default(),
                expr: PolyTypingML4Expr::Int(1),
                ty: PolyTypingML4Type::Int,
            },
            "T-Int",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "|- 1 : int by T-Int {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            PolyTypingML4Judgment::HasType {
                env: PolyTypingML4Env::default(),
                expr: PolyTypingML4Expr::BinOp {
                    op: PolyTypingML4BinOp::Plus,
                    left: Box::new(PolyTypingML4Expr::Int(1)),
                    right: Box::new(PolyTypingML4Expr::Int(2)),
                },
                ty: PolyTypingML4Type::Int,
            },
            "T-Plus",
            vec![
                derivation(
                    PolyTypingML4Judgment::HasType {
                        env: PolyTypingML4Env::default(),
                        expr: PolyTypingML4Expr::Int(1),
                        ty: PolyTypingML4Type::Int,
                    },
                    "T-Int",
                    Vec::new(),
                ),
                derivation(
                    PolyTypingML4Judgment::HasType {
                        env: PolyTypingML4Env::default(),
                        expr: PolyTypingML4Expr::Int(2),
                        ty: PolyTypingML4Type::Int,
                    },
                    "T-Int",
                    Vec::new(),
                ),
            ],
        );

        let expected = "\
|- 1 + 2 : int by T-Plus {
  |- 1 : int by T-Int {};
  |- 2 : int by T-Int {}
}";
        assert_eq!(derivation.to_string(), expected);
        parse_source(&derivation.to_string()).expect("formatted derivation should parse");
    }
}
