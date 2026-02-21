use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct NamelessML3Env(pub Vec<String>);

impl fmt::Display for NamelessML3Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NamelessML3BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl NamelessML3BinOp {
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
pub enum NamedExpr {
    Int(i64),
    Bool(bool),
    Var(String),
    BinOp {
        op: NamelessML3BinOp,
        left: Box<NamedExpr>,
        right: Box<NamedExpr>,
    },
    If {
        condition: Box<NamedExpr>,
        then_branch: Box<NamedExpr>,
        else_branch: Box<NamedExpr>,
    },
    Let {
        name: String,
        bound_expr: Box<NamedExpr>,
        body: Box<NamedExpr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<NamedExpr>,
        body: Box<NamedExpr>,
    },
    Fun {
        param: String,
        body: Box<NamedExpr>,
    },
    App {
        func: Box<NamedExpr>,
        arg: Box<NamedExpr>,
    },
}

impl NamedExpr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Var(_) => 5,
            Self::App { .. } => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } | Self::Let { .. } | Self::LetRec { .. } | Self::Fun { .. } => 0,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent
            || matches!(self, Self::Int(value) if *value < 0 && parent >= self.precedence());
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
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for NamedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamelessExpr {
    Int(i64),
    Bool(bool),
    Index(usize),
    BinOp {
        op: NamelessML3BinOp,
        left: Box<NamelessExpr>,
        right: Box<NamelessExpr>,
    },
    If {
        condition: Box<NamelessExpr>,
        then_branch: Box<NamelessExpr>,
        else_branch: Box<NamelessExpr>,
    },
    Let {
        bound_expr: Box<NamelessExpr>,
        body: Box<NamelessExpr>,
    },
    LetRec {
        fun_body: Box<NamelessExpr>,
        body: Box<NamelessExpr>,
    },
    Fun {
        body: Box<NamelessExpr>,
    },
    App {
        func: Box<NamelessExpr>,
        arg: Box<NamelessExpr>,
    },
}

impl NamelessExpr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Index(_) => 5,
            Self::App { .. } => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } | Self::Let { .. } | Self::LetRec { .. } | Self::Fun { .. } => 0,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent
            || matches!(self, Self::Int(value) if *value < 0 && parent >= self.precedence());
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

impl fmt::Display for NamelessExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamelessML3Judgment {
    Translates {
        env: NamelessML3Env,
        named: NamedExpr,
        nameless: NamelessExpr,
    },
}

impl fmt::Display for NamelessML3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Translates {
                env,
                named,
                nameless,
            } => {
                if env.0.is_empty() {
                    write!(f, "|- {named} ==> {nameless}")
                } else {
                    write!(f, "{env} |- {named} ==> {nameless}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamelessML3Derivation {
    pub span: SourceSpan,
    pub judgment: NamelessML3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<NamelessML3Derivation>,
}

impl fmt::Display for NamelessML3Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &NamelessML3Derivation,
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
        NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Derivation, NamelessML3Env,
        NamelessML3Judgment,
    };
    use crate::core::SourceSpan;
    use crate::games::nameless_ml3::parser::parse_source;

    fn derivation(
        judgment: NamelessML3Judgment,
        rule_name: &str,
        subderivations: Vec<NamelessML3Derivation>,
    ) -> NamelessML3Derivation {
        NamelessML3Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            NamelessML3Judgment::Translates {
                env: NamelessML3Env::default(),
                named: NamedExpr::Int(1),
                nameless: NamelessExpr::Int(1),
            },
            "Tr-Int",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "|- 1 ==> 1 by Tr-Int {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            NamelessML3Judgment::Translates {
                env: NamelessML3Env::default(),
                named: NamedExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(NamedExpr::Int(1)),
                    right: Box::new(NamedExpr::Int(2)),
                },
                nameless: NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(NamelessExpr::Int(1)),
                    right: Box::new(NamelessExpr::Int(2)),
                },
            },
            "Tr-Plus",
            vec![
                derivation(
                    NamelessML3Judgment::Translates {
                        env: NamelessML3Env::default(),
                        named: NamedExpr::Int(1),
                        nameless: NamelessExpr::Int(1),
                    },
                    "Tr-Int",
                    Vec::new(),
                ),
                derivation(
                    NamelessML3Judgment::Translates {
                        env: NamelessML3Env::default(),
                        named: NamedExpr::Int(2),
                        nameless: NamelessExpr::Int(2),
                    },
                    "Tr-Int",
                    Vec::new(),
                ),
            ],
        );

        let expected = "\
|- 1 + 2 ==> 1 + 2 by Tr-Plus {
  |- 1 ==> 1 by Tr-Int {};
  |- 2 ==> 2 by Tr-Int {}
}";
        assert_eq!(derivation.to_string(), expected);
        parse_source(&derivation.to_string()).expect("formatted derivation should parse");
    }

    #[test]
    fn formats_named_and_nameless_app_with_parenthesized_negative_int_argument() {
        let named = NamedExpr::App {
            func: Box::new(NamedExpr::Var("f".to_string())),
            arg: Box::new(NamedExpr::Int(-2)),
        };
        assert_eq!(named.to_string(), "f (-2)");

        let nameless = NamelessExpr::App {
            func: Box::new(NamelessExpr::Index(1)),
            arg: Box::new(NamelessExpr::Int(-2)),
        };
        assert_eq!(nameless.to_string(), "#1 (-2)");
    }
}
