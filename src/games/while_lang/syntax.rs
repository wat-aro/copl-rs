use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Store {
    bindings: Vec<(String, i64)>,
}

impl Store {
    pub fn new(bindings: Vec<(String, i64)>) -> Self {
        Self { bindings }
    }

    pub fn empty() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<i64> {
        self.bindings
            .iter()
            .find_map(|(key, value)| (key == name).then_some(*value))
    }

    pub fn update(&self, name: &str, value: i64) -> Self {
        let mut bindings = self.bindings.clone();
        if let Some((_, current)) = bindings.iter_mut().find(|(key, _)| key == name) {
            *current = value;
            return Self { bindings };
        }

        bindings.push((name.to_string(), value));
        Self { bindings }
    }
}

impl fmt::Display for Store {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.bindings.is_empty() {
            return write!(f, ".");
        }

        for (index, (name, value)) in self.bindings.iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{name} = {value}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhileAExp {
    Int(i64),
    Var(String),
    Plus(Box<WhileAExp>, Box<WhileAExp>),
    Minus(Box<WhileAExp>, Box<WhileAExp>),
    Times(Box<WhileAExp>, Box<WhileAExp>),
}

impl WhileAExp {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Var(_) => 3,
            Self::Times(_, _) => 2,
            Self::Plus(_, _) | Self::Minus(_, _) => 1,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int(value) => write!(f, "{value}")?,
            Self::Var(name) => write!(f, "{name}")?,
            Self::Plus(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " + ")?;
                right.fmt_with_precedence(f, self.precedence() + 1)?;
            }
            Self::Minus(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " - ")?;
                right.fmt_with_precedence(f, self.precedence() + 1)?;
            }
            Self::Times(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " * ")?;
                right.fmt_with_precedence(f, self.precedence() + 1)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for WhileAExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhileBExp {
    Bool(bool),
    Not(Box<WhileBExp>),
    And(Box<WhileBExp>, Box<WhileBExp>),
    Or(Box<WhileBExp>, Box<WhileBExp>),
    Lt(Box<WhileAExp>, Box<WhileAExp>),
    Eq(Box<WhileAExp>, Box<WhileAExp>),
    Le(Box<WhileAExp>, Box<WhileAExp>),
}

impl WhileBExp {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Bool(_) | Self::Lt(_, _) | Self::Eq(_, _) | Self::Le(_, _) => 4,
            Self::Not(_) => 3,
            Self::And(_, _) => 2,
            Self::Or(_, _) => 1,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Bool(value) => {
                if *value {
                    write!(f, "true")?;
                } else {
                    write!(f, "false")?;
                }
            }
            Self::Not(inner) => {
                write!(f, "!")?;
                inner.fmt_with_precedence(f, self.precedence())?;
            }
            Self::And(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " && ")?;
                right.fmt_with_precedence(f, self.precedence() + 1)?;
            }
            Self::Or(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " || ")?;
                right.fmt_with_precedence(f, self.precedence() + 1)?;
            }
            Self::Lt(left, right) => {
                left.fmt_with_precedence(f, 0)?;
                write!(f, " < ")?;
                right.fmt_with_precedence(f, 0)?;
            }
            Self::Eq(left, right) => {
                left.fmt_with_precedence(f, 0)?;
                write!(f, " = ")?;
                right.fmt_with_precedence(f, 0)?;
            }
            Self::Le(left, right) => {
                left.fmt_with_precedence(f, 0)?;
                write!(f, " <= ")?;
                right.fmt_with_precedence(f, 0)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for WhileBExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhileCom {
    Skip,
    Assign {
        name: String,
        expr: WhileAExp,
    },
    Seq(Box<WhileCom>, Box<WhileCom>),
    If {
        cond: WhileBExp,
        then_branch: Box<WhileCom>,
        else_branch: Box<WhileCom>,
    },
    While {
        cond: WhileBExp,
        body: Box<WhileCom>,
    },
}

impl WhileCom {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Seq(_, _) => 1,
            Self::Skip | Self::Assign { .. } | Self::If { .. } | Self::While { .. } => 2,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Skip => write!(f, "skip")?,
            Self::Assign { name, expr } => write!(f, "{name} := {expr}")?,
            Self::Seq(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, "; ")?;
                right.fmt_with_precedence(f, self.precedence())?;
            }
            Self::If {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {cond} then ")?;
                then_branch.fmt_with_precedence(f, 0)?;
                write!(f, " else ")?;
                else_branch.fmt_with_precedence(f, 0)?;
            }
            Self::While { cond, body } => {
                write!(f, "while ({cond}) do ")?;
                body.fmt_with_precedence(f, 0)?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for WhileCom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhileJudgment {
    AEval {
        store: Store,
        expr: WhileAExp,
        value: i64,
    },
    BEval {
        store: Store,
        expr: WhileBExp,
        value: bool,
    },
    Changes {
        command: WhileCom,
        from: Store,
        to: Store,
    },
}

impl fmt::Display for WhileJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AEval { store, expr, value } => write!(f, "{store} |- {expr} evalto {value}"),
            Self::BEval { store, expr, value } => {
                let value = if *value { "true" } else { "false" };
                write!(f, "{store} |- {expr} evalto {value}")
            }
            Self::Changes { command, from, to } => write!(f, "{command} changes {from} to {to}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileDerivation {
    pub span: SourceSpan,
    pub judgment: WhileJudgment,
    pub rule_name: String,
    pub subderivations: Vec<WhileDerivation>,
}

impl fmt::Display for WhileDerivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &WhileDerivation,
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
    use super::{Store, WhileAExp, WhileBExp, WhileCom, WhileDerivation, WhileJudgment};
    use crate::core::SourceSpan;
    use crate::games::while_lang::parser::parse_source;

    fn derivation(
        judgment: WhileJudgment,
        rule_name: &str,
        subderivations: Vec<WhileDerivation>,
    ) -> WhileDerivation {
        WhileDerivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_store() {
        let store = Store::new(vec![("x".to_string(), 1), ("y".to_string(), 2)]);
        assert_eq!(store.to_string(), "x = 1, y = 2");
        assert_eq!(Store::empty().to_string(), ".");
    }

    #[test]
    fn formats_while_judgment() {
        let judgment = WhileJudgment::Changes {
            command: WhileCom::Assign {
                name: "x".to_string(),
                expr: WhileAExp::Plus(
                    Box::new(WhileAExp::Var("x".to_string())),
                    Box::new(WhileAExp::Int(1)),
                ),
            },
            from: Store::new(vec![("x".to_string(), 2)]),
            to: Store::new(vec![("x".to_string(), 3)]),
        };

        assert_eq!(judgment.to_string(), "x := x + 1 changes x = 2 to x = 3");
    }

    #[test]
    fn formats_nested_derivation() {
        let store = Store::new(vec![("x".to_string(), 0)]);
        let derivation = derivation(
            WhileJudgment::Changes {
                command: WhileCom::If {
                    cond: WhileBExp::Bool(true),
                    then_branch: Box::new(WhileCom::Skip),
                    else_branch: Box::new(WhileCom::Skip),
                },
                from: store.clone(),
                to: store.clone(),
            },
            "C-IfT",
            vec![
                derivation(
                    WhileJudgment::BEval {
                        store: store.clone(),
                        expr: WhileBExp::Bool(true),
                        value: true,
                    },
                    "B-Const",
                    Vec::new(),
                ),
                derivation(
                    WhileJudgment::Changes {
                        command: WhileCom::Skip,
                        from: store.clone(),
                        to: store.clone(),
                    },
                    "C-Skip",
                    Vec::new(),
                ),
            ],
        );

        let rendered = derivation.to_string();
        assert!(rendered.contains("by C-IfT"));
        assert!(parse_source(&rendered).is_ok());
    }
}
