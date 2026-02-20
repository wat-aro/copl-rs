use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalRefML3Value {
    Int(i64),
    Unit,
    Loc(String),
}

impl fmt::Display for EvalRefML3Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Unit => write!(f, "()"),
            Self::Loc(name) => write!(f, "@{name}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalRefML3Binding {
    pub name: String,
    pub value: EvalRefML3Value,
}

impl fmt::Display for EvalRefML3Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalRefML3Env(pub Vec<EvalRefML3Binding>);

impl fmt::Display for EvalRefML3Env {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalRefML3StoreEntry {
    pub location: String,
    pub value: EvalRefML3Value,
}

impl fmt::Display for EvalRefML3StoreEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{} = {}", self.location, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalRefML3Store(pub Vec<EvalRefML3StoreEntry>);

impl EvalRefML3Store {
    pub fn lookup(&self, location: &str) -> Option<&EvalRefML3Value> {
        self.0
            .iter()
            .find(|entry| entry.location == location)
            .map(|entry| &entry.value)
    }

    pub fn contains(&self, location: &str) -> bool {
        self.lookup(location).is_some()
    }

    pub fn with_updated(&self, location: &str, value: EvalRefML3Value) -> Option<Self> {
        let mut next = self.0.clone();
        let entry = next.iter_mut().find(|entry| entry.location == location)?;
        entry.value = value;
        Some(Self(next))
    }

    pub fn with_appended(&self, location: String, value: EvalRefML3Value) -> Self {
        let mut next = self.0.clone();
        next.push(EvalRefML3StoreEntry { location, value });
        Self(next)
    }
}

impl fmt::Display for EvalRefML3Store {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "()");
        }

        let text = self
            .0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{text}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalRefML3Expr {
    Int(i64),
    Unit,
    Var(String),
    Loc(String),
    Let {
        name: String,
        bound_expr: Box<EvalRefML3Expr>,
        body: Box<EvalRefML3Expr>,
    },
    Ref {
        expr: Box<EvalRefML3Expr>,
    },
    Deref {
        expr: Box<EvalRefML3Expr>,
    },
    Assign {
        target: Box<EvalRefML3Expr>,
        value: Box<EvalRefML3Expr>,
    },
}

impl EvalRefML3Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Let { .. } => 0,
            Self::Assign { .. } => 1,
            Self::Ref { .. } | Self::Deref { .. } => 3,
            Self::Int(_) | Self::Unit | Self::Var(_) | Self::Loc(_) => 4,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Int(value) => write!(f, "{value}")?,
            Self::Unit => write!(f, "()")?,
            Self::Var(name) => write!(f, "{name}")?,
            Self::Loc(name) => write!(f, "@{name}")?,
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
            Self::Ref { expr } => {
                write!(f, "ref ")?;
                expr.fmt_with_precedence(f, self.precedence())?;
            }
            Self::Deref { expr } => {
                write!(f, "!")?;
                expr.fmt_with_precedence(f, self.precedence())?;
            }
            Self::Assign { target, value } => {
                target.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " := ")?;
                value.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl fmt::Display for EvalRefML3Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalRefML3Judgment {
    EvalTo {
        env: EvalRefML3Env,
        expr: EvalRefML3Expr,
        store: EvalRefML3Store,
        value: EvalRefML3Value,
        result_store: EvalRefML3Store,
    },
}

impl fmt::Display for EvalRefML3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo {
                env,
                expr,
                store,
                value,
                result_store,
            } => {
                if env.0.is_empty() {
                    write!(f, "|- {expr} / {store} evalto {value} / {result_store}")
                } else {
                    write!(
                        f,
                        "{env} |- {expr} / {store} evalto {value} / {result_store}"
                    )
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalRefML3Derivation {
    pub span: SourceSpan,
    pub judgment: EvalRefML3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalRefML3Derivation>,
}

impl fmt::Display for EvalRefML3Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_derivation(self, f, 0)
    }
}

fn fmt_derivation(
    derivation: &EvalRefML3Derivation,
    f: &mut fmt::Formatter<'_>,
    indent: usize,
) -> fmt::Result {
    write!(f, "{} by {}", derivation.judgment, derivation.rule_name)?;
    if derivation.subderivations.is_empty() {
        return write!(f, " {{}}");
    }

    writeln!(f, " {{")?;
    for (index, sub) in derivation.subderivations.iter().enumerate() {
        write!(f, "{}", " ".repeat(indent + 2))?;
        fmt_derivation(sub, f, indent + 2)?;
        if index + 1 != derivation.subderivations.len() {
            writeln!(f, ";")?;
        } else {
            writeln!(f)?;
        }
    }
    write!(f, "{}", " ".repeat(indent))?;
    write!(f, "}}")
}

#[cfg(test)]
mod tests {
    use super::{
        EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr, EvalRefML3Judgment, EvalRefML3Store,
        EvalRefML3Value,
    };
    use crate::core::SourceSpan;

    #[test]
    fn formats_leaf_derivation() {
        let derivation = EvalRefML3Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment: EvalRefML3Judgment::EvalTo {
                env: EvalRefML3Env::default(),
                expr: EvalRefML3Expr::Int(1),
                store: EvalRefML3Store::default(),
                value: EvalRefML3Value::Int(1),
                result_store: EvalRefML3Store::default(),
            },
            rule_name: "E-Int".to_string(),
            subderivations: Vec::new(),
        };

        assert_eq!(
            derivation.to_string(),
            "|- 1 / () evalto 1 / () by E-Int {}"
        );
    }
}
