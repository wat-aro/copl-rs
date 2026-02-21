use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalRefML3BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalRefML3BinOp {
    const fn symbol(self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Lt => "<",
        }
    }

    pub const fn precedence(self) -> u8 {
        match self {
            Self::Lt => 2,
            Self::Plus | Self::Minus => 3,
            Self::Times => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalRefML3Value {
    Int(i64),
    Bool(bool),
    Unit,
    Loc(String),
    Closure {
        env: EvalRefML3Env,
        param: String,
        body: EvalRefML3Expr,
    },
    RecClosure {
        env: EvalRefML3Env,
        name: String,
        param: String,
        body: EvalRefML3Expr,
    },
}

impl fmt::Display for EvalRefML3Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Unit => write!(f, "()"),
            Self::Loc(name) => write!(f, "@{name}"),
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
    Bool(bool),
    Unit,
    Var(String),
    Loc(String),
    BinOp {
        op: EvalRefML3BinOp,
        left: Box<EvalRefML3Expr>,
        right: Box<EvalRefML3Expr>,
    },
    If {
        condition: Box<EvalRefML3Expr>,
        then_branch: Box<EvalRefML3Expr>,
        else_branch: Box<EvalRefML3Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalRefML3Expr>,
        body: Box<EvalRefML3Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<EvalRefML3Expr>,
        body: Box<EvalRefML3Expr>,
    },
    Fun {
        param: String,
        body: Box<EvalRefML3Expr>,
    },
    App {
        func: Box<EvalRefML3Expr>,
        arg: Box<EvalRefML3Expr>,
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
            Self::If { .. } | Self::Let { .. } | Self::LetRec { .. } | Self::Fun { .. } => 0,
            Self::Assign { .. } => 1,
            Self::BinOp { op, .. } => op.precedence(),
            Self::App { .. } => 5,
            Self::Ref { .. } | Self::Deref { .. } => 6,
            Self::Int(_) | Self::Bool(_) | Self::Unit | Self::Var(_) | Self::Loc(_) => 6,
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
            Self::Unit => write!(f, "()")?,
            Self::Var(name) => write!(f, "{name}")?,
            Self::Loc(name) => write!(f, "@{name}")?,
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                right.fmt_with_precedence(f, op.precedence() + 1)?;
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalRefML3Judgment {
    EvalTo {
        env: EvalRefML3Env,
        expr: EvalRefML3Expr,
        store: EvalRefML3Store,
        value: EvalRefML3Value,
        result_store: EvalRefML3Store,
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
                if store.0.is_empty() {
                    if env.0.is_empty() {
                        write!(f, "|- {expr} evalto {value}")?;
                    } else {
                        write!(f, "{env} |- {expr} evalto {value}")?;
                    }
                } else if env.0.is_empty() {
                    write!(f, "{store} / |- {expr} evalto {value}")?;
                } else {
                    write!(f, "{store} / {env} |- {expr} evalto {value}")?;
                }

                if !store.0.is_empty() || !result_store.0.is_empty() {
                    write!(f, " / {result_store}")?;
                }
                Ok(())
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
        EvalRefML3BinOp, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr, EvalRefML3Judgment,
        EvalRefML3Store, EvalRefML3Value,
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

        assert_eq!(derivation.to_string(), "|- 1 evalto 1 by E-Int {}");
    }

    #[test]
    fn formats_app_with_parenthesized_negative_int_argument() {
        let app = EvalRefML3Expr::App {
            func: Box::new(EvalRefML3Expr::Var("f".to_string())),
            arg: Box::new(EvalRefML3Expr::Int(-2)),
        };
        assert_eq!(app.to_string(), "f (-2)");
    }

    #[test]
    fn formats_assign_with_right_associativity() {
        let right_assoc = EvalRefML3Expr::Assign {
            target: Box::new(EvalRefML3Expr::Var("x".to_string())),
            value: Box::new(EvalRefML3Expr::Assign {
                target: Box::new(EvalRefML3Expr::Var("y".to_string())),
                value: Box::new(EvalRefML3Expr::Int(1)),
            }),
        };
        assert_eq!(right_assoc.to_string(), "x := y := 1");

        let left_grouped = EvalRefML3Expr::Assign {
            target: Box::new(EvalRefML3Expr::Assign {
                target: Box::new(EvalRefML3Expr::Var("x".to_string())),
                value: Box::new(EvalRefML3Expr::Var("y".to_string())),
            }),
            value: Box::new(EvalRefML3Expr::Int(1)),
        };
        assert_eq!(left_grouped.to_string(), "(x := y) := 1");
    }

    #[test]
    fn formats_deref_and_application_precedence() {
        let app_of_deref = EvalRefML3Expr::App {
            func: Box::new(EvalRefML3Expr::Deref {
                expr: Box::new(EvalRefML3Expr::Var("f".to_string())),
            }),
            arg: Box::new(EvalRefML3Expr::Var("x".to_string())),
        };
        assert_eq!(app_of_deref.to_string(), "!f x");

        let deref_of_app = EvalRefML3Expr::Deref {
            expr: Box::new(EvalRefML3Expr::App {
                func: Box::new(EvalRefML3Expr::Var("f".to_string())),
                arg: Box::new(EvalRefML3Expr::Var("x".to_string())),
            }),
        };
        assert_eq!(deref_of_app.to_string(), "!(f x)");
    }

    #[test]
    fn formats_application_with_parenthesized_binop_argument() {
        let app = EvalRefML3Expr::App {
            func: Box::new(EvalRefML3Expr::Var("f".to_string())),
            arg: Box::new(EvalRefML3Expr::BinOp {
                op: EvalRefML3BinOp::Plus,
                left: Box::new(EvalRefML3Expr::Int(1)),
                right: Box::new(EvalRefML3Expr::Int(2)),
            }),
        };
        assert_eq!(app.to_string(), "f (1 + 2)");
    }

    #[test]
    fn formats_eval_judgment_in_copl_store_env_order() {
        let judgment = EvalRefML3Judgment::EvalTo {
            env: EvalRefML3Env(vec![super::EvalRefML3Binding {
                name: "x".to_string(),
                value: EvalRefML3Value::Loc("l".to_string()),
            }]),
            expr: EvalRefML3Expr::Deref {
                expr: Box::new(EvalRefML3Expr::Var("x".to_string())),
            },
            store: EvalRefML3Store(vec![super::EvalRefML3StoreEntry {
                location: "l".to_string(),
                value: EvalRefML3Value::Int(2),
            }]),
            value: EvalRefML3Value::Int(2),
            result_store: EvalRefML3Store(vec![super::EvalRefML3StoreEntry {
                location: "l".to_string(),
                value: EvalRefML3Value::Int(2),
            }]),
        };

        assert_eq!(
            judgment.to_string(),
            "@l = 2 / x = @l |- !x evalto 2 / @l = 2"
        );
    }
}
