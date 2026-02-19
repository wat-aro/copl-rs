use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML2Value {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for EvalML2Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML2Binding {
    pub name: String,
    pub value: EvalML2Value,
}

impl fmt::Display for EvalML2Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalML2Env(pub Vec<EvalML2Binding>);

impl fmt::Display for EvalML2Env {
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
pub enum EvalML2BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML2BinOp {
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
pub enum EvalML2Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    BinOp {
        op: EvalML2BinOp,
        left: Box<EvalML2Expr>,
        right: Box<EvalML2Expr>,
    },
    If {
        condition: Box<EvalML2Expr>,
        then_branch: Box<EvalML2Expr>,
        else_branch: Box<EvalML2Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalML2Expr>,
        body: Box<EvalML2Expr>,
    },
}

impl EvalML2Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Var(_) => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } | Self::Let { .. } => 0,
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
                        || matches!(right.as_ref(), Self::Let { .. }))
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
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML2Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML2Judgment {
    EvalTo {
        env: EvalML2Env,
        expr: EvalML2Expr,
        value: EvalML2Value,
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

impl fmt::Display for EvalML2Judgment {
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
pub struct EvalML2Derivation {
    pub span: SourceSpan,
    pub judgment: EvalML2Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML2Derivation>,
}

impl fmt::Display for EvalML2Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalML2Derivation,
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
        EvalML2BinOp, EvalML2Derivation, EvalML2Env, EvalML2Expr, EvalML2Judgment, EvalML2Value,
    };
    use crate::core::SourceSpan;
    use crate::games::eval_ml2::parser::parse_source;

    fn derivation(
        judgment: EvalML2Judgment,
        rule_name: &str,
        subderivations: Vec<EvalML2Derivation>,
    ) -> EvalML2Derivation {
        EvalML2Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalML2Judgment::EvalTo {
                env: EvalML2Env::default(),
                expr: EvalML2Expr::Int(3),
                value: EvalML2Value::Int(3),
            },
            "E-Int",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "|- 3 evalto 3 by E-Int {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalML2Judgment::EvalTo {
                env: EvalML2Env::default(),
                expr: EvalML2Expr::BinOp {
                    op: EvalML2BinOp::Plus,
                    left: Box::new(EvalML2Expr::Int(3)),
                    right: Box::new(EvalML2Expr::Int(5)),
                },
                value: EvalML2Value::Int(8),
            },
            "E-Plus",
            vec![
                derivation(
                    EvalML2Judgment::EvalTo {
                        env: EvalML2Env::default(),
                        expr: EvalML2Expr::Int(3),
                        value: EvalML2Value::Int(3),
                    },
                    "E-Int",
                    Vec::new(),
                ),
                derivation(
                    EvalML2Judgment::EvalTo {
                        env: EvalML2Env::default(),
                        expr: EvalML2Expr::Int(5),
                        value: EvalML2Value::Int(5),
                    },
                    "E-Int",
                    Vec::new(),
                ),
                derivation(
                    EvalML2Judgment::PlusIs {
                        left: 3,
                        right: 5,
                        result: 8,
                    },
                    "B-Plus",
                    Vec::new(),
                ),
            ],
        );

        let expected = "\
|- 3 + 5 evalto 8 by E-Plus {
  |- 3 evalto 3 by E-Int {};
  |- 5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {}
}";
        assert_eq!(derivation.to_string(), expected);
        parse_source(&derivation.to_string()).expect("formatted derivation should parse");
    }
}
