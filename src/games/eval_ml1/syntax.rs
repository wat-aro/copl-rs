use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1Value {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for EvalML1Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalML1BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML1BinOp {
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
pub enum EvalML1Expr {
    Int(i64),
    Bool(bool),
    BinOp {
        op: EvalML1BinOp,
        left: Box<EvalML1Expr>,
        right: Box<EvalML1Expr>,
    },
    If {
        condition: Box<EvalML1Expr>,
        then_branch: Box<EvalML1Expr>,
        else_branch: Box<EvalML1Expr>,
    },
}

impl EvalML1Expr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Bool(_) => 4,
            Self::BinOp { op, .. } => op.precedence(),
            Self::If { .. } => 0,
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
            Self::BinOp { op, left, right } => {
                left.fmt_with_precedence(f, op.precedence())?;
                write!(f, " {} ", op.symbol())?;
                // At top level, `e + if ... then ... else ...` is readable without extra parens.
                if parent == 0 && matches!(right.as_ref(), Self::If { .. }) {
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
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML1Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1Judgment {
    EvalTo {
        expr: EvalML1Expr,
        value: EvalML1Value,
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

impl fmt::Display for EvalML1Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { expr, value } => write!(f, "{expr} evalto {value}"),
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
pub struct EvalML1Derivation {
    pub span: SourceSpan,
    pub judgment: EvalML1Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML1Derivation>,
}

impl fmt::Display for EvalML1Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalML1Derivation,
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
    use super::{EvalML1BinOp, EvalML1Derivation, EvalML1Expr, EvalML1Judgment, EvalML1Value};
    use crate::core::SourceSpan;
    use crate::games::eval_ml1::parser::parse_source;

    fn derivation(
        judgment: EvalML1Judgment,
        rule_name: &str,
        subderivations: Vec<EvalML1Derivation>,
    ) -> EvalML1Derivation {
        EvalML1Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalML1Judgment::EvalTo {
                expr: EvalML1Expr::Int(3),
                value: EvalML1Value::Int(3),
            },
            "E-Int",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "3 evalto 3 by E-Int {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalML1Judgment::EvalTo {
                expr: EvalML1Expr::BinOp {
                    op: EvalML1BinOp::Plus,
                    left: Box::new(EvalML1Expr::Int(3)),
                    right: Box::new(EvalML1Expr::Int(5)),
                },
                value: EvalML1Value::Int(8),
            },
            "E-Plus",
            vec![
                derivation(
                    EvalML1Judgment::EvalTo {
                        expr: EvalML1Expr::Int(3),
                        value: EvalML1Value::Int(3),
                    },
                    "E-Int",
                    Vec::new(),
                ),
                derivation(
                    EvalML1Judgment::EvalTo {
                        expr: EvalML1Expr::Int(5),
                        value: EvalML1Value::Int(5),
                    },
                    "E-Int",
                    Vec::new(),
                ),
                derivation(
                    EvalML1Judgment::PlusIs {
                        left: 3,
                        right: 5,
                        result: 8,
                    },
                    "B-Plus",
                    Vec::new(),
                ),
            ],
        );

        let rendered = derivation.to_string();
        let expected = "\
3 + 5 evalto 8 by E-Plus {
  3 evalto 3 by E-Int {};
  5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {}
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
