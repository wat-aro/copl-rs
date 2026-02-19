use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1ErrValue {
    Int(i64),
    Bool(bool),
    Error,
}

impl fmt::Display for EvalML1ErrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalML1ErrBinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML1ErrBinOp {
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
pub enum EvalML1ErrExpr {
    Int(i64),
    Bool(bool),
    BinOp {
        op: EvalML1ErrBinOp,
        left: Box<EvalML1ErrExpr>,
        right: Box<EvalML1ErrExpr>,
    },
    If {
        condition: Box<EvalML1ErrExpr>,
        then_branch: Box<EvalML1ErrExpr>,
        else_branch: Box<EvalML1ErrExpr>,
    },
}

impl EvalML1ErrExpr {
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
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML1ErrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML1ErrJudgment {
    EvalTo {
        expr: EvalML1ErrExpr,
        value: EvalML1ErrValue,
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

impl fmt::Display for EvalML1ErrJudgment {
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
pub struct EvalML1ErrDerivation {
    pub span: SourceSpan,
    pub judgment: EvalML1ErrJudgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML1ErrDerivation>,
}

impl fmt::Display for EvalML1ErrDerivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalML1ErrDerivation,
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
        EvalML1ErrBinOp, EvalML1ErrDerivation, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
    };
    use crate::core::SourceSpan;
    use crate::games::eval_ml1_err::parser::parse_source;

    fn derivation(
        judgment: EvalML1ErrJudgment,
        rule_name: &str,
        subderivations: Vec<EvalML1ErrDerivation>,
    ) -> EvalML1ErrDerivation {
        EvalML1ErrDerivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: EvalML1ErrExpr::Bool(true),
                value: EvalML1ErrValue::Bool(true),
            },
            "E-Bool",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "true evalto true by E-Bool {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalML1ErrJudgment::EvalTo {
                expr: EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Plus,
                    left: Box::new(EvalML1ErrExpr::Int(1)),
                    right: Box::new(EvalML1ErrExpr::Bool(true)),
                },
                value: EvalML1ErrValue::Error,
            },
            "E-PlusBoolR",
            vec![derivation(
                EvalML1ErrJudgment::EvalTo {
                    expr: EvalML1ErrExpr::Bool(true),
                    value: EvalML1ErrValue::Bool(true),
                },
                "E-Bool",
                Vec::new(),
            )],
        );

        let rendered = derivation.to_string();
        let expected = "\
1 + true evalto error by E-PlusBoolR {
  true evalto true by E-Bool {}
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
