use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1Value {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for EvalContML1Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalContML1BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalContML1BinOp {
    pub const fn symbol(self) -> &'static str {
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
pub enum EvalContML1Expr {
    Int(i64),
    Bool(bool),
    BinOp {
        op: EvalContML1BinOp,
        left: Box<EvalContML1Expr>,
        right: Box<EvalContML1Expr>,
    },
    If {
        condition: Box<EvalContML1Expr>,
        then_branch: Box<EvalContML1Expr>,
        else_branch: Box<EvalContML1Expr>,
    },
}

impl EvalContML1Expr {
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

impl fmt::Display for EvalContML1Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1ContFrame {
    EvalR {
        op: EvalContML1BinOp,
        right: EvalContML1Expr,
    },
    Plus {
        left: i64,
    },
    Minus {
        left: i64,
    },
    Times {
        left: i64,
    },
    Lt {
        left: i64,
    },
    If {
        then_branch: EvalContML1Expr,
        else_branch: EvalContML1Expr,
    },
}

impl fmt::Display for EvalContML1ContFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalR { op, right } => {
                write!(f, "{{_ {} ", op.symbol())?;
                right.fmt_with_precedence(f, op.precedence() + 1)?;
                write!(f, "}}")
            }
            Self::Plus { left } => write!(f, "{{{left} + _}}"),
            Self::Minus { left } => write!(f, "{{{left} - _}}"),
            Self::Times { left } => write!(f, "{{{left} * _}}"),
            Self::Lt { left } => write!(f, "{{{left} < _}}"),
            Self::If {
                then_branch,
                else_branch,
            } => write!(f, "{{if _ then {then_branch} else {else_branch}}}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalContML1Continuation {
    pub frames: Vec<EvalContML1ContFrame>,
    pub explicit_ret: bool,
}

impl EvalContML1Continuation {
    pub const fn hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: true,
        }
    }

    pub const fn implicit_hole() -> Self {
        Self {
            frames: Vec::new(),
            explicit_ret: false,
        }
    }

    pub fn semantic_eq(&self, other: &Self) -> bool {
        self.frames == other.frames
    }
}

impl fmt::Display for EvalContML1Continuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.frames.is_empty() {
            return write!(f, "_");
        }

        for (index, frame) in self.frames.iter().enumerate() {
            if index > 0 {
                write!(f, " >> ")?;
            }
            write!(f, "{frame}")?;
        }
        if self.explicit_ret {
            write!(f, " >> _")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalContML1Judgment {
    EvalTo {
        expr: EvalContML1Expr,
        continuation: EvalContML1Continuation,
        value: EvalContML1Value,
        has_continuation: bool,
    },
    ContEvalTo {
        input: EvalContML1Value,
        continuation: EvalContML1Continuation,
        value: EvalContML1Value,
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

impl fmt::Display for EvalContML1Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo {
                expr,
                continuation,
                value,
                has_continuation,
            } => {
                if *has_continuation || !continuation.frames.is_empty() || continuation.explicit_ret
                {
                    write!(f, "{expr} >> {continuation} evalto {value}")
                } else {
                    write!(f, "{expr} evalto {value}")
                }
            }
            Self::ContEvalTo {
                input,
                continuation,
                value,
            } => write!(f, "{input} => {continuation} evalto {value}"),
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
pub struct EvalContML1Derivation {
    pub span: SourceSpan,
    pub judgment: EvalContML1Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalContML1Derivation>,
}

impl fmt::Display for EvalContML1Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalContML1Derivation,
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
        EvalContML1BinOp, EvalContML1ContFrame, EvalContML1Continuation, EvalContML1Derivation,
        EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
    };
    use crate::core::SourceSpan;
    use crate::games::eval_cont_ml1::parser::parse_source;

    fn derivation(
        judgment: EvalContML1Judgment,
        rule_name: &str,
        subderivations: Vec<EvalContML1Derivation>,
    ) -> EvalContML1Derivation {
        EvalContML1Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalContML1Judgment::PlusIs {
                left: 3,
                right: 5,
                result: 8,
            },
            "B-Plus",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "3 plus 5 is 8 by B-Plus {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalContML1Judgment::EvalTo {
                expr: EvalContML1Expr::BinOp {
                    op: EvalContML1BinOp::Plus,
                    left: Box::new(EvalContML1Expr::Int(3)),
                    right: Box::new(EvalContML1Expr::Int(5)),
                },
                continuation: EvalContML1Continuation::implicit_hole(),
                value: EvalContML1Value::Int(8),
                has_continuation: false,
            },
            "E-BinOp",
            vec![derivation(
                EvalContML1Judgment::EvalTo {
                    expr: EvalContML1Expr::Int(3),
                    continuation: EvalContML1Continuation {
                        frames: vec![EvalContML1ContFrame::EvalR {
                            op: EvalContML1BinOp::Plus,
                            right: EvalContML1Expr::Int(5),
                        }],
                        explicit_ret: false,
                    },
                    value: EvalContML1Value::Int(8),
                    has_continuation: true,
                },
                "E-Int",
                vec![derivation(
                    EvalContML1Judgment::ContEvalTo {
                        input: EvalContML1Value::Int(3),
                        continuation: EvalContML1Continuation {
                            frames: vec![EvalContML1ContFrame::EvalR {
                                op: EvalContML1BinOp::Plus,
                                right: EvalContML1Expr::Int(5),
                            }],
                            explicit_ret: false,
                        },
                        value: EvalContML1Value::Int(8),
                    },
                    "C-EvalR",
                    vec![derivation(
                        EvalContML1Judgment::EvalTo {
                            expr: EvalContML1Expr::Int(5),
                            continuation: EvalContML1Continuation {
                                frames: vec![EvalContML1ContFrame::Plus { left: 3 }],
                                explicit_ret: false,
                            },
                            value: EvalContML1Value::Int(8),
                            has_continuation: true,
                        },
                        "E-Int",
                        vec![derivation(
                            EvalContML1Judgment::ContEvalTo {
                                input: EvalContML1Value::Int(5),
                                continuation: EvalContML1Continuation {
                                    frames: vec![EvalContML1ContFrame::Plus { left: 3 }],
                                    explicit_ret: false,
                                },
                                value: EvalContML1Value::Int(8),
                            },
                            "C-Plus",
                            vec![
                                derivation(
                                    EvalContML1Judgment::PlusIs {
                                        left: 3,
                                        right: 5,
                                        result: 8,
                                    },
                                    "B-Plus",
                                    Vec::new(),
                                ),
                                derivation(
                                    EvalContML1Judgment::ContEvalTo {
                                        input: EvalContML1Value::Int(8),
                                        continuation: EvalContML1Continuation::hole(),
                                        value: EvalContML1Value::Int(8),
                                    },
                                    "C-Ret",
                                    Vec::new(),
                                ),
                            ],
                        )],
                    )],
                )],
            )],
        );

        let expected = "\
3 + 5 evalto 8 by E-BinOp {
  3 >> {_ + 5} evalto 8 by E-Int {
    3 => {_ + 5} evalto 8 by C-EvalR {
      5 >> {3 + _} evalto 8 by E-Int {
        5 => {3 + _} evalto 8 by C-Plus {
          3 plus 5 is 8 by B-Plus {};
          8 => _ evalto 8 by C-Ret {}
        }
      }
    }
  }
}";
        assert_eq!(derivation.to_string(), expected);
        parse_source(&derivation.to_string()).expect("formatted derivation should parse");
    }

    #[test]
    fn formats_evalr_frame_with_parenthesized_lower_precedence_right_expr() {
        let frame = EvalContML1ContFrame::EvalR {
            op: EvalContML1BinOp::Times,
            right: EvalContML1Expr::BinOp {
                op: EvalContML1BinOp::Minus,
                left: Box::new(EvalContML1Expr::Int(1)),
                right: Box::new(EvalContML1Expr::Int(10)),
            },
        };

        assert_eq!(frame.to_string(), "{_ * (1 - 10)}");
    }
}
