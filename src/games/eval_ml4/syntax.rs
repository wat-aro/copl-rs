use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML4Value {
    Int(i64),
    Bool(bool),
    Closure {
        env: EvalML4Env,
        param: String,
        body: EvalML4Expr,
    },
    RecClosure {
        env: EvalML4Env,
        name: String,
        param: String,
        body: EvalML4Expr,
    },
    Nil,
    Cons {
        head: Box<EvalML4Value>,
        tail: Box<EvalML4Value>,
    },
}

impl EvalML4Value {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Cons { .. } => 1,
            _ => 2,
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
            Self::Closure { env, param, body } => {
                write!(f, "({env})[fun {param} -> {body}]")?;
            }
            Self::RecClosure {
                env,
                name,
                param,
                body,
            } => {
                write!(f, "({env})[rec {name} = fun {param} -> {body}]")?;
            }
            Self::Nil => write!(f, "[]")?,
            Self::Cons { head, tail } => {
                head.fmt_with_precedence(f, self.precedence() + 1)?;
                write!(f, " :: ")?;
                tail.fmt_with_precedence(f, self.precedence())?;
            }
        }

        if needs_paren {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for EvalML4Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalML4Binding {
    pub name: String,
    pub value: EvalML4Value,
}

impl fmt::Display for EvalML4Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EvalML4Env(pub Vec<EvalML4Binding>);

impl fmt::Display for EvalML4Env {
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
pub enum EvalML4BinOp {
    Plus,
    Minus,
    Times,
    Lt,
}

impl EvalML4BinOp {
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
pub enum EvalML4Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Nil,
    Cons {
        head: Box<EvalML4Expr>,
        tail: Box<EvalML4Expr>,
    },
    BinOp {
        op: EvalML4BinOp,
        left: Box<EvalML4Expr>,
        right: Box<EvalML4Expr>,
    },
    If {
        condition: Box<EvalML4Expr>,
        then_branch: Box<EvalML4Expr>,
        else_branch: Box<EvalML4Expr>,
    },
    Let {
        name: String,
        bound_expr: Box<EvalML4Expr>,
        body: Box<EvalML4Expr>,
    },
    LetRec {
        name: String,
        param: String,
        fun_body: Box<EvalML4Expr>,
        body: Box<EvalML4Expr>,
    },
    Fun {
        param: String,
        body: Box<EvalML4Expr>,
    },
    App {
        func: Box<EvalML4Expr>,
        arg: Box<EvalML4Expr>,
    },
    Match {
        scrutinee: Box<EvalML4Expr>,
        nil_case: Box<EvalML4Expr>,
        head_name: String,
        tail_name: String,
        cons_case: Box<EvalML4Expr>,
    },
}

impl EvalML4Expr {
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

impl fmt::Display for EvalML4Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML4Judgment {
    EvalTo {
        env: EvalML4Env,
        expr: EvalML4Expr,
        value: EvalML4Value,
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

impl fmt::Display for EvalML4Judgment {
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
pub struct EvalML4Derivation {
    pub span: SourceSpan,
    pub judgment: EvalML4Judgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalML4Derivation>,
}

impl fmt::Display for EvalML4Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalML4Derivation,
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
        EvalML4BinOp, EvalML4Derivation, EvalML4Env, EvalML4Expr, EvalML4Judgment, EvalML4Value,
    };
    use crate::core::SourceSpan;
    use crate::games::eval_ml4::parser::parse_source;

    fn derivation(
        judgment: EvalML4Judgment,
        rule_name: &str,
        subderivations: Vec<EvalML4Derivation>,
    ) -> EvalML4Derivation {
        EvalML4Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalML4Judgment::EvalTo {
                env: EvalML4Env::default(),
                expr: EvalML4Expr::Nil,
                value: EvalML4Value::Nil,
            },
            "E-Nil",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "|- [] evalto [] by E-Nil {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalML4Judgment::EvalTo {
                env: EvalML4Env::default(),
                expr: EvalML4Expr::Cons {
                    head: Box::new(EvalML4Expr::BinOp {
                        op: EvalML4BinOp::Plus,
                        left: Box::new(EvalML4Expr::Int(1)),
                        right: Box::new(EvalML4Expr::Int(2)),
                    }),
                    tail: Box::new(EvalML4Expr::Nil),
                },
                value: EvalML4Value::Cons {
                    head: Box::new(EvalML4Value::Int(3)),
                    tail: Box::new(EvalML4Value::Nil),
                },
            },
            "E-Cons",
            vec![
                derivation(
                    EvalML4Judgment::EvalTo {
                        env: EvalML4Env::default(),
                        expr: EvalML4Expr::BinOp {
                            op: EvalML4BinOp::Plus,
                            left: Box::new(EvalML4Expr::Int(1)),
                            right: Box::new(EvalML4Expr::Int(2)),
                        },
                        value: EvalML4Value::Int(3),
                    },
                    "E-Plus",
                    vec![
                        derivation(
                            EvalML4Judgment::EvalTo {
                                env: EvalML4Env::default(),
                                expr: EvalML4Expr::Int(1),
                                value: EvalML4Value::Int(1),
                            },
                            "E-Int",
                            Vec::new(),
                        ),
                        derivation(
                            EvalML4Judgment::EvalTo {
                                env: EvalML4Env::default(),
                                expr: EvalML4Expr::Int(2),
                                value: EvalML4Value::Int(2),
                            },
                            "E-Int",
                            Vec::new(),
                        ),
                        derivation(
                            EvalML4Judgment::PlusIs {
                                left: 1,
                                right: 2,
                                result: 3,
                            },
                            "B-Plus",
                            Vec::new(),
                        ),
                    ],
                ),
                derivation(
                    EvalML4Judgment::EvalTo {
                        env: EvalML4Env::default(),
                        expr: EvalML4Expr::Nil,
                        value: EvalML4Value::Nil,
                    },
                    "E-Nil",
                    Vec::new(),
                ),
            ],
        );

        let expected = "\
|- 1 + 2 :: [] evalto 3 :: [] by E-Cons {
  |- 1 + 2 evalto 3 by E-Plus {
    |- 1 evalto 1 by E-Int {};
    |- 2 evalto 2 by E-Int {};
    1 plus 2 is 3 by B-Plus {}
  };
  |- [] evalto [] by E-Nil {}
}";
        assert_eq!(derivation.to_string(), expected);
        parse_source(&derivation.to_string()).expect("formatted derivation should parse");
    }
}
