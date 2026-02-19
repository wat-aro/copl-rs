use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatTerm {
    Z,
    S(Box<NatTerm>),
}

impl fmt::Display for NatTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalNatExpExpr {
    Nat(NatTerm),
    Plus(Box<EvalNatExpExpr>, Box<EvalNatExpExpr>),
    Times(Box<EvalNatExpExpr>, Box<EvalNatExpExpr>),
}

impl EvalNatExpExpr {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Nat(_) => 3,
            Self::Times(_, _) => 2,
            Self::Plus(_, _) => 1,
        }
    }

    fn fmt_with_precedence(&self, f: &mut fmt::Formatter<'_>, parent: u8) -> fmt::Result {
        let needs_paren = self.precedence() < parent;
        if needs_paren {
            write!(f, "(")?;
        }

        match self {
            Self::Nat(term) => write!(f, "{term}")?,
            Self::Plus(left, right) => {
                left.fmt_with_precedence(f, self.precedence())?;
                write!(f, " + ")?;
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

impl fmt::Display for EvalNatExpExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalNatExpJudgment {
    EvalTo {
        expr: EvalNatExpExpr,
        value: NatTerm,
    },
    PlusIs {
        left: NatTerm,
        right: NatTerm,
        result: NatTerm,
    },
    TimesIs {
        left: NatTerm,
        right: NatTerm,
        result: NatTerm,
    },
}

impl fmt::Display for EvalNatExpJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalTo { expr, value } => write!(f, "{expr} evalto {value}"),
            Self::PlusIs {
                left,
                right,
                result,
            } => write!(f, "{left} plus {right} is {result}"),
            Self::TimesIs {
                left,
                right,
                result,
            } => write!(f, "{left} times {right} is {result}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalNatExpDerivation {
    pub span: SourceSpan,
    pub judgment: EvalNatExpJudgment,
    pub rule_name: String,
    pub subderivations: Vec<EvalNatExpDerivation>,
}

impl fmt::Display for EvalNatExpDerivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &EvalNatExpDerivation,
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
    use super::{EvalNatExpDerivation, EvalNatExpExpr, EvalNatExpJudgment, NatTerm};
    use crate::core::SourceSpan;
    use crate::games::eval_nat_exp::parser::parse_source;

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    fn derivation(
        judgment: EvalNatExpJudgment,
        rule_name: &str,
        subderivations: Vec<EvalNatExpDerivation>,
    ) -> EvalNatExpDerivation {
        EvalNatExpDerivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Nat(s(z())),
                value: s(z()),
            },
            "E-Const",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "S(Z) evalto S(Z) by E-Const {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Plus(
                    Box::new(EvalNatExpExpr::Nat(s(z()))),
                    Box::new(EvalNatExpExpr::Nat(s(z()))),
                ),
                value: s(s(z())),
            },
            "E-Plus",
            vec![
                derivation(
                    EvalNatExpJudgment::EvalTo {
                        expr: EvalNatExpExpr::Nat(s(z())),
                        value: s(z()),
                    },
                    "E-Const",
                    Vec::new(),
                ),
                derivation(
                    EvalNatExpJudgment::EvalTo {
                        expr: EvalNatExpExpr::Nat(s(z())),
                        value: s(z()),
                    },
                    "E-Const",
                    Vec::new(),
                ),
                derivation(
                    EvalNatExpJudgment::PlusIs {
                        left: s(z()),
                        right: s(z()),
                        result: s(s(z())),
                    },
                    "P-Succ",
                    vec![derivation(
                        EvalNatExpJudgment::PlusIs {
                            left: z(),
                            right: s(z()),
                            result: s(z()),
                        },
                        "P-Zero",
                        Vec::new(),
                    )],
                ),
            ],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(Z) + S(Z) evalto S(S(Z)) by E-Plus {
  S(Z) evalto S(Z) by E-Const {};
  S(Z) evalto S(Z) by E-Const {};
  S(Z) plus S(Z) is S(S(Z)) by P-Succ {
    Z plus S(Z) is S(Z) by P-Zero {}
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }

    #[test]
    fn keeps_parentheses_for_right_nested_same_precedence_expression() {
        let right_nested_plus = derivation(
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Plus(
                    Box::new(EvalNatExpExpr::Nat(z())),
                    Box::new(EvalNatExpExpr::Plus(
                        Box::new(EvalNatExpExpr::Nat(s(z()))),
                        Box::new(EvalNatExpExpr::Nat(z())),
                    )),
                ),
                value: s(z()),
            },
            "E-Plus",
            Vec::new(),
        );
        assert_eq!(
            right_nested_plus.to_string(),
            "Z + (S(Z) + Z) evalto S(Z) by E-Plus {}"
        );

        let right_nested_times = derivation(
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Times(
                    Box::new(EvalNatExpExpr::Nat(s(z()))),
                    Box::new(EvalNatExpExpr::Times(
                        Box::new(EvalNatExpExpr::Nat(s(z()))),
                        Box::new(EvalNatExpExpr::Nat(s(z()))),
                    )),
                ),
                value: s(s(z())),
            },
            "E-Times",
            Vec::new(),
        );
        assert_eq!(
            right_nested_times.to_string(),
            "S(Z) * (S(Z) * S(Z)) evalto S(S(Z)) by E-Times {}"
        );
    }
}
