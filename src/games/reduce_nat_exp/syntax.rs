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
pub enum ReduceNatExpExpr {
    Nat(NatTerm),
    Plus(Box<ReduceNatExpExpr>, Box<ReduceNatExpExpr>),
    Times(Box<ReduceNatExpExpr>, Box<ReduceNatExpExpr>),
}

impl ReduceNatExpExpr {
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

impl fmt::Display for ReduceNatExpExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_precedence(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReduceNatExpJudgment {
    ReducesTo {
        from: ReduceNatExpExpr,
        to: ReduceNatExpExpr,
    },
    DeterministicReducesTo {
        from: ReduceNatExpExpr,
        to: ReduceNatExpExpr,
    },
    MultiReducesTo {
        from: ReduceNatExpExpr,
        to: ReduceNatExpExpr,
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

impl fmt::Display for ReduceNatExpJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReducesTo { from, to } => write!(f, "{from} ---> {to}"),
            Self::DeterministicReducesTo { from, to } => write!(f, "{from} -d-> {to}"),
            Self::MultiReducesTo { from, to } => write!(f, "{from} -*-> {to}"),
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
pub struct ReduceNatExpDerivation {
    pub span: SourceSpan,
    pub judgment: ReduceNatExpJudgment,
    pub rule_name: String,
    pub subderivations: Vec<ReduceNatExpDerivation>,
}

impl fmt::Display for ReduceNatExpDerivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &ReduceNatExpDerivation,
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
    use super::{NatTerm, ReduceNatExpDerivation, ReduceNatExpExpr, ReduceNatExpJudgment};
    use crate::core::SourceSpan;
    use crate::games::reduce_nat_exp::parser::parse_source;

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    fn nat_expr(term: NatTerm) -> ReduceNatExpExpr {
        ReduceNatExpExpr::Nat(term)
    }

    fn derivation(
        judgment: ReduceNatExpJudgment,
        rule_name: &str,
        subderivations: Vec<ReduceNatExpDerivation>,
    ) -> ReduceNatExpDerivation {
        ReduceNatExpDerivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            ReduceNatExpJudgment::ReducesTo {
                from: ReduceNatExpExpr::Plus(Box::new(nat_expr(z())), Box::new(nat_expr(s(z())))),
                to: nat_expr(s(z())),
            },
            "R-Plus",
            vec![derivation(
                ReduceNatExpJudgment::PlusIs {
                    left: z(),
                    right: s(z()),
                    result: s(z()),
                },
                "P-Zero",
                Vec::new(),
            )],
        );

        let expected = "\
Z + S(Z) ---> S(Z) by R-Plus {
  Z plus S(Z) is S(Z) by P-Zero {}
}";
        assert_eq!(derivation.to_string(), expected);
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            ReduceNatExpJudgment::MultiReducesTo {
                from: ReduceNatExpExpr::Plus(
                    Box::new(ReduceNatExpExpr::Times(
                        Box::new(nat_expr(s(z()))),
                        Box::new(nat_expr(s(z()))),
                    )),
                    Box::new(nat_expr(s(z()))),
                ),
                to: nat_expr(s(s(z()))),
            },
            "MR-Multi",
            vec![
                derivation(
                    ReduceNatExpJudgment::MultiReducesTo {
                        from: ReduceNatExpExpr::Plus(
                            Box::new(ReduceNatExpExpr::Times(
                                Box::new(nat_expr(s(z()))),
                                Box::new(nat_expr(s(z()))),
                            )),
                            Box::new(nat_expr(s(z()))),
                        ),
                        to: ReduceNatExpExpr::Plus(
                            Box::new(nat_expr(s(z()))),
                            Box::new(nat_expr(s(z()))),
                        ),
                    },
                    "MR-One",
                    vec![derivation(
                        ReduceNatExpJudgment::ReducesTo {
                            from: ReduceNatExpExpr::Plus(
                                Box::new(ReduceNatExpExpr::Times(
                                    Box::new(nat_expr(s(z()))),
                                    Box::new(nat_expr(s(z()))),
                                )),
                                Box::new(nat_expr(s(z()))),
                            ),
                            to: ReduceNatExpExpr::Plus(
                                Box::new(nat_expr(s(z()))),
                                Box::new(nat_expr(s(z()))),
                            ),
                        },
                        "R-PlusL",
                        vec![derivation(
                            ReduceNatExpJudgment::ReducesTo {
                                from: ReduceNatExpExpr::Times(
                                    Box::new(nat_expr(s(z()))),
                                    Box::new(nat_expr(s(z()))),
                                ),
                                to: nat_expr(s(z())),
                            },
                            "R-Times",
                            vec![derivation(
                                ReduceNatExpJudgment::TimesIs {
                                    left: s(z()),
                                    right: s(z()),
                                    result: s(z()),
                                },
                                "T-Succ",
                                vec![
                                    derivation(
                                        ReduceNatExpJudgment::TimesIs {
                                            left: z(),
                                            right: s(z()),
                                            result: z(),
                                        },
                                        "T-Zero",
                                        Vec::new(),
                                    ),
                                    derivation(
                                        ReduceNatExpJudgment::PlusIs {
                                            left: s(z()),
                                            right: z(),
                                            result: s(z()),
                                        },
                                        "P-Succ",
                                        vec![derivation(
                                            ReduceNatExpJudgment::PlusIs {
                                                left: z(),
                                                right: z(),
                                                result: z(),
                                            },
                                            "P-Zero",
                                            Vec::new(),
                                        )],
                                    ),
                                ],
                            )],
                        )],
                    )],
                ),
                derivation(
                    ReduceNatExpJudgment::MultiReducesTo {
                        from: ReduceNatExpExpr::Plus(
                            Box::new(nat_expr(s(z()))),
                            Box::new(nat_expr(s(z()))),
                        ),
                        to: nat_expr(s(s(z()))),
                    },
                    "MR-One",
                    vec![derivation(
                        ReduceNatExpJudgment::ReducesTo {
                            from: ReduceNatExpExpr::Plus(
                                Box::new(nat_expr(s(z()))),
                                Box::new(nat_expr(s(z()))),
                            ),
                            to: nat_expr(s(s(z()))),
                        },
                        "R-Plus",
                        vec![derivation(
                            ReduceNatExpJudgment::PlusIs {
                                left: s(z()),
                                right: s(z()),
                                result: s(s(z())),
                            },
                            "P-Succ",
                            vec![derivation(
                                ReduceNatExpJudgment::PlusIs {
                                    left: z(),
                                    right: s(z()),
                                    result: s(z()),
                                },
                                "P-Zero",
                                Vec::new(),
                            )],
                        )],
                    )],
                ),
            ],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(Z) * S(Z) + S(Z) -*-> S(S(Z)) by MR-Multi {
  S(Z) * S(Z) + S(Z) -*-> S(Z) + S(Z) by MR-One {
    S(Z) * S(Z) + S(Z) ---> S(Z) + S(Z) by R-PlusL {
      S(Z) * S(Z) ---> S(Z) by R-Times {
        S(Z) times S(Z) is S(Z) by T-Succ {
          Z times S(Z) is Z by T-Zero {};
          S(Z) plus Z is S(Z) by P-Succ {
            Z plus Z is Z by P-Zero {}
          }
        }
      }
    }
  };
  S(Z) + S(Z) -*-> S(S(Z)) by MR-One {
    S(Z) + S(Z) ---> S(S(Z)) by R-Plus {
      S(Z) plus S(Z) is S(S(Z)) by P-Succ {
        Z plus S(Z) is S(Z) by P-Zero {}
      }
    }
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }

    #[test]
    fn keeps_parentheses_for_right_nested_same_precedence_expression() {
        let right_nested_plus = derivation(
            ReduceNatExpJudgment::ReducesTo {
                from: ReduceNatExpExpr::Plus(
                    Box::new(nat_expr(z())),
                    Box::new(ReduceNatExpExpr::Plus(
                        Box::new(nat_expr(s(z()))),
                        Box::new(nat_expr(z())),
                    )),
                ),
                to: ReduceNatExpExpr::Plus(Box::new(nat_expr(z())), Box::new(nat_expr(s(z())))),
            },
            "R-PlusR",
            Vec::new(),
        );
        assert_eq!(
            right_nested_plus.to_string(),
            "Z + (S(Z) + Z) ---> Z + S(Z) by R-PlusR {}"
        );

        let right_nested_times = derivation(
            ReduceNatExpJudgment::DeterministicReducesTo {
                from: ReduceNatExpExpr::Times(
                    Box::new(nat_expr(s(z()))),
                    Box::new(ReduceNatExpExpr::Times(
                        Box::new(nat_expr(s(z()))),
                        Box::new(nat_expr(z())),
                    )),
                ),
                to: ReduceNatExpExpr::Times(Box::new(nat_expr(s(z()))), Box::new(nat_expr(z()))),
            },
            "DR-TimesR",
            Vec::new(),
        );
        assert_eq!(
            right_nested_times.to_string(),
            "S(Z) * (S(Z) * Z) -d-> S(Z) * Z by DR-TimesR {}"
        );
    }
}
