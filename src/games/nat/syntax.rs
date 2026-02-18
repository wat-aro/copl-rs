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
pub enum NatJudgment {
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

impl fmt::Display for NatJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
pub struct NatDerivation {
    pub span: SourceSpan,
    pub judgment: NatJudgment,
    pub rule_name: String,
    pub subderivations: Vec<NatDerivation>,
}

impl fmt::Display for NatDerivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &NatDerivation,
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
    use super::{NatDerivation, NatJudgment, NatTerm};
    use crate::core::SourceSpan;
    use crate::games::nat::parser::parse_source;

    fn z() -> NatTerm {
        NatTerm::Z
    }

    fn s(inner: NatTerm) -> NatTerm {
        NatTerm::S(Box::new(inner))
    }

    fn derivation(
        judgment: NatJudgment,
        rule_name: &str,
        subderivations: Vec<NatDerivation>,
    ) -> NatDerivation {
        NatDerivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            NatJudgment::PlusIs {
                left: z(),
                right: s(z()),
                result: s(z()),
            },
            "P-Zero",
            Vec::new(),
        );

        assert_eq!(derivation.to_string(), "Z plus S(Z) is S(Z) by P-Zero {}");
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            NatJudgment::TimesIs {
                left: s(z()),
                right: s(z()),
                result: s(z()),
            },
            "T-Succ",
            vec![
                derivation(
                    NatJudgment::TimesIs {
                        left: z(),
                        right: s(z()),
                        result: z(),
                    },
                    "T-Zero",
                    Vec::new(),
                ),
                derivation(
                    NatJudgment::PlusIs {
                        left: s(z()),
                        right: z(),
                        result: s(z()),
                    },
                    "P-Succ",
                    vec![derivation(
                        NatJudgment::PlusIs {
                            left: z(),
                            right: z(),
                            result: z(),
                        },
                        "P-Zero",
                        Vec::new(),
                    )],
                ),
            ],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(Z) times S(Z) is S(Z) by T-Succ {
  Z times S(Z) is Z by T-Zero {};
  S(Z) plus Z is S(Z) by P-Succ {
    Z plus Z is Z by P-Zero {}
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
