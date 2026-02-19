use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat1Term {
    Z,
    S(Box<CompareNat1Term>),
}

impl fmt::Display for CompareNat1Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat1Judgment {
    pub left: CompareNat1Term,
    pub right: CompareNat1Term,
}

impl fmt::Display for CompareNat1Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat1Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat1Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat1Derivation>,
}

impl fmt::Display for CompareNat1Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &CompareNat1Derivation,
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
    use super::{CompareNat1Derivation, CompareNat1Judgment, CompareNat1Term};
    use crate::core::SourceSpan;
    use crate::games::compare_nat1::parser::parse_source;

    fn z() -> CompareNat1Term {
        CompareNat1Term::Z
    }

    fn s(inner: CompareNat1Term) -> CompareNat1Term {
        CompareNat1Term::S(Box::new(inner))
    }

    fn derivation(
        judgment: CompareNat1Judgment,
        rule_name: &str,
        subderivations: Vec<CompareNat1Derivation>,
    ) -> CompareNat1Derivation {
        CompareNat1Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            CompareNat1Judgment {
                left: s(s(z())),
                right: s(s(s(z()))),
            },
            "L-Succ",
            Vec::new(),
        );

        assert_eq!(
            derivation.to_string(),
            "S(S(Z)) is less than S(S(S(Z))) by L-Succ {}"
        );
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            CompareNat1Judgment {
                left: s(s(z())),
                right: s(s(s(s(s(z()))))),
            },
            "L-Trans",
            vec![
                derivation(
                    CompareNat1Judgment {
                        left: s(s(z())),
                        right: s(s(s(z()))),
                    },
                    "L-Succ",
                    Vec::new(),
                ),
                derivation(
                    CompareNat1Judgment {
                        left: s(s(s(z()))),
                        right: s(s(s(s(s(z()))))),
                    },
                    "L-Trans",
                    vec![
                        derivation(
                            CompareNat1Judgment {
                                left: s(s(s(z()))),
                                right: s(s(s(s(z())))),
                            },
                            "L-Succ",
                            Vec::new(),
                        ),
                        derivation(
                            CompareNat1Judgment {
                                left: s(s(s(s(z())))),
                                right: s(s(s(s(s(z()))))),
                            },
                            "L-Succ",
                            Vec::new(),
                        ),
                    ],
                ),
            ],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(S(Z)) is less than S(S(S(S(S(Z))))) by L-Trans {
  S(S(Z)) is less than S(S(S(Z))) by L-Succ {};
  S(S(S(Z))) is less than S(S(S(S(S(Z))))) by L-Trans {
    S(S(S(Z))) is less than S(S(S(S(Z)))) by L-Succ {};
    S(S(S(S(Z)))) is less than S(S(S(S(S(Z))))) by L-Succ {}
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
