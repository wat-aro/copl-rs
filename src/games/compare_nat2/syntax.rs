use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat2Term {
    Z,
    S(Box<CompareNat2Term>),
}

impl fmt::Display for CompareNat2Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat2Judgment {
    pub left: CompareNat2Term,
    pub right: CompareNat2Term,
}

impl fmt::Display for CompareNat2Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat2Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat2Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat2Derivation>,
}

impl fmt::Display for CompareNat2Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &CompareNat2Derivation,
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
    use super::{CompareNat2Derivation, CompareNat2Judgment, CompareNat2Term};
    use crate::core::SourceSpan;
    use crate::games::compare_nat2::parser::parse_source;

    fn z() -> CompareNat2Term {
        CompareNat2Term::Z
    }

    fn s(inner: CompareNat2Term) -> CompareNat2Term {
        CompareNat2Term::S(Box::new(inner))
    }

    fn derivation(
        judgment: CompareNat2Judgment,
        rule_name: &str,
        subderivations: Vec<CompareNat2Derivation>,
    ) -> CompareNat2Derivation {
        CompareNat2Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            CompareNat2Judgment {
                left: z(),
                right: s(s(z())),
            },
            "L-Zero",
            Vec::new(),
        );

        assert_eq!(
            derivation.to_string(),
            "Z is less than S(S(Z)) by L-Zero {}"
        );
    }

    #[test]
    fn formats_nested_derivation_in_checker_accepted_shape() {
        let derivation = derivation(
            CompareNat2Judgment {
                left: s(s(z())),
                right: s(s(s(s(s(z()))))),
            },
            "L-SuccSucc",
            vec![derivation(
                CompareNat2Judgment {
                    left: s(z()),
                    right: s(s(s(s(z())))),
                },
                "L-SuccSucc",
                vec![derivation(
                    CompareNat2Judgment {
                        left: z(),
                        right: s(s(s(z()))),
                    },
                    "L-Zero",
                    Vec::new(),
                )],
            )],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(S(Z)) is less than S(S(S(S(S(Z))))) by L-SuccSucc {
  S(Z) is less than S(S(S(S(Z)))) by L-SuccSucc {
    Z is less than S(S(S(Z))) by L-Zero {}
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
