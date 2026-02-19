use std::fmt;

use crate::core::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareNat3Term {
    Z,
    S(Box<CompareNat3Term>),
}

impl fmt::Display for CompareNat3Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Z => write!(f, "Z"),
            Self::S(inner) => write!(f, "S({inner})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat3Judgment {
    pub left: CompareNat3Term,
    pub right: CompareNat3Term,
}

impl fmt::Display for CompareNat3Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is less than {}", self.left, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareNat3Derivation {
    pub span: SourceSpan,
    pub judgment: CompareNat3Judgment,
    pub rule_name: String,
    pub subderivations: Vec<CompareNat3Derivation>,
}

impl fmt::Display for CompareNat3Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_derivation(self, f, 0)
    }
}

fn format_derivation(
    derivation: &CompareNat3Derivation,
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
    use super::{CompareNat3Derivation, CompareNat3Judgment, CompareNat3Term};
    use crate::core::SourceSpan;
    use crate::games::compare_nat3::parser::parse_source;

    fn z() -> CompareNat3Term {
        CompareNat3Term::Z
    }

    fn s(inner: CompareNat3Term) -> CompareNat3Term {
        CompareNat3Term::S(Box::new(inner))
    }

    fn derivation(
        judgment: CompareNat3Judgment,
        rule_name: &str,
        subderivations: Vec<CompareNat3Derivation>,
    ) -> CompareNat3Derivation {
        CompareNat3Derivation {
            span: SourceSpan { line: 1, column: 1 },
            judgment,
            rule_name: rule_name.to_string(),
            subderivations,
        }
    }

    #[test]
    fn formats_leaf_derivation() {
        let derivation = derivation(
            CompareNat3Judgment {
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
            CompareNat3Judgment {
                left: s(s(z())),
                right: s(s(s(s(s(z()))))),
            },
            "L-SuccR",
            vec![derivation(
                CompareNat3Judgment {
                    left: s(s(z())),
                    right: s(s(s(s(z())))),
                },
                "L-SuccR",
                vec![derivation(
                    CompareNat3Judgment {
                        left: s(s(z())),
                        right: s(s(s(z()))),
                    },
                    "L-Succ",
                    Vec::new(),
                )],
            )],
        );

        let rendered = derivation.to_string();
        let expected = "\
S(S(Z)) is less than S(S(S(S(S(Z))))) by L-SuccR {
  S(S(Z)) is less than S(S(S(S(Z)))) by L-SuccR {
    S(S(Z)) is less than S(S(S(Z))) by L-Succ {}
  }
}";
        assert_eq!(rendered, expected);
        assert!(parse_source(&rendered).is_ok());
    }
}
