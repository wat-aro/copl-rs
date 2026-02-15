use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GameKind {
    Nat,
    CompareNat1,
    CompareNat2,
    CompareNat3,
    EvalNatExp,
}

impl GameKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Nat => "Nat",
            Self::CompareNat1 => "CompareNat1",
            Self::CompareNat2 => "CompareNat2",
            Self::CompareNat3 => "CompareNat3",
            Self::EvalNatExp => "EvalNatExp",
        }
    }
}

impl TryFrom<&str> for GameKind {
    type Error = ParseGameKindError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.eq_ignore_ascii_case("Nat") {
            Ok(Self::Nat)
        } else if value.eq_ignore_ascii_case("CompareNat1") {
            Ok(Self::CompareNat1)
        } else if value.eq_ignore_ascii_case("CompareNat2") {
            Ok(Self::CompareNat2)
        } else if value.eq_ignore_ascii_case("CompareNat3") {
            Ok(Self::CompareNat3)
        } else if value.eq_ignore_ascii_case("EvalNatExp") {
            Ok(Self::EvalNatExp)
        } else {
            Err(ParseGameKindError {
                raw: value.to_string(),
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseGameKindError {
    raw: String,
}

impl fmt::Display for ParseGameKindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown game: {}", self.raw)
    }
}

impl Error for ParseGameKindError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckReport {
    pub game: GameKind,
    pub summary: String,
}

impl fmt::Display for CheckReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.summary)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckErrorKind {
    Parse,
    RuleViolation,
    Internal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckError {
    kind: CheckErrorKind,
    message: String,
    span: Option<SourceSpan>,
}

impl CheckError {
    pub fn parse(message: impl Into<String>) -> Self {
        Self {
            kind: CheckErrorKind::Parse,
            message: message.into(),
            span: None,
        }
    }

    pub fn rule_violation(message: impl Into<String>) -> Self {
        Self {
            kind: CheckErrorKind::RuleViolation,
            message: message.into(),
            span: None,
        }
    }

    pub fn internal(message: impl Into<String>) -> Self {
        Self {
            kind: CheckErrorKind::Internal,
            message: message.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn kind(&self) -> CheckErrorKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<&SourceSpan> {
        self.span.as_ref()
    }
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = &self.span {
            write!(f, "{} at {}", self.message, span)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl Error for CheckError {}

pub trait Game {
    fn kind(&self) -> GameKind;
    fn check(&self, source: &str) -> Result<CheckReport, CheckError>;
}

#[cfg(test)]
mod tests {
    use super::GameKind;

    #[test]
    fn parses_compare_nat3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("CompareNat3").expect("CompareNat3 should parse");
        assert_eq!(canonical.as_str(), "CompareNat3");

        let lowercase = GameKind::try_from("comparenat3").expect("comparenat3 should parse");
        assert_eq!(lowercase.as_str(), "CompareNat3");
    }

    #[test]
    fn parses_eval_nat_exp_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalNatExp").expect("EvalNatExp should parse");
        assert_eq!(canonical.as_str(), "EvalNatExp");

        let lowercase = GameKind::try_from("evalnatexp").expect("evalnatexp should parse");
        assert_eq!(lowercase.as_str(), "EvalNatExp");
    }
}
