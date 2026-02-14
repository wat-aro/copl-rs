use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GameKind {
    Nat,
}

impl GameKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Nat => "nat",
        }
    }
}

impl TryFrom<&str> for GameKind {
    type Error = ParseGameKindError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_ascii_lowercase().as_str() {
            "nat" => Ok(Self::Nat),
            _ => Err(ParseGameKindError {
                raw: value.to_string(),
            }),
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
