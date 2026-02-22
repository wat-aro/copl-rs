use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GameKind {
    Nat,
    CompareNat1,
    CompareNat2,
    CompareNat3,
    EvalML1,
    EvalML1Err,
    EvalML2,
    EvalML3,
    EvalML4,
    EvalML5,
    EvalML6,
    EvalContML1,
    EvalContML4,
    EvalRefML3,
    TypingML2,
    TypingML3,
    TypingML4,
    TypingML5,
    TypingML6,
    PolyTypingML3,
    PolyTypingML4,
    NamelessML3,
    EvalNamelessML3,
    EvalNatExp,
    ReduceNatExp,
    While,
}

impl GameKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Nat => "Nat",
            Self::CompareNat1 => "CompareNat1",
            Self::CompareNat2 => "CompareNat2",
            Self::CompareNat3 => "CompareNat3",
            Self::EvalML1 => "EvalML1",
            Self::EvalML1Err => "EvalML1Err",
            Self::EvalML2 => "EvalML2",
            Self::EvalML3 => "EvalML3",
            Self::EvalML4 => "EvalML4",
            Self::EvalML5 => "EvalML5",
            Self::EvalML6 => "EvalML6",
            Self::EvalContML1 => "EvalContML1",
            Self::EvalContML4 => "EvalContML4",
            Self::EvalRefML3 => "EvalRefML3",
            Self::TypingML2 => "TypingML2",
            Self::TypingML3 => "TypingML3",
            Self::TypingML4 => "TypingML4",
            Self::TypingML5 => "TypingML5",
            Self::TypingML6 => "TypingML6",
            Self::PolyTypingML3 => "PolyTypingML3",
            Self::PolyTypingML4 => "PolyTypingML4",
            Self::NamelessML3 => "NamelessML3",
            Self::EvalNamelessML3 => "EvalNamelessML3",
            Self::EvalNatExp => "EvalNatExp",
            Self::ReduceNatExp => "ReduceNatExp",
            Self::While => "While",
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
        } else if value.eq_ignore_ascii_case("EvalML1") {
            Ok(Self::EvalML1)
        } else if value.eq_ignore_ascii_case("EvalML1Err") {
            Ok(Self::EvalML1Err)
        } else if value.eq_ignore_ascii_case("EvalML2") {
            Ok(Self::EvalML2)
        } else if value.eq_ignore_ascii_case("EvalML3") {
            Ok(Self::EvalML3)
        } else if value.eq_ignore_ascii_case("EvalML4") {
            Ok(Self::EvalML4)
        } else if value.eq_ignore_ascii_case("EvalML5") {
            Ok(Self::EvalML5)
        } else if value.eq_ignore_ascii_case("EvalML6") {
            Ok(Self::EvalML6)
        } else if value.eq_ignore_ascii_case("EvalContML1") {
            Ok(Self::EvalContML1)
        } else if value.eq_ignore_ascii_case("EvalContML4") {
            Ok(Self::EvalContML4)
        } else if value.eq_ignore_ascii_case("EvalRefML3") {
            Ok(Self::EvalRefML3)
        } else if value.eq_ignore_ascii_case("TypingML2") {
            Ok(Self::TypingML2)
        } else if value.eq_ignore_ascii_case("TypingML3") {
            Ok(Self::TypingML3)
        } else if value.eq_ignore_ascii_case("TypingML4") {
            Ok(Self::TypingML4)
        } else if value.eq_ignore_ascii_case("TypingML5") {
            Ok(Self::TypingML5)
        } else if value.eq_ignore_ascii_case("TypingML6") {
            Ok(Self::TypingML6)
        } else if value.eq_ignore_ascii_case("PolyTypingML3") {
            Ok(Self::PolyTypingML3)
        } else if value.eq_ignore_ascii_case("PolyTypingML4") {
            Ok(Self::PolyTypingML4)
        } else if value.eq_ignore_ascii_case("NamelessML3") {
            Ok(Self::NamelessML3)
        } else if value.eq_ignore_ascii_case("EvalNamelessML3") {
            Ok(Self::EvalNamelessML3)
        } else if value.eq_ignore_ascii_case("EvalNatExp") {
            Ok(Self::EvalNatExp)
        } else if value.eq_ignore_ascii_case("ReduceNatExp") {
            Ok(Self::ReduceNatExp)
        } else if value.eq_ignore_ascii_case("While") {
            Ok(Self::While)
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

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
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

pub fn annotate_rule_violation_with_premise_path<T, FSpan, FChildren>(
    err: CheckError,
    root: &T,
    span_of: FSpan,
    children_of: FChildren,
) -> CheckError
where
    FSpan: Fn(&T) -> &SourceSpan + Copy,
    FChildren: Fn(&T) -> &[T] + Copy,
{
    if err.kind() != CheckErrorKind::RuleViolation {
        return err;
    }

    let Some(span) = err.span() else {
        return err;
    };
    let Some(path) = find_premise_path(root, span, span_of, children_of) else {
        return err;
    };

    let message = err.message().to_string();
    err.with_message(format!(
        "{message} (premise path: {})",
        format_premise_path(&path)
    ))
}

fn find_premise_path<T, FSpan, FChildren>(
    node: &T,
    target: &SourceSpan,
    span_of: FSpan,
    children_of: FChildren,
) -> Option<Vec<usize>>
where
    FSpan: Fn(&T) -> &SourceSpan + Copy,
    FChildren: Fn(&T) -> &[T] + Copy,
{
    if span_of(node) == target {
        return Some(Vec::new());
    }

    for (index, child) in children_of(node).iter().enumerate() {
        if let Some(mut path) = find_premise_path(child, target, span_of, children_of) {
            path.insert(0, index + 1);
            return Some(path);
        }
    }

    None
}

fn format_premise_path(path: &[usize]) -> String {
    if path.is_empty() {
        "root".to_string()
    } else {
        path.iter()
            .map(|index| index.to_string())
            .collect::<Vec<_>>()
            .join(".")
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
    fn parses_eval_ml1_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML1").expect("EvalML1 should parse");
        assert_eq!(canonical.as_str(), "EvalML1");

        let lowercase = GameKind::try_from("evalml1").expect("evalml1 should parse");
        assert_eq!(lowercase.as_str(), "EvalML1");
    }

    #[test]
    fn parses_eval_ml1_err_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML1Err").expect("EvalML1Err should parse");
        assert_eq!(canonical.as_str(), "EvalML1Err");

        let lowercase = GameKind::try_from("evalml1err").expect("evalml1err should parse");
        assert_eq!(lowercase.as_str(), "EvalML1Err");
    }

    #[test]
    fn parses_eval_ml2_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML2").expect("EvalML2 should parse");
        assert_eq!(canonical.as_str(), "EvalML2");

        let lowercase = GameKind::try_from("evalml2").expect("evalml2 should parse");
        assert_eq!(lowercase.as_str(), "EvalML2");
    }

    #[test]
    fn parses_eval_ml3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML3").expect("EvalML3 should parse");
        assert_eq!(canonical.as_str(), "EvalML3");

        let lowercase = GameKind::try_from("evalml3").expect("evalml3 should parse");
        assert_eq!(lowercase.as_str(), "EvalML3");
    }

    #[test]
    fn parses_eval_ml4_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML4").expect("EvalML4 should parse");
        assert_eq!(canonical.as_str(), "EvalML4");

        let lowercase = GameKind::try_from("evalml4").expect("evalml4 should parse");
        assert_eq!(lowercase.as_str(), "EvalML4");
    }

    #[test]
    fn parses_eval_ml5_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML5").expect("EvalML5 should parse");
        assert_eq!(canonical.as_str(), "EvalML5");

        let lowercase = GameKind::try_from("evalml5").expect("evalml5 should parse");
        assert_eq!(lowercase.as_str(), "EvalML5");
    }

    #[test]
    fn parses_eval_ml6_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalML6").expect("EvalML6 should parse");
        assert_eq!(canonical.as_str(), "EvalML6");

        let lowercase = GameKind::try_from("evalml6").expect("evalml6 should parse");
        assert_eq!(lowercase.as_str(), "EvalML6");
    }

    #[test]
    fn parses_eval_cont_ml1_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalContML1").expect("EvalContML1 should parse");
        assert_eq!(canonical.as_str(), "EvalContML1");

        let lowercase = GameKind::try_from("evalcontml1").expect("evalcontml1 should parse");
        assert_eq!(lowercase.as_str(), "EvalContML1");
    }

    #[test]
    fn parses_eval_cont_ml4_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalContML4").expect("EvalContML4 should parse");
        assert_eq!(canonical.as_str(), "EvalContML4");

        let lowercase = GameKind::try_from("evalcontml4").expect("evalcontml4 should parse");
        assert_eq!(lowercase.as_str(), "EvalContML4");
    }

    #[test]
    fn parses_eval_ref_ml3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalRefML3").expect("EvalRefML3 should parse");
        assert_eq!(canonical.as_str(), "EvalRefML3");

        let lowercase = GameKind::try_from("evalrefml3").expect("evalrefml3 should parse");
        assert_eq!(lowercase.as_str(), "EvalRefML3");
    }

    #[test]
    fn parses_typing_ml2_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("TypingML2").expect("TypingML2 should parse");
        assert_eq!(canonical.as_str(), "TypingML2");

        let lowercase = GameKind::try_from("typingml2").expect("typingml2 should parse");
        assert_eq!(lowercase.as_str(), "TypingML2");
    }

    #[test]
    fn parses_typing_ml3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("TypingML3").expect("TypingML3 should parse");
        assert_eq!(canonical.as_str(), "TypingML3");

        let lowercase = GameKind::try_from("typingml3").expect("typingml3 should parse");
        assert_eq!(lowercase.as_str(), "TypingML3");
    }

    #[test]
    fn parses_typing_ml4_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("TypingML4").expect("TypingML4 should parse");
        assert_eq!(canonical.as_str(), "TypingML4");

        let lowercase = GameKind::try_from("typingml4").expect("typingml4 should parse");
        assert_eq!(lowercase.as_str(), "TypingML4");
    }

    #[test]
    fn parses_typing_ml5_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("TypingML5").expect("TypingML5 should parse");
        assert_eq!(canonical.as_str(), "TypingML5");

        let lowercase = GameKind::try_from("typingml5").expect("typingml5 should parse");
        assert_eq!(lowercase.as_str(), "TypingML5");
    }

    #[test]
    fn parses_typing_ml6_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("TypingML6").expect("TypingML6 should parse");
        assert_eq!(canonical.as_str(), "TypingML6");

        let lowercase = GameKind::try_from("typingml6").expect("typingml6 should parse");
        assert_eq!(lowercase.as_str(), "TypingML6");
    }

    #[test]
    fn parses_poly_typing_ml3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("PolyTypingML3").expect("PolyTypingML3 should parse");
        assert_eq!(canonical.as_str(), "PolyTypingML3");

        let lowercase = GameKind::try_from("polytypingml3").expect("polytypingml3 should parse");
        assert_eq!(lowercase.as_str(), "PolyTypingML3");
    }

    #[test]
    fn parses_poly_typing_ml4_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("PolyTypingML4").expect("PolyTypingML4 should parse");
        assert_eq!(canonical.as_str(), "PolyTypingML4");

        let lowercase = GameKind::try_from("polytypingml4").expect("polytypingml4 should parse");
        assert_eq!(lowercase.as_str(), "PolyTypingML4");
    }

    #[test]
    fn parses_nameless_ml3_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("NamelessML3").expect("NamelessML3 should parse");
        assert_eq!(canonical.as_str(), "NamelessML3");

        let lowercase = GameKind::try_from("namelessml3").expect("namelessml3 should parse");
        assert_eq!(lowercase.as_str(), "NamelessML3");
    }

    #[test]
    fn parses_eval_nameless_ml3_game_kind_case_insensitively() {
        let canonical =
            GameKind::try_from("EvalNamelessML3").expect("EvalNamelessML3 should parse");
        assert_eq!(canonical.as_str(), "EvalNamelessML3");

        let lowercase =
            GameKind::try_from("evalnamelessml3").expect("evalnamelessml3 should parse");
        assert_eq!(lowercase.as_str(), "EvalNamelessML3");
    }

    #[test]
    fn parses_eval_nat_exp_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("EvalNatExp").expect("EvalNatExp should parse");
        assert_eq!(canonical.as_str(), "EvalNatExp");

        let lowercase = GameKind::try_from("evalnatexp").expect("evalnatexp should parse");
        assert_eq!(lowercase.as_str(), "EvalNatExp");
    }

    #[test]
    fn parses_reduce_nat_exp_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("ReduceNatExp").expect("ReduceNatExp should parse");
        assert_eq!(canonical.as_str(), "ReduceNatExp");

        let lowercase = GameKind::try_from("reducenatexp").expect("reducenatexp should parse");
        assert_eq!(lowercase.as_str(), "ReduceNatExp");
    }

    #[test]
    fn parses_while_game_kind_case_insensitively() {
        let canonical = GameKind::try_from("While").expect("While should parse");
        assert_eq!(canonical.as_str(), "While");

        let lowercase = GameKind::try_from("while").expect("while should parse");
        assert_eq!(lowercase.as_str(), "While");
    }
}
