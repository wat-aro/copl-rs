use std::error::Error;
use std::fmt;
use std::marker::PhantomData;
use std::path::PathBuf;

use crate::core::GameKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cli {
    pub command: Command,
}

impl Cli {
    pub fn parse<I, S>(args: I) -> Result<Self, CliError>
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut iter = args.into_iter().map(Into::into);
        let program = iter.next().unwrap_or_else(|| "copl-rs".to_string());
        let Some(subcommand) = iter.next() else {
            return Err(CliError::missing_subcommand(program));
        };

        match subcommand.as_str() {
            "checker" => parse_checker(program, iter.collect()),
            _ => Err(CliError::unknown_subcommand(program, subcommand)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Checker(CheckerCommand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckerCommand {
    pub game: GameKind,
    pub input: InputSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputSource {
    Stdin,
    File(PathBuf),
}

impl fmt::Display for InputSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stdin => write!(f, "stdin"),
            Self::File(path) => write!(f, "{}", path.display()),
        }
    }
}

fn parse_checker(program: String, args: Vec<String>) -> Result<Cli, CliError> {
    let state = args
        .iter()
        .enumerate()
        .try_fold(CheckerParseState::default(), |state, (index, token)| {
            state.consume(&program, &args, index, token)
        })?;
    state.finish(program)
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct CheckerStateCore {
    game: Option<GameKind>,
    input: Option<InputSource>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ModeOptions;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ModePositional;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ExpectAny;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ExpectGameValue;

#[derive(Debug, Clone, PartialEq, Eq)]
struct CheckerParser<M, E> {
    core: CheckerStateCore,
    _marker: PhantomData<(M, E)>,
}

impl CheckerParser<ModeOptions, ExpectAny> {
    fn default_state() -> Self {
        Self {
            core: CheckerStateCore::default(),
            _marker: PhantomData,
        }
    }

    fn consume(
        self,
        program: &str,
        args: &[String],
        index: usize,
        token: &str,
    ) -> Result<CheckerParseState, CliError> {
        match classify_options_event(token) {
            OptionsEvent::EndOfOptions => Ok(CheckerParseState::PositionalAny(CheckerParser::new(
                self.core,
            ))),
            OptionsEvent::GameOption => {
                if self.core.game.is_some() {
                    return Err(CliError::duplicate_option(
                        program.to_owned(),
                        "--game".to_string(),
                    ));
                }
                Ok(CheckerParseState::OptionsGameValue(CheckerParser::new(
                    self.core,
                )))
            }
            OptionsEvent::GameInlineValue(value) => {
                if self.core.game.is_some() {
                    return Err(CliError::duplicate_option(
                        program.to_owned(),
                        "--game".to_string(),
                    ));
                }
                let game = parse_game_kind(program, value)?;
                Ok(CheckerParseState::OptionsAny(CheckerParser::new(
                    self.core.with_game(game),
                )))
            }
            OptionsEvent::UnexpectedOption(option) => Err(CliError::unexpected_option(
                program.to_owned(),
                option.to_string(),
            )),
            OptionsEvent::PositionalInput(path) => Ok(CheckerParseState::OptionsAny(
                CheckerParser::new(self.core.with_input(program, args, index, path)?),
            )),
        }
    }
}

impl<M> CheckerParser<M, ExpectAny> {
    fn finish(self, program: String) -> Result<Cli, CliError> {
        self.core.finish(program)
    }
}

impl CheckerParser<ModeOptions, ExpectGameValue> {
    fn consume(self, program: &str, token: &str) -> Result<CheckerParseState, CliError> {
        let game = parse_game_kind(program, token)?;
        Ok(CheckerParseState::OptionsAny(CheckerParser::new(
            self.core.with_game(game),
        )))
    }
}

impl CheckerParser<ModePositional, ExpectAny> {
    fn consume(
        self,
        program: &str,
        args: &[String],
        index: usize,
        token: &str,
    ) -> Result<CheckerParseState, CliError> {
        let core = self.core.with_input(program, args, index, token)?;
        Ok(CheckerParseState::PositionalAny(CheckerParser::new(core)))
    }
}

impl<M, E> CheckerParser<M, E> {
    fn new(core: CheckerStateCore) -> Self {
        Self {
            core,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CheckerParseState {
    OptionsAny(CheckerParser<ModeOptions, ExpectAny>),
    OptionsGameValue(CheckerParser<ModeOptions, ExpectGameValue>),
    PositionalAny(CheckerParser<ModePositional, ExpectAny>),
}

impl Default for CheckerParseState {
    fn default() -> Self {
        Self::OptionsAny(CheckerParser::default_state())
    }
}

impl CheckerParseState {
    fn consume(
        self,
        program: &str,
        args: &[String],
        index: usize,
        token: &str,
    ) -> Result<Self, CliError> {
        match self {
            Self::OptionsAny(state) => state.consume(program, args, index, token),
            Self::OptionsGameValue(state) => state.consume(program, token),
            Self::PositionalAny(state) => state.consume(program, args, index, token),
        }
    }

    fn finish(self, program: String) -> Result<Cli, CliError> {
        match self {
            Self::OptionsAny(state) => state.finish(program),
            Self::PositionalAny(state) => state.finish(program),
            Self::OptionsGameValue(_) => Err(CliError::missing_game_value(program)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OptionsEvent<'a> {
    EndOfOptions,
    GameOption,
    GameInlineValue(&'a str),
    UnexpectedOption(&'a str),
    PositionalInput(&'a str),
}

impl CheckerStateCore {
    fn with_game(self, game: GameKind) -> Self {
        Self {
            game: Some(game),
            ..self
        }
    }

    fn with_input(
        self,
        program: &str,
        args: &[String],
        index: usize,
        path: &str,
    ) -> Result<Self, CliError> {
        if self.input.is_some() {
            return Err(CliError::too_many_inputs(
                program.to_owned(),
                args[index..].to_vec(),
            ));
        }
        Ok(Self {
            input: Some(InputSource::File(PathBuf::from(path))),
            ..self
        })
    }

    fn finish(self, program: String) -> Result<Cli, CliError> {
        let game = self
            .game
            .ok_or_else(|| CliError::missing_game(program.clone()))?;
        Ok(Cli {
            command: Command::Checker(CheckerCommand {
                game,
                input: self.input.unwrap_or(InputSource::Stdin),
            }),
        })
    }
}

fn parse_game_kind(program: &str, token: &str) -> Result<GameKind, CliError> {
    GameKind::try_from(token).map_err(|e| CliError::invalid_game(program.to_owned(), e))
}

fn classify_options_event(token: &str) -> OptionsEvent<'_> {
    match token {
        "--" => OptionsEvent::EndOfOptions,
        "--game" => OptionsEvent::GameOption,
        _ => {
            if let Some(value) = token.strip_prefix("--game=") {
                OptionsEvent::GameInlineValue(value)
            } else if token.starts_with('-') {
                OptionsEvent::UnexpectedOption(token)
            } else {
                OptionsEvent::PositionalInput(token)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CliError {
    kind: CliErrorKind,
    usage: String,
}

impl CliError {
    fn missing_subcommand(program: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::MissingSubcommand,
        }
    }

    fn unknown_subcommand(program: String, subcommand: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::UnknownSubcommand { subcommand },
        }
    }

    fn missing_game(program: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::MissingGame,
        }
    }

    fn missing_game_value(program: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::MissingGameValue,
        }
    }

    fn invalid_game(program: String, source: crate::core::ParseGameKindError) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::InvalidGame { source },
        }
    }

    fn unexpected_option(program: String, option: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::UnexpectedOption { option },
        }
    }

    fn duplicate_option(program: String, option: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::DuplicateOption { option },
        }
    }

    fn too_many_inputs(program: String, inputs: Vec<String>) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::TooManyInputs { inputs },
        }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CliErrorKind::MissingSubcommand => {
                write!(f, "missing subcommand\n{}", self.usage)
            }
            CliErrorKind::UnknownSubcommand { subcommand } => {
                write!(f, "unknown subcommand: {subcommand}\n{}", self.usage)
            }
            CliErrorKind::MissingGame => {
                write!(f, "missing required option: --game <name>\n{}", self.usage)
            }
            CliErrorKind::MissingGameValue => {
                write!(f, "missing value for --game\n{}", self.usage)
            }
            CliErrorKind::InvalidGame { source } => {
                write!(f, "{source}\n{}", self.usage)
            }
            CliErrorKind::UnexpectedOption { option } => {
                write!(f, "unexpected option: {option}\n{}", self.usage)
            }
            CliErrorKind::DuplicateOption { option } => {
                write!(f, "duplicate option: {option}\n{}", self.usage)
            }
            CliErrorKind::TooManyInputs { inputs } => {
                write!(
                    f,
                    "too many input files: {}\n{}",
                    inputs.join(" "),
                    self.usage
                )
            }
        }
    }
}

impl Error for CliError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            CliErrorKind::InvalidGame { source } => Some(source),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CliErrorKind {
    MissingSubcommand,
    UnknownSubcommand {
        subcommand: String,
    },
    MissingGame,
    MissingGameValue,
    InvalidGame {
        source: crate::core::ParseGameKindError,
    },
    UnexpectedOption {
        option: String,
    },
    DuplicateOption {
        option: String,
    },
    TooManyInputs {
        inputs: Vec<String>,
    },
}

fn usage_for(program: &str) -> String {
    format!("Usage: {program} checker --game <name> [file]")
}

#[cfg(test)]
mod tests {
    use super::{Cli, Command, InputSource};
    use crate::core::GameKind;

    #[test]
    fn parses_checker_with_stdin() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "nat"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_nat() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "Nat"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::CompareNat1);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::CompareNat1);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::CompareNat2);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::CompareNat2);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "CompareNat3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "CompareNat3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml1() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML1"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml1() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml1"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml1_err() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML1Err"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML1Err");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml1_err() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalml1err"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML1Err");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml2() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML2"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml2() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml2"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml3() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML3"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml3() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml3"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml4() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML4"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml4() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml4"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml5() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML5"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML5");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml5() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml5"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalML5");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "NamelessML3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "NamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "namelessml3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "NamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalNamelessML3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalNamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalnamelessml3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalNamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalNatExp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalnatexp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "EvalNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_reduce_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "ReduceNatExp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "ReduceNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_reduce_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "reducenatexp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game.as_str(), "ReduceNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_file() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game=nat", "001.copl"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::File("001.copl".into()));
    }

    #[test]
    fn rejects_unknown_game() {
        let err = Cli::parse(vec!["copl-rs", "checker", "--game", "foo"])
            .expect_err("cli should reject unknown game");
        let message = err.to_string();
        assert!(message.contains("unknown game: foo"));
    }

    #[test]
    fn rejects_missing_game_value() {
        let err = Cli::parse(vec!["copl-rs", "checker", "--game"]).expect_err("cli should reject");
        let message = err.to_string();
        assert!(message.contains("missing value for --game"));
    }

    #[test]
    fn rejects_duplicate_game_option() {
        let err = Cli::parse(vec!["copl-rs", "checker", "--game", "nat", "--game=nat"])
            .expect_err("cli should reject");
        let message = err.to_string();
        assert!(message.contains("duplicate option: --game"));
    }

    #[test]
    fn parses_dash_prefixed_file_after_double_dash() {
        let cli = Cli::parse(vec![
            "copl-rs",
            "checker",
            "--game",
            "nat",
            "--",
            "-input.copl",
        ])
        .expect("cli should parse");
        let Command::Checker(cmd) = cli.command;
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::File("-input.copl".into()));
    }

    #[test]
    fn rejects_dash_prefixed_file_without_double_dash() {
        let err = Cli::parse(vec!["copl-rs", "checker", "--game", "nat", "-input.copl"])
            .expect_err("cli should reject");
        let message = err.to_string();
        assert!(message.contains("unexpected option: -input.copl"));
    }
}
