use std::error::Error;
use std::fmt;
use std::path::PathBuf;

use crate::core::GameKind;

pub(crate) mod checker;
mod game_command;
pub(crate) mod prover;

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
            "prover" => parse_prover(program, iter.collect()),
            _ => Err(CliError::unknown_subcommand(program, subcommand)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Checker(CheckerCommand),
    Prover(ProverCommand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckerCommand {
    pub game: GameKind,
    pub input: InputSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProverCommand {
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
    let command = checker::parse_checker_command(&program, &args)?;
    Ok(Cli {
        command: Command::Checker(command),
    })
}

fn parse_prover(program: String, args: Vec<String>) -> Result<Cli, CliError> {
    let command = prover::parse_prover_command(&program, &args)?;
    Ok(Cli {
        command: Command::Prover(command),
    })
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

    pub(super) fn missing_game(program: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::MissingGame,
        }
    }

    pub(super) fn missing_game_value(program: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::MissingGameValue,
        }
    }

    pub(super) fn invalid_game(program: String, source: crate::core::ParseGameKindError) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::InvalidGame { source },
        }
    }

    pub(super) fn unexpected_option(program: String, option: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::UnexpectedOption { option },
        }
    }

    pub(super) fn duplicate_option(program: String, option: String) -> Self {
        Self {
            usage: usage_for(&program),
            kind: CliErrorKind::DuplicateOption { option },
        }
    }

    pub(super) fn too_many_inputs(program: String, inputs: Vec<String>) -> Self {
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
    format!(
        "Usage:\n  {program} checker --game <name> [file]\n  {program} prover --game <name> [file]"
    )
}

#[cfg(test)]
mod tests {
    use super::{Cli, Command, InputSource};
    use crate::core::GameKind;

    #[test]
    fn parses_checker_with_split_subcommand_parser() {
        let args = vec!["--game".to_string(), "nat".to_string()];
        let command = super::checker::parse_checker_command("copl-rs", &args)
            .expect("checker parser should parse");
        assert_eq!(command.game, GameKind::Nat);
        assert_eq!(command.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_stdin() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "nat"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_prover_with_stdin() {
        let cli = Cli::parse(vec!["copl-rs", "prover", "--game", "nat"]).expect("cli should parse");
        let Command::Prover(cmd) = cli.command else {
            panic!("expected prover command");
        };
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_nat() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "Nat"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::CompareNat1);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::CompareNat1);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::CompareNat2);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::CompareNat2);
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_compare_nat3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "CompareNat3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "CompareNat3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_compare_nat3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "comparenat3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "CompareNat3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml1() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML1"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml1() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml1"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml1_err() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML1Err"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML1Err");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml1_err() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalml1err"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML1Err");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml2() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML2"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml2() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml2"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml3() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML3"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml3() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml3"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml4() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML4"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml4() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml4"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml5() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML5"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML5");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml5() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml5"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML5");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_ml6() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "EvalML6"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML6");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_ml6() {
        let cli =
            Cli::parse(vec!["copl-rs", "checker", "--game", "evalml6"]).expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalML6");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_cont_ml1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalContML1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalContML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_cont_ml1() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalcontml1"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalContML1");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_cont_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalContML4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalContML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_cont_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalcontml4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalContML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_typing_ml2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "TypingML2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_typing_ml2() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "typingml2"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML2");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_typing_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "TypingML3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_typing_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "typingml3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_typing_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "TypingML4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_typing_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "typingml4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "TypingML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_poly_typing_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "PolyTypingML4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "PolyTypingML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_poly_typing_ml4() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "polytypingml4"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "PolyTypingML4");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "NamelessML3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "NamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "namelessml3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "NamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalNamelessML3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalNamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_nameless_ml3() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalnamelessml3"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalNamelessML3");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_eval_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "EvalNatExp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_eval_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "evalnatexp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "EvalNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_derivation_system_name_reduce_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "ReduceNatExp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "ReduceNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn keeps_backward_compatibility_for_lowercase_reduce_nat_exp() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game", "reducenatexp"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game.as_str(), "ReduceNatExp");
        assert_eq!(cmd.input, InputSource::Stdin);
    }

    #[test]
    fn parses_checker_with_file() {
        let cli = Cli::parse(vec!["copl-rs", "checker", "--game=nat", "001.copl"])
            .expect("cli should parse");
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::File("001.copl".into()));
    }

    #[test]
    fn parses_prover_with_file() {
        let cli = Cli::parse(vec!["copl-rs", "prover", "--game=nat", "001.copl"])
            .expect("cli should parse");
        let Command::Prover(cmd) = cli.command else {
            panic!("expected prover command");
        };
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
        let Command::Checker(cmd) = cli.command else {
            panic!("expected checker command");
        };
        assert_eq!(cmd.game, GameKind::Nat);
        assert_eq!(cmd.input, InputSource::File("-input.copl".into()));
    }

    #[test]
    fn parses_prover_dash_prefixed_file_after_double_dash() {
        let cli = Cli::parse(vec![
            "copl-rs",
            "prover",
            "--game",
            "nat",
            "--",
            "-input.copl",
        ])
        .expect("cli should parse");
        let Command::Prover(cmd) = cli.command else {
            panic!("expected prover command");
        };
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
