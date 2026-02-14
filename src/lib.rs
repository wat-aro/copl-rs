pub mod cli;
pub mod core;
pub mod games;

use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{self, Read, Write};

use cli::{Cli, Command, InputSource};

#[cfg(test)]
const MAX_INPUT_BYTES: usize = 1024;
#[cfg(not(test))]
const MAX_INPUT_BYTES: usize = 8 * 1024 * 1024;

pub fn run<I, S>(
    args: I,
    stdin: &mut dyn Read,
    stdout: &mut dyn Write,
    _stderr: &mut dyn Write,
) -> Result<(), RunError>
where
    I: IntoIterator<Item = S>,
    S: Into<String>,
{
    let cli = Cli::parse(args).map_err(RunError::Cli)?;
    execute(cli, stdin, stdout)
}

fn execute(cli: Cli, stdin: &mut dyn Read, stdout: &mut dyn Write) -> Result<(), RunError> {
    match cli.command {
        Command::Checker(command) => {
            let source = read_source(&command.input, stdin)?;
            let report = games::run_checker(command.game, &source).map_err(RunError::Check)?;
            writeln!(stdout, "{report}").map_err(|source| RunError::Io {
                source,
                context: "stdout".to_string(),
            })?;
            Ok(())
        }
    }
}

fn read_source(input: &InputSource, stdin: &mut dyn Read) -> Result<String, RunError> {
    match input {
        InputSource::Stdin => read_limited_utf8(stdin, "stdin"),
        InputSource::File(path) => {
            let context = path.display().to_string();
            let mut file = File::open(path).map_err(|source| RunError::Io {
                source,
                context: context.clone(),
            })?;
            read_limited_utf8(&mut file, &context)
        }
    }
}

fn read_limited_utf8(reader: &mut dyn Read, context: &str) -> Result<String, RunError> {
    let mut bytes = Vec::new();
    let mut limited_reader = reader.take((MAX_INPUT_BYTES + 1) as u64);
    limited_reader
        .read_to_end(&mut bytes)
        .map_err(|source| RunError::Io {
            source,
            context: context.to_string(),
        })?;
    if bytes.len() > MAX_INPUT_BYTES {
        return Err(RunError::InputTooLarge {
            context: context.to_string(),
            max_bytes: MAX_INPUT_BYTES,
        });
    }

    String::from_utf8(bytes).map_err(|_| RunError::InvalidUtf8 {
        context: context.to_string(),
    })
}

#[derive(Debug)]
pub enum RunError {
    Cli(cli::CliError),
    Io { source: io::Error, context: String },
    InputTooLarge { context: String, max_bytes: usize },
    InvalidUtf8 { context: String },
    Check(core::CheckError),
}

impl RunError {
    pub fn exit_code(&self) -> u8 {
        match self {
            Self::Cli(_) => 2,
            Self::Io { .. } => 1,
            Self::InputTooLarge { .. } => 1,
            Self::InvalidUtf8 { .. } => 1,
            Self::Check(_) => 1,
        }
    }
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cli(err) => write!(f, "{err}"),
            Self::Io { source, context } => {
                write!(f, "I/O error ({context}): {source}")
            }
            Self::InputTooLarge { context, max_bytes } => {
                write!(f, "input too large ({context}): limit is {max_bytes} bytes")
            }
            Self::InvalidUtf8 { context } => write!(f, "input is not valid UTF-8 ({context})"),
            Self::Check(err) => write!(f, "{err}"),
        }
    }
}

impl Error for RunError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Cli(err) => Some(err),
            Self::Io { source, .. } => Some(source),
            Self::InputTooLarge { .. } => None,
            Self::InvalidUtf8 { .. } => None,
            Self::Check(err) => Some(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{run, MAX_INPUT_BYTES};

    #[test]
    fn routes_checker_nat() {
        let mut stdin = &b"// -*- copl-game: \"Nat\" -*-\n\nZ plus Z is Z by P-Zero {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "nat"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "Z plus Z is Z");
    }

    #[test]
    fn routes_checker_compare_nat1() {
        let mut stdin =
            &b"// -*- copl-game: \"CompareNat1\" -*-\n\nS(S(Z)) is less than S(S(S(Z))) by L-Succ {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "comparenat1"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "S(S(Z)) is less than S(S(S(Z)))");
    }

    #[test]
    fn routes_checker_compare_nat1_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"CompareNat1\" -*-\n\nS(S(Z)) is less than S(S(S(Z))) by L-Succ {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "CompareNat1"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "S(S(Z)) is less than S(S(S(Z)))");
    }

    #[test]
    fn routes_checker_compare_nat2_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"CompareNat2\" -*-\n\nS(S(Z)) is less than S(S(S(Z))) by L-SuccSucc {\n  S(Z) is less than S(S(Z)) by L-SuccSucc {\n    Z is less than S(Z) by L-Zero {}\n  }\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "CompareNat2"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "S(S(Z)) is less than S(S(S(Z)))");
    }

    #[test]
    fn rejects_non_utf8_input() {
        let mut stdin = &b"\xFF"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "nat"],
            &mut stdin,
            &mut out,
            &mut err,
        )
        .expect_err("run should fail");
        let message = result.to_string();
        assert!(message.contains("not valid UTF-8"));
    }

    #[test]
    fn rejects_oversized_input() {
        let data = vec![b'a'; MAX_INPUT_BYTES + 1];
        let mut stdin = &data[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "nat"],
            &mut stdin,
            &mut out,
            &mut err,
        )
        .expect_err("run should fail");
        let message = result.to_string();
        assert!(message.contains("input too large"));
    }

    #[test]
    fn reports_file_open_error() {
        let mut stdin = &b""[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "nat", "missing-file.copl"],
            &mut stdin,
            &mut out,
            &mut err,
        )
        .expect_err("run should fail");
        assert!(result.to_string().contains("I/O error"));
    }

    #[test]
    fn reports_rule_violation_with_source_location() {
        let mut stdin =
            &b"S(Z) plus Z is S(Z) by P-Succ {\n  Z plus Z is Z by P-Unknown {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "nat"],
            &mut stdin,
            &mut out,
            &mut err,
        )
        .expect_err("run should fail");

        let message = result.to_string();
        assert!(message.contains("No such rule: P-Unknown"));
        assert!(message.contains("at 2:3"));
    }
}
