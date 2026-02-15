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
    fn routes_checker_compare_nat3_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"CompareNat3\" -*-\n\nS(S(Z)) is less than S(S(S(Z))) by L-Succ {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "CompareNat3"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "S(S(Z)) is less than S(S(S(Z)))");
    }

    #[test]
    fn routes_checker_eval_ml1_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalML1\" -*-\n\n3 + 5 evalto 8 by E-Plus {\n  3 evalto 3 by E-Int {};\n  5 evalto 5 by E-Int {};\n  3 plus 5 is 8 by B-Plus {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML1"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "3 + 5 evalto 8");
    }

    #[test]
    fn routes_checker_eval_ml1_err_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"EvalML1Err\" -*-\n\n1 + true + 2 evalto error by E-PlusErrorL {\n  1 + true evalto error by E-PlusBoolR {\n    true evalto true by E-Bool {}\n  }\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML1Err"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "1 + true + 2 evalto error");
    }

    #[test]
    fn routes_checker_eval_ml2_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"EvalML2\" -*-\n\nx = 3, y = 2 |- x evalto 3 by E-Var2 {\n  x = 3 |- x evalto 3 by E-Var1 {};\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML2"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "x = 3, y = 2 |- x evalto 3");
    }

    #[test]
    fn routes_checker_eval_ml3_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalML3\" -*-\n\n|- fun x -> x + 1 evalto ()[fun x -> x + 1] by E-Fun {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML3"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- fun x -> x + 1 evalto ()[fun x -> x + 1]");
    }

    #[test]
    fn routes_checker_eval_ml4_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalML4\" -*-\n\n|- [] evalto [] by E-Nil {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML4"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- [] evalto []");
    }

    #[test]
    fn routes_checker_eval_ml5_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalML5\" -*-\n\n|- [] evalto [] by E-Nil {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalML5"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- [] evalto []");
    }

    #[test]
    fn routes_checker_eval_cont_ml1_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalContML1\" -*-\n\n3 >> _ evalto 3 by E-Int {\n  3 => _ evalto 3 by C-Ret {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalContML1"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "3 >> _ evalto 3");
    }

    #[test]
    fn routes_checker_eval_cont_ml4_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalContML4\" -*-\n\n|- 1 >> _ evalto 1 by E-Int {\n  1 => _ evalto 1 by C-Ret {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalContML4"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- 1 >> _ evalto 1");
    }

    #[test]
    fn routes_checker_typing_ml4_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"TypingML4\" -*-\n\n|- 1 : int by T-Int {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "TypingML4"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- 1 : int");
    }

    #[test]
    fn routes_checker_poly_typing_ml4_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"PolyTypingML4\" -*-\n\n|- fun x -> x : 'a -> 'a by T-Abs {\n  x : 'a |- x : 'a by T-Var {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "PolyTypingML4"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- fun x -> x : 'a -> 'a");
    }

    #[test]
    fn routes_checker_nameless_ml3_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"NamelessML3\" -*-\n\n|- 1 ==> 1 by Tr-Int {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "NamelessML3"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- 1 ==> 1");
    }

    #[test]
    fn routes_checker_eval_nameless_ml3_with_derivation_system_name() {
        let mut stdin =
            &b"// -*- copl-game: \"EvalNamelessML3\" -*-\n\n|- 1 evalto 1 by E-Int {}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalNamelessML3"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "|- 1 evalto 1");
    }

    #[test]
    fn routes_checker_eval_nat_exp_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"EvalNatExp\" -*-\n\nZ + S(S(Z)) evalto S(S(Z)) by E-Plus {\n  Z evalto Z by E-Const {};\n  S(S(Z)) evalto S(S(Z)) by E-Const {};\n  Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "EvalNatExp"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "Z + S(S(Z)) evalto S(S(Z))");
    }

    #[test]
    fn routes_checker_reduce_nat_exp_with_derivation_system_name() {
        let mut stdin = &b"// -*- copl-game: \"ReduceNatExp\" -*-\n\nZ + S(S(Z)) -*-> S(S(Z)) by MR-One {\n  Z + S(S(Z)) ---> S(S(Z)) by R-Plus {\n    Z plus S(S(Z)) is S(S(Z)) by P-Zero {}\n  }\n}\n"[..];
        let mut out = Vec::new();
        let mut err = Vec::new();

        let result = run(
            vec!["copl-rs", "checker", "--game", "ReduceNatExp"],
            &mut stdin,
            &mut out,
            &mut err,
        );

        assert!(result.is_ok());
        let text = String::from_utf8(out).expect("stdout should be utf-8");
        assert_eq!(text.trim(), "Z + S(S(Z)) -*-> S(S(Z))");
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
