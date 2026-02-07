use std::io;
use std::io::Write;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut stderr = io::stderr().lock();

    match copl_rs::run(std::env::args(), &mut stdin, &mut stdout, &mut stderr) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            let _ = writeln!(stderr, "{err}");
            ExitCode::from(err.exit_code())
        }
    }
}
