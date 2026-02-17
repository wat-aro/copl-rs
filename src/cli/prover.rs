use super::{game_command, CliError, ProverCommand};

pub(super) fn parse_prover_command(
    program: &str,
    args: &[String],
) -> Result<ProverCommand, CliError> {
    let command = game_command::parse_game_command(program, args)?;
    Ok(ProverCommand {
        game: command.game,
        input: command.input,
    })
}
