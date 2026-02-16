use std::marker::PhantomData;
use std::path::PathBuf;

use crate::core::GameKind;

use super::{CheckerCommand, CliError, InputSource};

pub(super) fn parse_checker_command(
    program: &str,
    args: &[String],
) -> Result<CheckerCommand, CliError> {
    let state = args
        .iter()
        .enumerate()
        .try_fold(CheckerParseState::default(), |state, (index, token)| {
            state.consume(program, args, index, token)
        })?;
    state.finish(program.to_owned())
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
    fn finish(self, program: String) -> Result<CheckerCommand, CliError> {
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

    fn finish(self, program: String) -> Result<CheckerCommand, CliError> {
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

    fn finish(self, program: String) -> Result<CheckerCommand, CliError> {
        let game = self
            .game
            .ok_or_else(|| CliError::missing_game(program.clone()))?;
        Ok(CheckerCommand {
            game,
            input: self.input.unwrap_or(InputSource::Stdin),
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
