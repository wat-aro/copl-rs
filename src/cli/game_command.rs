use std::marker::PhantomData;
use std::path::PathBuf;

use crate::core::GameKind;

use super::{CliError, InputSource};

pub(super) fn parse_game_command(
    program: &str,
    args: &[String],
) -> Result<ParsedGameCommand, CliError> {
    let state = args
        .iter()
        .enumerate()
        .try_fold(ParseState::default(), |state, (index, token)| {
            state.consume(program, args, index, token)
        })?;
    state.finish(program.to_owned())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ParsedGameCommand {
    pub(super) game: GameKind,
    pub(super) input: InputSource,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct StateCore {
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
struct Parser<M, E> {
    core: StateCore,
    _marker: PhantomData<(M, E)>,
}

impl Parser<ModeOptions, ExpectAny> {
    fn default_state() -> Self {
        Self {
            core: StateCore::default(),
            _marker: PhantomData,
        }
    }

    fn consume(
        self,
        program: &str,
        args: &[String],
        index: usize,
        token: &str,
    ) -> Result<ParseState, CliError> {
        match classify_options_event(token) {
            OptionsEvent::EndOfOptions => Ok(ParseState::PositionalAny(Parser::new(self.core))),
            OptionsEvent::GameOption => {
                if self.core.game.is_some() {
                    return Err(CliError::duplicate_option(
                        program.to_owned(),
                        "--game".to_string(),
                    ));
                }
                Ok(ParseState::OptionsGameValue(Parser::new(self.core)))
            }
            OptionsEvent::GameInlineValue(value) => {
                if self.core.game.is_some() {
                    return Err(CliError::duplicate_option(
                        program.to_owned(),
                        "--game".to_string(),
                    ));
                }
                let game = parse_game_kind(program, value)?;
                Ok(ParseState::OptionsAny(Parser::new(
                    self.core.with_game(game),
                )))
            }
            OptionsEvent::UnexpectedOption(option) => Err(CliError::unexpected_option(
                program.to_owned(),
                option.to_string(),
            )),
            OptionsEvent::PositionalInput(path) => Ok(ParseState::OptionsAny(Parser::new(
                self.core.with_input(program, args, index, path)?,
            ))),
        }
    }
}

impl<M> Parser<M, ExpectAny> {
    fn finish(self, program: String) -> Result<ParsedGameCommand, CliError> {
        self.core.finish(program)
    }
}

impl Parser<ModeOptions, ExpectGameValue> {
    fn consume(self, program: &str, token: &str) -> Result<ParseState, CliError> {
        let game = parse_game_kind(program, token)?;
        Ok(ParseState::OptionsAny(Parser::new(
            self.core.with_game(game),
        )))
    }
}

impl Parser<ModePositional, ExpectAny> {
    fn consume(
        self,
        program: &str,
        args: &[String],
        index: usize,
        token: &str,
    ) -> Result<ParseState, CliError> {
        let core = self.core.with_input(program, args, index, token)?;
        Ok(ParseState::PositionalAny(Parser::new(core)))
    }
}

impl<M, E> Parser<M, E> {
    fn new(core: StateCore) -> Self {
        Self {
            core,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseState {
    OptionsAny(Parser<ModeOptions, ExpectAny>),
    OptionsGameValue(Parser<ModeOptions, ExpectGameValue>),
    PositionalAny(Parser<ModePositional, ExpectAny>),
}

impl Default for ParseState {
    fn default() -> Self {
        Self::OptionsAny(Parser::default_state())
    }
}

impl ParseState {
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

    fn finish(self, program: String) -> Result<ParsedGameCommand, CliError> {
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

impl StateCore {
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

    fn finish(self, program: String) -> Result<ParsedGameCommand, CliError> {
        let game = self
            .game
            .ok_or_else(|| CliError::missing_game(program.clone()))?;
        Ok(ParsedGameCommand {
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
