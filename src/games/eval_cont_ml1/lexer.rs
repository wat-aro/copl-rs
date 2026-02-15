use crate::core::{CheckError, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    True,
    False,
    If,
    Then,
    Else,
    EvalTo,
    PlusWord,
    MinusWord,
    TimesWord,
    Less,
    Than,
    Is,
    By,
    ContArrow,
    ContEvalArrow,
    Underscore,
    PlusSymbol,
    MinusSymbol,
    TimesSymbol,
    LtSymbol,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Identifier(String),
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, CheckError> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next_token()?;
        let is_eof = matches!(token.kind, TokenKind::Eof);
        tokens.push(token);
        if is_eof {
            break;
        }
    }

    Ok(tokens)
}

struct Lexer<'a> {
    source: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn next_token(&mut self) -> Result<Token, CheckError> {
        self.skip_layout();
        let span = self.current_span();

        let Some(ch) = self.peek_char() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                span,
            });
        };

        match ch {
            '+' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::PlusSymbol,
                    span,
                })
            }
            '-' => {
                if self
                    .peek_next_char()
                    .is_some_and(|next| next.is_ascii_digit())
                {
                    let value = self.lex_int_literal()?;
                    Ok(Token {
                        kind: TokenKind::Int(value),
                        span,
                    })
                } else {
                    self.bump_char(ch);
                    Ok(Token {
                        kind: TokenKind::MinusSymbol,
                        span,
                    })
                }
            }
            '*' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::TimesSymbol,
                    span,
                })
            }
            '<' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::LtSymbol,
                    span,
                })
            }
            '=' => {
                if self.peek_next_char() == Some('>') {
                    self.bump_char('=');
                    self.bump_char('>');
                    Ok(Token {
                        kind: TokenKind::ContEvalArrow,
                        span,
                    })
                } else {
                    Err(self.error("expected '=>'", span))
                }
            }
            '>' => {
                if self.peek_next_char() == Some('>') {
                    self.bump_char('>');
                    self.bump_char('>');
                    Ok(Token {
                        kind: TokenKind::ContArrow,
                        span,
                    })
                } else {
                    Err(self.error("expected '>>'", span))
                }
            }
            '_' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Underscore,
                    span,
                })
            }
            '(' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::LParen,
                    span,
                })
            }
            ')' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::RParen,
                    span,
                })
            }
            '{' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::LBrace,
                    span,
                })
            }
            '}' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::RBrace,
                    span,
                })
            }
            ';' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Semicolon,
                    span,
                })
            }
            c if c.is_ascii_digit() => {
                let value = self.lex_int_literal()?;
                Ok(Token {
                    kind: TokenKind::Int(value),
                    span,
                })
            }
            c if c.is_ascii_alphabetic() => self.lex_identifier(),
            _ => Err(self.error(&format!("unexpected character: '{ch}'"), span)),
        }
    }

    fn lex_int_literal(&mut self) -> Result<i64, CheckError> {
        let start = self.current_span();
        let start_index = self.index;

        if self.peek_char() == Some('-') {
            self.bump_char('-');
        }

        let mut saw_digit = false;
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.bump_char(ch);
                saw_digit = true;
            } else {
                break;
            }
        }

        if !saw_digit {
            return Err(self.error("expected integer literal", start));
        }

        let text = &self.source[start_index..self.index];
        text.parse::<i64>()
            .map_err(|_| self.error("invalid integer literal", start))
    }

    fn lex_identifier(&mut self) -> Result<Token, CheckError> {
        let span = self.current_span();
        let start_index = self.index;

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '-' {
                self.bump_char(ch);
            } else {
                break;
            }
        }

        let text = &self.source[start_index..self.index];
        if text.is_empty() {
            return Err(self.error("expected identifier", span));
        }

        let kind = match text {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "evalto" => TokenKind::EvalTo,
            "plus" => TokenKind::PlusWord,
            "minus" => TokenKind::MinusWord,
            "times" => TokenKind::TimesWord,
            "less" => TokenKind::Less,
            "than" => TokenKind::Than,
            "is" => TokenKind::Is,
            "by" => TokenKind::By,
            _ => TokenKind::Identifier(text.to_string()),
        };

        Ok(Token { kind, span })
    }

    fn skip_layout(&mut self) {
        loop {
            let before = self.index;

            while let Some(ch) = self.peek_char() {
                if ch.is_whitespace() {
                    self.bump_char(ch);
                } else {
                    break;
                }
            }

            if self.source[self.index..].starts_with("//") {
                while let Some(ch) = self.peek_char() {
                    self.bump_char(ch);
                    if ch == '\n' {
                        break;
                    }
                }
            }

            if self.index == before {
                break;
            }
        }
    }

    fn current_span(&self) -> SourceSpan {
        SourceSpan {
            line: self.line,
            column: self.column,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    fn peek_next_char(&self) -> Option<char> {
        let mut chars = self.source[self.index..].chars();
        chars.next()?;
        chars.next()
    }

    fn bump_char(&mut self, ch: char) {
        self.index += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    fn error(&self, message: &str, span: SourceSpan) -> CheckError {
        CheckError::parse(message.to_string()).with_span(span)
    }
}

#[cfg(test)]
mod tests {
    use super::{tokenize, TokenKind};

    #[test]
    fn tokenizes_continuation_operators() {
        let tokens = tokenize("3 >> {_ + 5} => _ evalto 8").expect("tokenize should succeed");
        let kinds = tokens
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>();
        assert!(kinds.contains(&TokenKind::ContArrow));
        assert!(kinds.contains(&TokenKind::ContEvalArrow));
        assert!(kinds.contains(&TokenKind::Underscore));
    }

    #[test]
    fn reports_error_for_single_greater_than() {
        let err = tokenize("> evalto 1").expect_err("tokenize should fail");
        assert!(err.message().contains("expected '>>'"));
    }
}
