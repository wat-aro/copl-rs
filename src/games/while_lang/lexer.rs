use crate::core::{CheckError, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    Identifier(String),
    True,
    False,
    Skip,
    If,
    Then,
    Else,
    While,
    Do,
    EvalTo,
    Changes,
    To,
    By,
    Turnstile,
    Assign,
    LessEqual,
    Less,
    Eq,
    And,
    Or,
    Not,
    Plus,
    Minus,
    Times,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Dot,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, CheckError> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
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

    fn tokenize(&mut self) -> Result<Vec<Token>, CheckError> {
        let mut tokens = Vec::new();
        loop {
            self.skip_layout();
            let token = self.next_token()?;
            let is_eof = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, CheckError> {
        let Some(ch) = self.peek_char() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                span: self.current_span(),
            });
        };

        let span = self.current_span();
        match ch {
            '0'..='9' => self.lex_int(),
            c if is_identifier_start(c) => self.lex_identifier(),
            '|' => {
                self.bump_char(ch);
                if self.consume_char('-') {
                    Ok(Token {
                        kind: TokenKind::Turnstile,
                        span,
                    })
                } else if self.consume_char('|') {
                    Ok(Token {
                        kind: TokenKind::Or,
                        span,
                    })
                } else {
                    Err(self.error("expected '-' or '|' after '|'", span))
                }
            }
            ':' => {
                self.bump_char(ch);
                if self.consume_char('=') {
                    Ok(Token {
                        kind: TokenKind::Assign,
                        span,
                    })
                } else {
                    Err(self.error("expected '=' after ':'", span))
                }
            }
            '<' => {
                self.bump_char(ch);
                if self.consume_char('=') {
                    Ok(Token {
                        kind: TokenKind::LessEqual,
                        span,
                    })
                } else {
                    Ok(Token {
                        kind: TokenKind::Less,
                        span,
                    })
                }
            }
            '=' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Eq,
                    span,
                })
            }
            '&' => {
                self.bump_char(ch);
                if self.consume_char('&') {
                    Ok(Token {
                        kind: TokenKind::And,
                        span,
                    })
                } else {
                    Err(self.error("expected '&' after '&'", span))
                }
            }
            '!' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Not,
                    span,
                })
            }
            '+' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Plus,
                    span,
                })
            }
            '-' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Minus,
                    span,
                })
            }
            '*' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Times,
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
            ',' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Comma,
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
            '.' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Dot,
                    span,
                })
            }
            _ => Err(self.error(&format!("unexpected character: '{ch}'"), span)),
        }
    }

    fn lex_int(&mut self) -> Result<Token, CheckError> {
        let span = self.current_span();
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.bump_char(ch);
            } else {
                break;
            }
        }

        let text = &self.source[start..self.index];
        let value = text
            .parse::<i64>()
            .map_err(|_| self.error("integer is out of range", span.clone()))?;

        Ok(Token {
            kind: TokenKind::Int(value),
            span,
        })
    }

    fn lex_identifier(&mut self) -> Result<Token, CheckError> {
        let span = self.current_span();
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if is_identifier_char(ch) {
                self.bump_char(ch);
            } else {
                break;
            }
        }

        let text = &self.source[start..self.index];
        if text.is_empty() {
            return Err(self.error("expected identifier", span));
        }

        let kind = match text {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "skip" => TokenKind::Skip,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "do" => TokenKind::Do,
            "evalto" => TokenKind::EvalTo,
            "changes" => TokenKind::Changes,
            "to" => TokenKind::To,
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

    fn consume_char(&mut self, expected: char) -> bool {
        if self.peek_char() == Some(expected) {
            self.bump_char(expected);
            true
        } else {
            false
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

fn is_identifier_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
}

#[cfg(test)]
mod tests {
    use super::{tokenize, TokenKind};

    #[test]
    fn tokenizes_command_judgment() {
        let tokens = tokenize("while (0 < x && true) do x := x - 1 changes x = 2 to x = 1")
            .expect("tokenize should succeed");

        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::While)));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::Assign)));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::Changes)));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::And)));
    }

    #[test]
    fn tokenizes_rule_name_identifier() {
        let tokens = tokenize("skip changes . to . by C-Skip {}").expect("tokenize should succeed");

        assert!(tokens.iter().any(|token| {
            matches!(token.kind, TokenKind::Identifier(ref name) if name == "C-Skip")
        }));
    }
}
