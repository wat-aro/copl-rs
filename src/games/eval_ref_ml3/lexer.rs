use crate::core::{CheckError, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    True,
    False,
    If,
    Then,
    Else,
    Fun,
    Let,
    Rec,
    In,
    Ref,
    Arrow,
    EvalTo,
    PlusWord,
    MinusWord,
    TimesWord,
    Less,
    Than,
    Is,
    By,
    Equal,
    Turnstile,
    Slash,
    Assign,
    Bang,
    PlusSymbol,
    MinusSymbol,
    TimesSymbol,
    LtSymbol,
    Comma,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semicolon,
    Identifier(String),
    Location(String),
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
                self.bump_char('+');
                Ok(Token {
                    kind: TokenKind::PlusSymbol,
                    span,
                })
            }
            '-' => {
                if self.peek_next_char() == Some('>') {
                    self.bump_char('-');
                    self.bump_char('>');
                    Ok(Token {
                        kind: TokenKind::Arrow,
                        span,
                    })
                } else if self
                    .peek_next_char()
                    .is_some_and(|next| next.is_ascii_digit())
                {
                    let value = self.lex_int_literal()?;
                    Ok(Token {
                        kind: TokenKind::Int(value),
                        span,
                    })
                } else {
                    self.bump_char('-');
                    Ok(Token {
                        kind: TokenKind::MinusSymbol,
                        span,
                    })
                }
            }
            '*' => {
                self.bump_char('*');
                Ok(Token {
                    kind: TokenKind::TimesSymbol,
                    span,
                })
            }
            '<' => {
                self.bump_char('<');
                Ok(Token {
                    kind: TokenKind::LtSymbol,
                    span,
                })
            }
            '=' => {
                self.bump_char('=');
                Ok(Token {
                    kind: TokenKind::Equal,
                    span,
                })
            }
            '/' => {
                self.bump_char('/');
                Ok(Token {
                    kind: TokenKind::Slash,
                    span,
                })
            }
            ':' => {
                if self.peek_next_char() == Some('=') {
                    self.bump_char(':');
                    self.bump_char('=');
                    Ok(Token {
                        kind: TokenKind::Assign,
                        span,
                    })
                } else {
                    Err(self.error("expected '='", span))
                }
            }
            '!' => {
                self.bump_char('!');
                Ok(Token {
                    kind: TokenKind::Bang,
                    span,
                })
            }
            ',' => {
                self.bump_char(',');
                Ok(Token {
                    kind: TokenKind::Comma,
                    span,
                })
            }
            '(' => {
                self.bump_char('(');
                Ok(Token {
                    kind: TokenKind::LParen,
                    span,
                })
            }
            ')' => {
                self.bump_char(')');
                Ok(Token {
                    kind: TokenKind::RParen,
                    span,
                })
            }
            '[' => {
                self.bump_char('[');
                Ok(Token {
                    kind: TokenKind::LBracket,
                    span,
                })
            }
            ']' => {
                self.bump_char(']');
                Ok(Token {
                    kind: TokenKind::RBracket,
                    span,
                })
            }
            '{' => {
                self.bump_char('{');
                Ok(Token {
                    kind: TokenKind::LBrace,
                    span,
                })
            }
            '}' => {
                self.bump_char('}');
                Ok(Token {
                    kind: TokenKind::RBrace,
                    span,
                })
            }
            ';' => {
                self.bump_char(';');
                Ok(Token {
                    kind: TokenKind::Semicolon,
                    span,
                })
            }
            '|' => {
                self.bump_char('|');
                if self.peek_char() == Some('-') {
                    self.bump_char('-');
                    Ok(Token {
                        kind: TokenKind::Turnstile,
                        span,
                    })
                } else {
                    Err(self.error("expected '-'", span))
                }
            }
            '@' => {
                self.bump_char('@');
                let location = self.lex_identifier(span.clone())?;
                Ok(Token {
                    kind: TokenKind::Location(location),
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
            c if c.is_ascii_alphabetic() || c == '_' => {
                let text = self.lex_identifier(span.clone())?;
                let kind = match text.as_str() {
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "if" => TokenKind::If,
                    "then" => TokenKind::Then,
                    "else" => TokenKind::Else,
                    "fun" => TokenKind::Fun,
                    "let" => TokenKind::Let,
                    "rec" => TokenKind::Rec,
                    "in" => TokenKind::In,
                    "ref" => TokenKind::Ref,
                    "evalto" => TokenKind::EvalTo,
                    "plus" => TokenKind::PlusWord,
                    "minus" => TokenKind::MinusWord,
                    "times" => TokenKind::TimesWord,
                    "less" => TokenKind::Less,
                    "than" => TokenKind::Than,
                    "is" => TokenKind::Is,
                    "by" => TokenKind::By,
                    _ => TokenKind::Identifier(text),
                };
                Ok(Token { kind, span })
            }
            _ => Err(self.error(&format!("unexpected character: '{ch}'"), span)),
        }
    }

    fn skip_layout(&mut self) {
        loop {
            while let Some(ch) = self.peek_char() {
                if ch.is_whitespace() {
                    self.bump_char(ch);
                } else {
                    break;
                }
            }

            if self.peek_char() == Some('/') && self.peek_next_char() == Some('/') {
                self.bump_char('/');
                self.bump_char('/');
                while let Some(ch) = self.peek_char() {
                    self.bump_char(ch);
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn lex_int_literal(&mut self) -> Result<i64, CheckError> {
        let start = self.current_span();
        let mut literal = String::new();
        if self.peek_char() == Some('-') {
            literal.push('-');
            self.bump_char('-');
        }

        let mut saw_digit = false;
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                saw_digit = true;
                literal.push(ch);
                self.bump_char(ch);
            } else {
                break;
            }
        }

        if !saw_digit {
            return Err(self.error("expected integer", start));
        }

        literal
            .parse::<i64>()
            .map_err(|_| self.error("integer literal out of range", start))
    }

    fn lex_identifier(&mut self, span: SourceSpan) -> Result<String, CheckError> {
        let mut identifier = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '\'' {
                identifier.push(ch);
                self.bump_char(ch);
            } else {
                break;
            }
        }

        if identifier.is_empty() {
            return Err(self.error("expected identifier", span));
        }

        Ok(identifier)
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
        let mut iter = self.source[self.index..].chars();
        iter.next()?;
        iter.next()
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
    fn tokenizes_ref_judgment_tokens() {
        let source = "|- ref 1 / () evalto @l0 / @l0 = 1";
        let tokens = tokenize(source).expect("source should tokenize");

        assert!(tokens.iter().any(|token| token.kind == TokenKind::Ref));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Slash));
        assert!(tokens
            .iter()
            .any(|token| token.kind == TokenKind::Location("l0".to_string())));
    }

    #[test]
    fn tokenizes_fun_and_binary_operator_tokens() {
        let source = "fun x -> if x < 1 then x + 1 else x * 2";
        let tokens = tokenize(source).expect("source should tokenize");

        assert!(tokens.iter().any(|token| token.kind == TokenKind::Fun));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Arrow));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::LtSymbol));
        assert!(tokens
            .iter()
            .any(|token| token.kind == TokenKind::PlusSymbol));
        assert!(tokens
            .iter()
            .any(|token| token.kind == TokenKind::TimesSymbol));
    }
}
