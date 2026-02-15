use crate::core::{CheckError, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    True,
    False,
    Let,
    Rec,
    In,
    If,
    Then,
    Else,
    Fun,
    Match,
    With,
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
    Comma,
    Turnstile,
    Bar,
    ConsSymbol,
    PlusSymbol,
    MinusSymbol,
    TimesSymbol,
    LtSymbol,
    LParen,
    RParen,
    LBracket,
    RBracket,
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
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::Equal,
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
            ':' => {
                if self.peek_next_char() == Some(':') {
                    self.bump_char(':');
                    self.bump_char(':');
                    Ok(Token {
                        kind: TokenKind::ConsSymbol,
                        span,
                    })
                } else {
                    Err(self.error("expected ':'", span))
                }
            }
            '|' => {
                self.bump_char(ch);
                if self.peek_char() == Some('-') {
                    self.bump_char('-');
                    Ok(Token {
                        kind: TokenKind::Turnstile,
                        span,
                    })
                } else {
                    Ok(Token {
                        kind: TokenKind::Bar,
                        span,
                    })
                }
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
            '[' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::LBracket,
                    span,
                })
            }
            ']' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::RBracket,
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
            "let" => TokenKind::Let,
            "rec" => TokenKind::Rec,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "fun" => TokenKind::Fun,
            "match" => TokenKind::Match,
            "with" => TokenKind::With,
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
    fn tokenizes_fun_and_closure_syntax() {
        let source = "|- fun x -> x + 1 evalto ()[fun x -> x + 1] by E-Fun {}";
        let tokens = tokenize(source).expect("tokenize should succeed");
        let kinds = tokens
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>();
        assert!(kinds.contains(&TokenKind::Fun));
        assert!(kinds.contains(&TokenKind::Arrow));
        assert!(kinds.contains(&TokenKind::LBracket));
        assert!(kinds.contains(&TokenKind::RBracket));
    }

    #[test]
    fn tokenizes_negative_integers() {
        let source = "|- -2 evalto -2 by E-Int {}";
        let tokens = tokenize(source).expect("tokenize should succeed");
        assert_eq!(tokens[1].kind, TokenKind::Int(-2));
        assert_eq!(tokens[3].kind, TokenKind::Int(-2));
    }

    #[test]
    fn skips_comments_and_whitespace() {
        let source = "// comment\n\n|- 3 evalto 3 by E-Int {}";
        let tokens = tokenize(source).expect("tokenize should succeed");
        assert_eq!(tokens[0].kind, TokenKind::Turnstile);
        assert_eq!(tokens[0].span.line, 3);
        assert_eq!(tokens[0].span.column, 1);
    }

    #[test]
    fn reports_unexpected_character() {
        let err = tokenize("|- 1 @ 2 evalto 3 by E-Int {}").expect_err("should fail");
        assert!(err.message().contains("unexpected character"));
    }

    #[test]
    fn tokenizes_match_and_cons_syntax() {
        let source = "|- match x with [] -> 0 | a :: b -> a evalto 0 by E-MatchNil {}";
        let tokens = tokenize(source).expect("tokenize should succeed");
        let kinds = tokens
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>();
        assert!(kinds.contains(&TokenKind::Match));
        assert!(kinds.contains(&TokenKind::With));
        assert!(kinds.contains(&TokenKind::Bar));
        assert!(kinds.contains(&TokenKind::ConsSymbol));
    }
}
