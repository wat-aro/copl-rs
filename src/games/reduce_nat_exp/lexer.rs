use crate::core::{CheckError, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Z,
    S,
    PlusWord,
    TimesWord,
    Is,
    By,
    PlusSymbol,
    TimesSymbol,
    ArrowReduce,
    ArrowDeterministic,
    ArrowMulti,
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

        match token.kind {
            TokenKind::Eof => {
                tokens.push(token);
                break;
            }
            _ => tokens.push(token),
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

        if self.source[self.index..].starts_with("--->") {
            self.bump_str("--->");
            return Ok(Token {
                kind: TokenKind::ArrowReduce,
                span,
            });
        }

        if self.source[self.index..].starts_with("-d->") {
            self.bump_str("-d->");
            return Ok(Token {
                kind: TokenKind::ArrowDeterministic,
                span,
            });
        }

        if self.source[self.index..].starts_with("-*->") {
            self.bump_str("-*->");
            return Ok(Token {
                kind: TokenKind::ArrowMulti,
                span,
            });
        }

        match ch {
            '+' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::PlusSymbol,
                    span,
                })
            }
            '*' => {
                self.bump_char(ch);
                Ok(Token {
                    kind: TokenKind::TimesSymbol,
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
            c if c.is_ascii_alphabetic() => self.lex_identifier(),
            _ => Err(self.error(&format!("unexpected character: '{ch}'"), span)),
        }
    }

    fn lex_identifier(&mut self) -> Result<Token, CheckError> {
        let start = self.current_span();
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
            return Err(self.error("expected identifier", start));
        }

        let kind = match text {
            "Z" => TokenKind::Z,
            "S" => TokenKind::S,
            "plus" => TokenKind::PlusWord,
            "times" => TokenKind::TimesWord,
            "is" => TokenKind::Is,
            "by" => TokenKind::By,
            _ => TokenKind::Identifier(text.to_owned()),
        };

        Ok(Token { kind, span: start })
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

    fn bump_str(&mut self, text: &str) {
        for ch in text.chars() {
            self.bump_char(ch);
        }
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
    fn tokenizes_simple_derivation() {
        let tokens = tokenize("Z + S(Z) -*-> S(Z) by MR-One {} ").expect("tokenize should succeed");
        assert_eq!(
            tokens
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            vec![
                TokenKind::Z,
                TokenKind::PlusSymbol,
                TokenKind::S,
                TokenKind::LParen,
                TokenKind::Z,
                TokenKind::RParen,
                TokenKind::ArrowMulti,
                TokenKind::S,
                TokenKind::LParen,
                TokenKind::Z,
                TokenKind::RParen,
                TokenKind::By,
                TokenKind::Identifier("MR-One".to_string()),
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokenizes_all_arrows() {
        let tokens = tokenize("Z ---> Z; Z -d-> Z; Z -*-> Z").expect("tokenize should succeed");
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::ArrowReduce)));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::ArrowDeterministic)));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.kind, TokenKind::ArrowMulti)));
    }

    #[test]
    fn skips_comments_and_whitespace() {
        let source = "// header\n\nZ plus Z is Z by P-Zero {}";
        let tokens = tokenize(source).expect("tokenize should succeed");
        assert!(matches!(tokens[0].kind, TokenKind::Z));
        assert!(matches!(tokens[1].kind, TokenKind::PlusWord));
    }

    #[test]
    fn reports_unexpected_character() {
        let err = tokenize("@").expect_err("tokenize should fail");
        assert!(err.message().contains("unexpected character"));
    }
}
