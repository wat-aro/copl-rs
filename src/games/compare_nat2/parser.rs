use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{CompareNat2Derivation, CompareNat2Judgment, CompareNat2Term};

pub fn parse_source(source: &str) -> Result<CompareNat2Derivation, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::parse("input is empty"));
    }

    let tokens = tokenize(source)?;
    let mut parser = Parser::new(tokens);
    let derivation = parser.parse_derivation()?;
    parser.expect_eof()?;
    Ok(derivation)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_derivation(&mut self) -> Result<CompareNat2Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(CompareNat2Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<CompareNat2Judgment, CheckError> {
        let left = self.parse_term()?;
        self.expect_keyword_is()?;
        self.expect_keyword_less()?;
        self.expect_keyword_than()?;
        let right = self.parse_term()?;
        Ok(CompareNat2Judgment { left, right })
    }

    fn parse_term(&mut self) -> Result<CompareNat2Term, CheckError> {
        if self.consume_z() {
            return Ok(CompareNat2Term::Z);
        }
        if self.consume_s() {
            self.expect_lparen()?;
            let inner = self.parse_term()?;
            self.expect_rparen()?;
            return Ok(CompareNat2Term::S(Box::new(inner)));
        }
        Err(self.error_here("expected Nat term"))
    }

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        let token = self.peek();
        let TokenKind::Identifier(name) = &token.kind else {
            return Err(self.error_here("expected rule name"));
        };
        let name = name.clone();
        self.bump();
        Ok(name)
    }

    fn parse_subderivations(&mut self) -> Result<Vec<CompareNat2Derivation>, CheckError> {
        if self.at_rbrace() {
            return Ok(Vec::new());
        }

        let mut subderivations = vec![self.parse_derivation()?];
        loop {
            if self.at_rbrace() {
                break;
            }
            self.expect_semicolon_or_rbrace()?;
            if self.at_rbrace() {
                break;
            }
            subderivations.push(self.parse_derivation()?);
        }
        Ok(subderivations)
    }

    fn expect_keyword_is(&mut self) -> Result<(), CheckError> {
        if self.consume_is() {
            Ok(())
        } else {
            Err(self.error_here("expected 'is'"))
        }
    }

    fn expect_keyword_less(&mut self) -> Result<(), CheckError> {
        if self.consume_less() {
            Ok(())
        } else {
            Err(self.error_here("expected 'less'"))
        }
    }

    fn expect_keyword_than(&mut self) -> Result<(), CheckError> {
        if self.consume_than() {
            Ok(())
        } else {
            Err(self.error_here("expected 'than'"))
        }
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        if self.consume_by() {
            Ok(())
        } else {
            Err(self.error_here("expected 'by'"))
        }
    }

    fn expect_lparen(&mut self) -> Result<(), CheckError> {
        if self.consume_lparen() {
            Ok(())
        } else {
            Err(self.error_here("expected '('"))
        }
    }

    fn expect_rparen(&mut self) -> Result<(), CheckError> {
        if self.consume_rparen() {
            Ok(())
        } else {
            Err(self.error_here("expected ')'"))
        }
    }

    fn expect_lbrace(&mut self) -> Result<(), CheckError> {
        if self.consume_lbrace() {
            Ok(())
        } else {
            Err(self.error_here("expected '{'"))
        }
    }

    fn expect_rbrace(&mut self) -> Result<(), CheckError> {
        if self.consume_rbrace() {
            Ok(())
        } else {
            Err(self.error_here("expected '}'"))
        }
    }

    fn expect_semicolon_or_rbrace(&mut self) -> Result<(), CheckError> {
        if self.consume_semicolon() || self.at_rbrace() {
            Ok(())
        } else {
            Err(self.error_here("expected ';' or '}'"))
        }
    }

    fn expect_eof(&self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Eof) {
            Ok(())
        } else {
            Err(self.error_here("expected end of input"))
        }
    }

    fn consume_z(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Z))
    }

    fn consume_s(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::S))
    }

    fn consume_is(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Is))
    }

    fn consume_less(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Less))
    }

    fn consume_than(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Than))
    }

    fn consume_by(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::By))
    }

    fn consume_lparen(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LParen))
    }

    fn consume_rparen(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::RParen))
    }

    fn consume_lbrace(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LBrace))
    }

    fn consume_rbrace(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::RBrace))
    }

    fn consume_semicolon(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Semicolon))
    }

    fn consume_if(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn at_rbrace(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RBrace)
    }

    fn bump(&mut self) {
        if !matches!(self.peek().kind, TokenKind::Eof) {
            self.index += 1;
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn error_here(&self, message: &str) -> CheckError {
        self.error_with_span(message, self.peek().span.clone())
    }

    fn error_with_span(&self, message: &str, span: SourceSpan) -> CheckError {
        CheckError::parse(message.to_string()).with_span(span)
    }
}

#[cfg(test)]
mod tests {
    use super::parse_source;
    use crate::games::compare_nat2::syntax::{CompareNat2Judgment, CompareNat2Term};

    #[test]
    fn parses_fixture_010() {
        let source = include_str!("../../../copl/010.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "L-SuccSucc");
        assert_eq!(parsed.subderivations.len(), 1);
        assert_eq!(
            parsed.judgment,
            CompareNat2Judgment {
                left: CompareNat2Term::S(Box::new(CompareNat2Term::S(Box::new(
                    CompareNat2Term::Z,
                )))),
                right: CompareNat2Term::S(Box::new(CompareNat2Term::S(Box::new(
                    CompareNat2Term::S(Box::new(CompareNat2Term::Z)),
                )))),
            }
        );
    }

    #[test]
    fn parses_fixture_013() {
        let source = include_str!("../../../copl/013.copl");
        parse_source(source).expect("fixture should parse");
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "S(Z) is less than S(S(Z)) by L-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "L-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
S(Z) is less than S(S(S(Z))) by L-SuccSucc {
  Z is less than S(S(Z)) by L-SuccSucc {
    Z is less than S(Z) by L-Zero {}
  }
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }
}
