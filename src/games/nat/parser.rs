use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{NatDerivation, NatJudgment, NatOperator, NatRule, NatTerm};

pub fn parse_source(source: &str) -> Result<NatDerivation, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<NatDerivation, CheckError> {
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule = self.parse_rule()?;
        self.expect_lbrace()?;
        let premises = self.parse_premises()?;
        self.expect_rbrace()?;
        Ok(NatDerivation {
            judgment,
            rule,
            premises,
        })
    }

    fn parse_judgment(&mut self) -> Result<NatJudgment, CheckError> {
        let left = self.parse_term()?;
        let operator = self.parse_operator()?;
        let right = self.parse_term()?;
        self.expect_keyword_is()?;
        let result = self.parse_term()?;
        Ok(NatJudgment {
            left,
            operator,
            right,
            result,
        })
    }

    fn parse_operator(&mut self) -> Result<NatOperator, CheckError> {
        if self.consume_plus() {
            Ok(NatOperator::Plus)
        } else if self.consume_times() {
            Ok(NatOperator::Times)
        } else {
            Err(self.error_here("expected 'plus' or 'times'"))
        }
    }

    fn parse_term(&mut self) -> Result<NatTerm, CheckError> {
        if self.consume_z() {
            return Ok(NatTerm::Z);
        }
        if self.consume_s() {
            self.expect_lparen()?;
            let inner = self.parse_term()?;
            self.expect_rparen()?;
            return Ok(NatTerm::S(Box::new(inner)));
        }
        Err(self.error_here("expected Nat term"))
    }

    fn parse_rule(&mut self) -> Result<NatRule, CheckError> {
        let token = self.peek();
        let TokenKind::Identifier(name) = &token.kind else {
            return Err(self.error_here("expected rule name"));
        };
        let name = name.clone();
        let span = token.span.clone();
        self.bump();
        NatRule::parse(&name)
            .ok_or_else(|| self.error_with_span(&format!("unknown rule name: {name}"), span))
    }

    fn parse_premises(&mut self) -> Result<Vec<NatDerivation>, CheckError> {
        if self.at_rbrace() {
            return Ok(Vec::new());
        }

        let mut premises = vec![self.parse_derivation()?];
        loop {
            if self.at_rbrace() {
                break;
            }
            self.expect_semicolon_or_rbrace()?;
            if self.at_rbrace() {
                break;
            }
            premises.push(self.parse_derivation()?);
        }
        Ok(premises)
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        if self.consume_by() {
            Ok(())
        } else {
            Err(self.error_here("expected 'by'"))
        }
    }

    fn expect_keyword_is(&mut self) -> Result<(), CheckError> {
        if self.consume_is() {
            Ok(())
        } else {
            Err(self.error_here("expected 'is'"))
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

    fn consume_plus(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Plus))
    }

    fn consume_times(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Times))
    }

    fn consume_is(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Is))
    }

    fn consume_by(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::By))
    }

    fn consume_z(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Z))
    }

    fn consume_s(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::S))
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
    use crate::games::nat::syntax::{NatOperator, NatRule, NatTerm};

    #[test]
    fn parses_fixture_001() {
        let source = include_str!("../../../copl/001.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule, NatRule::PZero);
        assert!(parsed.premises.is_empty());
        assert_eq!(parsed.judgment.operator, NatOperator::Plus);
        assert_eq!(parsed.judgment.left, NatTerm::Z);
        assert_eq!(parsed.judgment.right, NatTerm::Z);
        assert_eq!(parsed.judgment.result, NatTerm::Z);
    }

    #[test]
    fn parses_fixtures_002_to_008() {
        for source in [
            include_str!("../../../copl/002.copl"),
            include_str!("../../../copl/003.copl"),
            include_str!("../../../copl/004.copl"),
            include_str!("../../../copl/005.copl"),
            include_str!("../../../copl/006.copl"),
            include_str!("../../../copl/007.copl"),
            include_str!("../../../copl/008.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn rejects_unknown_rule_name_immediately() {
        let source = "Z plus Z is Z by P-Unknown {}";
        let err = parse_source(source).expect_err("parser should fail");
        assert!(err.message().contains("unknown rule name"));
    }
}
