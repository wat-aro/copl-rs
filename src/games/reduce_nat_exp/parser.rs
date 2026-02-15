use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{NatTerm, ReduceNatExpDerivation, ReduceNatExpExpr, ReduceNatExpJudgment};

pub fn parse_source(source: &str) -> Result<ReduceNatExpDerivation, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<ReduceNatExpDerivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(ReduceNatExpDerivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<ReduceNatExpJudgment, CheckError> {
        let expr = self.parse_expr()?;

        if self.consume_arrow_reduce() {
            let to = self.parse_expr()?;
            return Ok(ReduceNatExpJudgment::ReducesTo { from: expr, to });
        }

        if self.consume_arrow_deterministic() {
            let to = self.parse_expr()?;
            return Ok(ReduceNatExpJudgment::DeterministicReducesTo { from: expr, to });
        }

        if self.consume_arrow_multi() {
            let to = self.parse_expr()?;
            return Ok(ReduceNatExpJudgment::MultiReducesTo { from: expr, to });
        }

        if let ReduceNatExpExpr::Nat(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_nat_term()?;
                self.expect_keyword_is()?;
                let result = self.parse_nat_term()?;
                return Ok(ReduceNatExpJudgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }
            if self.consume_times_word() {
                let right = self.parse_nat_term()?;
                self.expect_keyword_is()?;
                let result = self.parse_nat_term()?;
                return Ok(ReduceNatExpJudgment::TimesIs {
                    left,
                    right,
                    result,
                });
            }
        }

        Err(self.error_here("expected '--->', '-d->', '-*->', 'plus', or 'times'"))
    }

    fn parse_expr(&mut self) -> Result<ReduceNatExpExpr, CheckError> {
        self.parse_plus_expr()
    }

    fn parse_plus_expr(&mut self) -> Result<ReduceNatExpExpr, CheckError> {
        let mut expr = self.parse_times_expr()?;
        while self.consume_plus_symbol() {
            let right = self.parse_times_expr()?;
            expr = ReduceNatExpExpr::Plus(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_times_expr(&mut self) -> Result<ReduceNatExpExpr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_atom_expr()?;
            expr = ReduceNatExpExpr::Times(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_atom_expr(&mut self) -> Result<ReduceNatExpExpr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        let nat = self.parse_nat_term()?;
        Ok(ReduceNatExpExpr::Nat(nat))
    }

    fn parse_nat_term(&mut self) -> Result<NatTerm, CheckError> {
        if self.consume_z() {
            return Ok(NatTerm::Z);
        }
        if self.consume_s() {
            self.expect_lparen()?;
            let inner = self.parse_nat_term()?;
            self.expect_rparen()?;
            return Ok(NatTerm::S(Box::new(inner)));
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

    fn parse_subderivations(&mut self) -> Result<Vec<ReduceNatExpDerivation>, CheckError> {
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

    fn consume_arrow_reduce(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ArrowReduce))
    }

    fn consume_arrow_deterministic(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ArrowDeterministic))
    }

    fn consume_arrow_multi(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ArrowMulti))
    }

    fn consume_plus_word(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::PlusWord))
    }

    fn consume_times_word(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::TimesWord))
    }

    fn consume_plus_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::PlusSymbol))
    }

    fn consume_times_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::TimesSymbol))
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
    use crate::games::reduce_nat_exp::syntax::{NatTerm, ReduceNatExpExpr, ReduceNatExpJudgment};

    #[test]
    fn parses_fixture_021() {
        let source = include_str!("../../../copl/021.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "MR-One");
        assert_eq!(parsed.subderivations.len(), 1);
        assert_eq!(
            parsed.judgment,
            ReduceNatExpJudgment::MultiReducesTo {
                from: ReduceNatExpExpr::Plus(
                    Box::new(ReduceNatExpExpr::Nat(NatTerm::Z)),
                    Box::new(ReduceNatExpExpr::Nat(NatTerm::S(Box::new(NatTerm::S(
                        Box::new(NatTerm::Z),
                    ))))),
                ),
                to: ReduceNatExpExpr::Nat(NatTerm::S(Box::new(NatTerm::S(Box::new(NatTerm::Z))))),
            }
        );
    }

    #[test]
    fn parses_fixtures_022_to_024() {
        for source in [
            include_str!("../../../copl/022.copl"),
            include_str!("../../../copl/023.copl"),
            include_str!("../../../copl/024.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "Z ---> Z by R-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "R-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
Z -*-> Z by MR-One {
  Z ---> Z by R-Unknown {}
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }
}
