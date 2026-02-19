use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{EvalNatExpDerivation, EvalNatExpExpr, EvalNatExpJudgment, NatTerm};

pub fn parse_source(source: &str) -> Result<EvalNatExpDerivation, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::parse("input is empty"));
    }

    let tokens = tokenize(source)?;
    let mut parser = Parser::new(tokens);
    let derivation = parser.parse_derivation()?;
    parser.consume_trailing_semicolons();
    parser.expect_eof()?;
    Ok(derivation)
}

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalNatExpJudgment, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::parse("input is empty"));
    }

    let tokens = tokenize(source)?;
    let mut parser = Parser::new(tokens);
    let judgment = parser.parse_judgment()?;
    parser.consume_trailing_semicolons();
    parser.expect_eof()?;
    Ok(judgment)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_derivation(&mut self) -> Result<EvalNatExpDerivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalNatExpDerivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalNatExpJudgment, CheckError> {
        let expr = self.parse_expr()?;
        if self.consume_evalto() {
            let value = self.parse_nat_term()?;
            return Ok(EvalNatExpJudgment::EvalTo { expr, value });
        }

        if let EvalNatExpExpr::Nat(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_nat_term()?;
                self.expect_keyword_is()?;
                let result = self.parse_nat_term()?;
                return Ok(EvalNatExpJudgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }
            if self.consume_times_word() {
                let right = self.parse_nat_term()?;
                self.expect_keyword_is()?;
                let result = self.parse_nat_term()?;
                return Ok(EvalNatExpJudgment::TimesIs {
                    left,
                    right,
                    result,
                });
            }
        }

        Err(self.error_here("expected 'evalto', 'plus', or 'times'"))
    }

    fn parse_expr(&mut self) -> Result<EvalNatExpExpr, CheckError> {
        self.parse_plus_expr()
    }

    fn parse_plus_expr(&mut self) -> Result<EvalNatExpExpr, CheckError> {
        let mut expr = self.parse_times_expr()?;
        while self.consume_plus_symbol() {
            let right = self.parse_times_expr()?;
            expr = EvalNatExpExpr::Plus(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_times_expr(&mut self) -> Result<EvalNatExpExpr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_atom_expr()?;
            expr = EvalNatExpExpr::Times(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_atom_expr(&mut self) -> Result<EvalNatExpExpr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        let nat = self.parse_nat_term()?;
        Ok(EvalNatExpExpr::Nat(nat))
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

    fn parse_subderivations(&mut self) -> Result<Vec<EvalNatExpDerivation>, CheckError> {
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

    fn consume_trailing_semicolons(&mut self) {
        while self.consume_semicolon() {}
    }

    fn consume_evalto(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::EvalTo))
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
    use super::{parse_judgment_source, parse_source};
    use crate::games::eval_nat_exp::syntax::{EvalNatExpExpr, EvalNatExpJudgment, NatTerm};

    #[test]
    fn parses_fixture_015() {
        let source = include_str!("../../../copl/015.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "E-Plus");
        assert_eq!(parsed.subderivations.len(), 3);
        assert_eq!(
            parsed.judgment,
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Plus(
                    Box::new(EvalNatExpExpr::Nat(NatTerm::Z)),
                    Box::new(EvalNatExpExpr::Nat(NatTerm::S(Box::new(NatTerm::S(
                        Box::new(NatTerm::Z),
                    ))))),
                ),
                value: NatTerm::S(Box::new(NatTerm::S(Box::new(NatTerm::Z)))),
            }
        );
    }

    #[test]
    fn parses_fixtures_016_to_020() {
        for source in [
            include_str!("../../../copl/016.copl"),
            include_str!("../../../copl/017.copl"),
            include_str!("../../../copl/018.copl"),
            include_str!("../../../copl/019.copl"),
            include_str!("../../../copl/020.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "Z evalto Z by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "E-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
Z + S(Z) evalto S(Z) by E-Plus {
  Z evalto Z by E-Const {};
  S(Z) evalto S(Z) by E-Const {};
  Z plus S(Z) is S(Z) by P-Zero {}
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed =
            parse_judgment_source("S(Z) * (S(Z) + Z) evalto S(Z)").expect("judgment should parse");
        assert_eq!(
            parsed,
            EvalNatExpJudgment::EvalTo {
                expr: EvalNatExpExpr::Times(
                    Box::new(EvalNatExpExpr::Nat(NatTerm::S(Box::new(NatTerm::Z)))),
                    Box::new(EvalNatExpExpr::Plus(
                        Box::new(EvalNatExpExpr::Nat(NatTerm::S(Box::new(NatTerm::Z)))),
                        Box::new(EvalNatExpExpr::Nat(NatTerm::Z)),
                    )),
                ),
                value: NatTerm::S(Box::new(NatTerm::Z)),
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("Z evalto Z by E-Const {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
