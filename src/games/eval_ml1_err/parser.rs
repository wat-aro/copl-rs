use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalML1ErrBinOp, EvalML1ErrDerivation, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
};

pub fn parse_source(source: &str) -> Result<EvalML1ErrDerivation, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::parse("input is empty"));
    }

    let tokens = tokenize(source)?;
    let mut parser = Parser::new(tokens);
    let derivation = parser.parse_derivation()?;
    parser.expect_eof()?;
    Ok(derivation)
}

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalML1ErrJudgment, CheckError> {
    if source.trim().is_empty() {
        return Err(CheckError::parse("input is empty"));
    }

    let tokens = tokenize(source)?;
    let mut parser = Parser::new(tokens);
    let judgment = parser.parse_judgment()?;
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

    fn parse_derivation(&mut self) -> Result<EvalML1ErrDerivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalML1ErrDerivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalML1ErrJudgment, CheckError> {
        let expr = self.parse_expr()?;
        if self.consume_evalto() {
            let value = self.parse_value()?;
            return Ok(EvalML1ErrJudgment::EvalTo { expr, value });
        }

        if let EvalML1ErrExpr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML1ErrJudgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML1ErrJudgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML1ErrJudgment::TimesIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_less() {
                self.expect_keyword_than()?;
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_bool_literal()?;
                return Ok(EvalML1ErrJudgment::LessThanIs {
                    left,
                    right,
                    result,
                });
            }
        }

        Err(self.error_here("expected 'evalto', 'plus', 'minus', 'times', or 'less than'"))
    }

    fn parse_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalML1ErrExpr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }
        self.parse_lt_expr()
    }

    fn parse_lt_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalML1ErrExpr::BinOp {
                op: EvalML1ErrBinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_atom_expr()?;
            expr = EvalML1ErrExpr::BinOp {
                op: EvalML1ErrBinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_atom_expr(&mut self) -> Result<EvalML1ErrExpr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML1ErrExpr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML1ErrExpr::Bool(value));
        }

        if self.at_if() {
            return self.parse_if_expr();
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalML1ErrValue, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML1ErrValue::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML1ErrValue::Bool(value));
        }
        if self.consume_error_literal() {
            return Ok(EvalML1ErrValue::Error);
        }
        Err(self.error_here("expected value"))
    }

    fn parse_int_literal(&mut self) -> Result<i64, CheckError> {
        self.consume_int_literal()
            .ok_or_else(|| self.error_here("expected integer literal"))
    }

    fn parse_bool_literal(&mut self) -> Result<bool, CheckError> {
        self.consume_bool_literal()
            .ok_or_else(|| self.error_here("expected boolean literal"))
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

    fn parse_subderivations(&mut self) -> Result<Vec<EvalML1ErrDerivation>, CheckError> {
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

    fn expect_keyword_then(&mut self) -> Result<(), CheckError> {
        if self.consume_then() {
            Ok(())
        } else {
            Err(self.error_here("expected 'then'"))
        }
    }

    fn expect_keyword_else(&mut self) -> Result<(), CheckError> {
        if self.consume_else() {
            Ok(())
        } else {
            Err(self.error_here("expected 'else'"))
        }
    }

    fn expect_keyword_than(&mut self) -> Result<(), CheckError> {
        if self.consume_than() {
            Ok(())
        } else {
            Err(self.error_here("expected 'than'"))
        }
    }

    fn expect_keyword_is(&mut self) -> Result<(), CheckError> {
        if self.consume_is() {
            Ok(())
        } else {
            Err(self.error_here("expected 'is'"))
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

    fn consume_evalto(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::EvalTo))
    }

    fn consume_plus_word(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::PlusWord))
    }

    fn consume_minus_word(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::MinusWord))
    }

    fn consume_times_word(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::TimesWord))
    }

    fn consume_less(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Less))
    }

    fn consume_than(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Than))
    }

    fn consume_plus_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::PlusSymbol))
    }

    fn consume_minus_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::MinusSymbol))
    }

    fn consume_times_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::TimesSymbol))
    }

    fn consume_lt_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LtSymbol))
    }

    fn consume_is(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Is))
    }

    fn consume_by(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::By))
    }

    fn consume_keyword_if(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::If))
    }

    fn consume_then(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Then))
    }

    fn consume_else(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Else))
    }

    fn at_if(&self) -> bool {
        matches!(self.peek().kind, TokenKind::If)
    }

    fn consume_int_literal(&mut self) -> Option<i64> {
        let TokenKind::Int(value) = self.peek().kind else {
            return None;
        };
        self.bump();
        Some(value)
    }

    fn consume_bool_literal(&mut self) -> Option<bool> {
        if self.consume_true() {
            return Some(true);
        }
        if self.consume_false() {
            return Some(false);
        }
        None
    }

    fn consume_true(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::True))
    }

    fn consume_false(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::False))
    }

    fn consume_error_literal(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Error))
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
    use crate::games::eval_ml1_err::syntax::{
        EvalML1ErrBinOp, EvalML1ErrExpr, EvalML1ErrJudgment, EvalML1ErrValue,
    };

    #[test]
    fn parses_fixture_031() {
        let source = include_str!("../../../copl/031.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "E-PlusErrorL");
        assert_eq!(parsed.subderivations.len(), 1);
        assert_eq!(
            parsed.judgment,
            EvalML1ErrJudgment::EvalTo {
                expr: EvalML1ErrExpr::BinOp {
                    op: EvalML1ErrBinOp::Plus,
                    left: Box::new(EvalML1ErrExpr::BinOp {
                        op: EvalML1ErrBinOp::Plus,
                        left: Box::new(EvalML1ErrExpr::Int(1)),
                        right: Box::new(EvalML1ErrExpr::Bool(true)),
                    }),
                    right: Box::new(EvalML1ErrExpr::Int(2)),
                },
                value: EvalML1ErrValue::Error,
            }
        );
    }

    #[test]
    fn parses_fixtures_032_and_033() {
        for source in [
            include_str!("../../../copl/032.copl"),
            include_str!("../../../copl/033.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "3 evalto 3 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "E-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
3 + 5 evalto 8 by E-Plus {
  3 evalto 3 by E-Int {};
  5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {}
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
        let parsed = parse_judgment_source("if 3 < 4 then 1 < true else 3 - false evalto error")
            .expect("judgment should parse");
        assert_eq!(
            parsed,
            EvalML1ErrJudgment::EvalTo {
                expr: EvalML1ErrExpr::If {
                    condition: Box::new(EvalML1ErrExpr::BinOp {
                        op: EvalML1ErrBinOp::Lt,
                        left: Box::new(EvalML1ErrExpr::Int(3)),
                        right: Box::new(EvalML1ErrExpr::Int(4)),
                    }),
                    then_branch: Box::new(EvalML1ErrExpr::BinOp {
                        op: EvalML1ErrBinOp::Lt,
                        left: Box::new(EvalML1ErrExpr::Int(1)),
                        right: Box::new(EvalML1ErrExpr::Bool(true)),
                    }),
                    else_branch: Box::new(EvalML1ErrExpr::BinOp {
                        op: EvalML1ErrBinOp::Minus,
                        left: Box::new(EvalML1ErrExpr::Int(3)),
                        right: Box::new(EvalML1ErrExpr::Bool(false)),
                    }),
                },
                value: EvalML1ErrValue::Error,
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("3 evalto 3 by E-Int {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
