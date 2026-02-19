use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalML2BinOp, EvalML2Binding, EvalML2Derivation, EvalML2Env, EvalML2Expr, EvalML2Judgment,
    EvalML2Value,
};

pub fn parse_source(source: &str) -> Result<EvalML2Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalML2Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalML2Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalML2Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalML2Judgment, CheckError> {
        if self.has_turnstile_before_by() {
            let env = self.parse_env()?;
            let expr = self.parse_expr()?;
            self.expect_evalto()?;
            let value = self.parse_value()?;
            return Ok(EvalML2Judgment::EvalTo { expr, value, env });
        }

        let expr = self.parse_expr()?;
        if let EvalML2Expr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML2Judgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML2Judgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML2Judgment::TimesIs {
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
                return Ok(EvalML2Judgment::LessThanIs {
                    left,
                    right,
                    result,
                });
            }
        }

        Err(self.error_here("expected judgment"))
    }

    fn has_turnstile_before_by(&self) -> bool {
        let mut index = self.index;
        loop {
            match self.tokens.get(index).map(|token| &token.kind) {
                Some(TokenKind::Turnstile) => return true,
                Some(TokenKind::By) | Some(TokenKind::Eof) | None => return false,
                _ => {
                    index += 1;
                }
            }
        }
    }

    fn parse_env(&mut self) -> Result<EvalML2Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(EvalML2Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        self.expect_turnstile()?;
        Ok(EvalML2Env(bindings))
    }

    fn parse_binding(&mut self) -> Result<EvalML2Binding, CheckError> {
        let name = self.parse_identifier()?;
        self.expect_equal()?;
        let value = self.parse_value()?;
        Ok(EvalML2Binding { name, value })
    }

    fn parse_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        if self.consume_keyword_let() {
            let name = self.parse_identifier()?;
            self.expect_equal()?;
            let bound_expr = self.parse_expr()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalML2Expr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalML2Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_lt_expr()
    }

    fn parse_lt_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalML2Expr::BinOp {
                op: EvalML2BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML2Expr::BinOp {
                    op: EvalML2BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML2Expr::BinOp {
                    op: EvalML2BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_atom_expr()?;
            expr = EvalML2Expr::BinOp {
                op: EvalML2BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_atom_expr(&mut self) -> Result<EvalML2Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML2Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML2Expr::Bool(value));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalML2Expr::Var(name));
        }

        if self.at_if() || self.at_let() {
            return self.parse_expr();
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalML2Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML2Value::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML2Value::Bool(value));
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

    fn parse_identifier(&mut self) -> Result<String, CheckError> {
        self.consume_identifier()
            .ok_or_else(|| self.error_here("expected identifier"))
    }

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        self.parse_identifier()
    }

    fn parse_subderivations(&mut self) -> Result<Vec<EvalML2Derivation>, CheckError> {
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

    fn expect_keyword_in(&mut self) -> Result<(), CheckError> {
        if self.consume_in() {
            Ok(())
        } else {
            Err(self.error_here("expected 'in'"))
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

    fn expect_evalto(&mut self) -> Result<(), CheckError> {
        if self.consume_evalto() {
            Ok(())
        } else {
            Err(self.error_here("expected 'evalto'"))
        }
    }

    fn expect_turnstile(&mut self) -> Result<(), CheckError> {
        if self.consume_turnstile() {
            Ok(())
        } else {
            Err(self.error_here("expected '|-'"))
        }
    }

    fn expect_equal(&mut self) -> Result<(), CheckError> {
        if self.consume_equal() {
            Ok(())
        } else {
            Err(self.error_here("expected '='"))
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

    fn consume_keyword_let(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Let))
    }

    fn consume_then(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Then))
    }

    fn consume_else(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Else))
    }

    fn consume_in(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::In))
    }

    fn at_if(&self) -> bool {
        matches!(self.peek().kind, TokenKind::If)
    }

    fn at_let(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Let)
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

    fn consume_identifier(&mut self) -> Option<String> {
        let TokenKind::Identifier(name) = &self.peek().kind else {
            return None;
        };
        let name = name.clone();
        self.bump();
        Some(name)
    }

    fn consume_true(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::True))
    }

    fn consume_false(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::False))
    }

    fn consume_equal(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Equal))
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_turnstile(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Turnstile))
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
    use crate::games::eval_ml2::syntax::{
        EvalML2BinOp, EvalML2Binding, EvalML2Env, EvalML2Expr, EvalML2Judgment, EvalML2Value,
    };

    #[test]
    fn parses_fixture_034() {
        let source = include_str!("../../../copl/034.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "E-Var2");
        assert_eq!(parsed.subderivations.len(), 1);
        assert_eq!(
            parsed.judgment,
            EvalML2Judgment::EvalTo {
                env: EvalML2Env(vec![
                    EvalML2Binding {
                        name: "x".to_string(),
                        value: EvalML2Value::Int(3),
                    },
                    EvalML2Binding {
                        name: "y".to_string(),
                        value: EvalML2Value::Int(2),
                    }
                ]),
                expr: EvalML2Expr::Var("x".to_string()),
                value: EvalML2Value::Int(3),
            }
        );
    }

    #[test]
    fn parses_fixtures_035_to_039() {
        for source in [
            include_str!("../../../copl/035.copl"),
            include_str!("../../../copl/036.copl"),
            include_str!("../../../copl/037.copl"),
            include_str!("../../../copl/038.copl"),
            include_str!("../../../copl/039.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "|- 3 evalto 3 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "E-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
|- let x = 1 + 2 in x * 4 evalto 12 by E-Let {
  |- 1 + 2 evalto 3 by E-Plus {
    |- 1 evalto 1 by E-Int {};
    |- 2 evalto 2 by E-Int {};
    1 plus 2 is 3 by B-Plus {};
  };
  x = 3 |- x * 4 evalto 12 by E-Times {
    x = 3 |- x evalto 3 by E-Var1 {};
    x = 3 |- 4 evalto 4 by E-Int {};
    3 times 4 is 12 by B-Times {};
  };
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }

    #[test]
    fn parses_let_expression_shape() {
        let source = "|- let x = 1 in x + 2 evalto 3 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let EvalML2Judgment::EvalTo { expr, .. } = parsed.judgment else {
            panic!("expected evalto");
        };
        assert_eq!(
            expr,
            EvalML2Expr::Let {
                name: "x".to_string(),
                bound_expr: Box::new(EvalML2Expr::Int(1)),
                body: Box::new(EvalML2Expr::BinOp {
                    op: EvalML2BinOp::Plus,
                    left: Box::new(EvalML2Expr::Var("x".to_string())),
                    right: Box::new(EvalML2Expr::Int(2)),
                }),
            }
        );
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source("|- let x = 1 + 2 in x * 4 evalto 12")
            .expect("judgment should parse");
        assert_eq!(
            parsed,
            EvalML2Judgment::EvalTo {
                env: EvalML2Env::default(),
                expr: EvalML2Expr::Let {
                    name: "x".to_string(),
                    bound_expr: Box::new(EvalML2Expr::BinOp {
                        op: EvalML2BinOp::Plus,
                        left: Box::new(EvalML2Expr::Int(1)),
                        right: Box::new(EvalML2Expr::Int(2)),
                    }),
                    body: Box::new(EvalML2Expr::BinOp {
                        op: EvalML2BinOp::Times,
                        left: Box::new(EvalML2Expr::Var("x".to_string())),
                        right: Box::new(EvalML2Expr::Int(4)),
                    }),
                },
                value: EvalML2Value::Int(12),
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- 3 evalto 3 by E-Int {}")
            .expect_err("derivation input should be rejected");
        assert!(err.message().contains("expected end of input"));
    }
}
