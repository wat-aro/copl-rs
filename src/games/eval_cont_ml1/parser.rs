use crate::core::CheckError;

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalContML1BinOp, EvalContML1ContFrame, EvalContML1Continuation, EvalContML1Derivation,
    EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
};

pub fn parse_source(source: &str) -> Result<EvalContML1Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalContML1Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalContML1Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalContML1Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalContML1Judgment, CheckError> {
        let checkpoint = self.index;
        if let Ok(input) = self.parse_value() {
            if self.consume_cont_eval_arrow() {
                let continuation = self.parse_continuation_body()?;
                self.expect_evalto()?;
                let value = self.parse_value()?;
                return Ok(EvalContML1Judgment::ContEvalTo {
                    input,
                    continuation,
                    value,
                });
            }
        }
        self.index = checkpoint;

        let expr = self.parse_expr()?;
        if self.consume_cont_arrow() {
            let continuation = self.parse_continuation_body()?;
            self.expect_evalto()?;
            let value = self.parse_value()?;
            return Ok(EvalContML1Judgment::EvalTo {
                expr,
                continuation,
                value,
                has_continuation: true,
            });
        }

        if self.consume_evalto() {
            let value = self.parse_value()?;
            return Ok(EvalContML1Judgment::EvalTo {
                expr,
                continuation: EvalContML1Continuation::implicit_hole(),
                value,
                has_continuation: false,
            });
        }

        if let EvalContML1Expr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML1Judgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML1Judgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML1Judgment::TimesIs {
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
                return Ok(EvalContML1Judgment::LessThanIs {
                    left,
                    right,
                    result,
                });
            }
        }

        Err(self.error_here(
            "expected judgment ('evalto', '>> ... evalto', '=> ... evalto', or arithmetic judgment)",
        ))
    }

    fn parse_continuation_body(&mut self) -> Result<EvalContML1Continuation, CheckError> {
        if self.consume_underscore() {
            return Ok(EvalContML1Continuation::hole());
        }

        let mut frames = vec![self.parse_continuation_frame()?];
        let mut explicit_ret = false;
        while self.consume_cont_arrow() {
            if self.consume_underscore() {
                explicit_ret = true;
                break;
            }
            frames.push(self.parse_continuation_frame()?);
        }

        Ok(EvalContML1Continuation {
            frames,
            explicit_ret,
        })
    }

    fn parse_continuation_frame(&mut self) -> Result<EvalContML1ContFrame, CheckError> {
        self.expect_lbrace()?;

        if self.consume_keyword_if() {
            self.expect_underscore()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            self.expect_rbrace()?;
            return Ok(EvalContML1ContFrame::If {
                then_branch,
                else_branch,
            });
        }

        if self.consume_underscore() {
            let op = self.parse_binop_symbol()?;
            let right = self.parse_expr()?;
            self.expect_rbrace()?;
            return Ok(EvalContML1ContFrame::EvalR { op, right });
        }

        let left = self.parse_int_literal()?;
        let op = self.parse_binop_symbol()?;
        self.expect_underscore()?;
        self.expect_rbrace()?;

        let frame = match op {
            EvalContML1BinOp::Plus => EvalContML1ContFrame::Plus { left },
            EvalContML1BinOp::Minus => EvalContML1ContFrame::Minus { left },
            EvalContML1BinOp::Times => EvalContML1ContFrame::Times { left },
            EvalContML1BinOp::Lt => EvalContML1ContFrame::Lt { left },
        };
        Ok(frame)
    }

    fn parse_binop_symbol(&mut self) -> Result<EvalContML1BinOp, CheckError> {
        if self.consume_plus_symbol() {
            return Ok(EvalContML1BinOp::Plus);
        }
        if self.consume_minus_symbol() {
            return Ok(EvalContML1BinOp::Minus);
        }
        if self.consume_times_symbol() {
            return Ok(EvalContML1BinOp::Times);
        }
        if self.consume_lt_symbol() {
            return Ok(EvalContML1BinOp::Lt);
        }
        Err(self.error_here("expected binary operator symbol (+, -, *, <)"))
    }

    fn parse_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalContML1Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }
        self.parse_lt_expr()
    }

    fn parse_lt_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalContML1Expr::BinOp {
                op: EvalContML1BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalContML1Expr::BinOp {
                    op: EvalContML1BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalContML1Expr::BinOp {
                    op: EvalContML1BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_atom_expr()?;
            expr = EvalContML1Expr::BinOp {
                op: EvalContML1BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_atom_expr(&mut self) -> Result<EvalContML1Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalContML1Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalContML1Expr::Bool(value));
        }

        if self.at_if() {
            return self.parse_if_expr();
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalContML1Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalContML1Value::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalContML1Value::Bool(value));
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
            return Err(self.error_here("expected rule name after 'by'"));
        };
        let name = name.clone();
        self.bump();
        Ok(name)
    }

    fn parse_subderivations(&mut self) -> Result<Vec<EvalContML1Derivation>, CheckError> {
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

    fn expect_evalto(&mut self) -> Result<(), CheckError> {
        if self.consume_evalto() {
            Ok(())
        } else {
            Err(self.error_here("expected 'evalto'"))
        }
    }

    fn expect_underscore(&mut self) -> Result<(), CheckError> {
        if self.consume_underscore() {
            Ok(())
        } else {
            Err(self.error_here("expected '_'"))
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

    fn consume_cont_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ContArrow))
    }

    fn consume_cont_eval_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ContEvalArrow))
    }

    fn consume_underscore(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Underscore))
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

    fn consume_int_literal(&mut self) -> Option<i64> {
        if let TokenKind::Int(value) = self.peek().kind {
            self.bump();
            Some(value)
        } else {
            None
        }
    }

    fn consume_bool_literal(&mut self) -> Option<bool> {
        match self.peek().kind {
            TokenKind::True => {
                self.bump();
                Some(true)
            }
            TokenKind::False => {
                self.bump();
                Some(false)
            }
            _ => None,
        }
    }

    fn at_if(&self) -> bool {
        matches!(self.peek().kind, TokenKind::If)
    }

    fn at_rbrace(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RBrace)
    }

    fn consume_if(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn bump(&mut self) {
        if self.index + 1 < self.tokens.len() {
            self.index += 1;
        }
    }

    fn error_here(&self, message: &str) -> CheckError {
        let span = self.peek().span.clone();
        CheckError::parse(message.to_string()).with_span(span)
    }
}

#[cfg(test)]
mod tests {
    use crate::core::CheckErrorKind;

    use super::{parse_judgment_source, parse_source};
    use crate::games::eval_cont_ml1::syntax::{
        EvalContML1ContFrame, EvalContML1Expr, EvalContML1Judgment, EvalContML1Value,
    };

    #[test]
    fn parses_eval_judgment_with_continuation_chain() {
        let source = "3 >> {_ + 5} >> {_ * 2} evalto 16 by E-Int { 3 => {_ + 5} >> {_ * 2} evalto 16 by C-EvalR { 5 >> {3 + _} >> {_ * 2} evalto 16 by E-Int { 5 => {3 + _} >> {_ * 2} evalto 16 by C-Plus { 3 plus 5 is 8 by B-Plus {}; 8 => {_ * 2} evalto 16 by C-EvalR { 2 >> {8 * _} evalto 16 by E-Int { 2 => {8 * _} evalto 16 by C-Times { 8 times 2 is 16 by B-Times {}; 16 => _ evalto 16 by C-Ret {} } } } } } } }";
        let parsed = parse_source(source).expect("parse should succeed");

        let EvalContML1Judgment::EvalTo {
            continuation,
            has_continuation,
            ..
        } = parsed.judgment
        else {
            panic!("expected eval judgment");
        };

        assert!(has_continuation);
        assert_eq!(continuation.frames.len(), 2);
        assert!(matches!(
            continuation.frames[0],
            EvalContML1ContFrame::EvalR { .. }
        ));
    }

    #[test]
    fn parses_continuation_application_judgment() {
        let source = "5 => {3 + _} evalto 8 by C-Plus { 3 plus 5 is 8 by B-Plus {}; 8 => _ evalto 8 by C-Ret {} }";
        let parsed = parse_source(source).expect("parse should succeed");

        let EvalContML1Judgment::ContEvalTo {
            input,
            continuation,
            value,
        } = parsed.judgment
        else {
            panic!("expected continuation judgment");
        };

        assert_eq!(input, EvalContML1Value::Int(5));
        assert_eq!(value, EvalContML1Value::Int(8));
        assert_eq!(continuation.frames.len(), 1);
    }

    #[test]
    fn parses_eval_judgment_without_explicit_continuation() {
        let source = "3 + 5 evalto 8 by E-BinOp { 3 >> {_ + 5} evalto 8 by E-Int { 3 => {_ + 5} evalto 8 by C-EvalR { 5 >> {3 + _} evalto 8 by E-Int { 5 => {3 + _} evalto 8 by C-Plus { 3 plus 5 is 8 by B-Plus {}; 8 => _ evalto 8 by C-Ret {} } } } } }";
        let parsed = parse_source(source).expect("parse should succeed");

        let EvalContML1Judgment::EvalTo {
            expr,
            continuation,
            has_continuation,
            ..
        } = parsed.judgment
        else {
            panic!("expected eval judgment");
        };

        assert_eq!(continuation.frames.len(), 0);
        assert!(!has_continuation);
        assert!(matches!(expr, EvalContML1Expr::BinOp { .. }));
    }

    #[test]
    fn rejects_invalid_continuation_frame() {
        let source = "3 >> {if 1 then 2 else 3} evalto 3 by E-Int { 3 => _ evalto 3 by C-Ret {} }";
        let err = parse_source(source).expect_err("parse should fail");
        assert_eq!(err.kind(), CheckErrorKind::Parse);
        assert!(err.message().contains("expected '_'"));
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source("if 4 < 5 then 2 + 3 else 8 * 8 evalto 5")
            .expect("judgment should parse");

        assert_eq!(
            parsed,
            EvalContML1Judgment::EvalTo {
                expr: EvalContML1Expr::If {
                    condition: Box::new(EvalContML1Expr::BinOp {
                        op: crate::games::eval_cont_ml1::syntax::EvalContML1BinOp::Lt,
                        left: Box::new(EvalContML1Expr::Int(4)),
                        right: Box::new(EvalContML1Expr::Int(5)),
                    }),
                    then_branch: Box::new(EvalContML1Expr::BinOp {
                        op: crate::games::eval_cont_ml1::syntax::EvalContML1BinOp::Plus,
                        left: Box::new(EvalContML1Expr::Int(2)),
                        right: Box::new(EvalContML1Expr::Int(3)),
                    }),
                    else_branch: Box::new(EvalContML1Expr::BinOp {
                        op: crate::games::eval_cont_ml1::syntax::EvalContML1BinOp::Times,
                        left: Box::new(EvalContML1Expr::Int(8)),
                        right: Box::new(EvalContML1Expr::Int(8)),
                    }),
                },
                continuation:
                    crate::games::eval_cont_ml1::syntax::EvalContML1Continuation::implicit_hole(),
                value: EvalContML1Value::Int(5),
                has_continuation: false,
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("3 evalto 3 by E-Int { 3 => _ evalto 3 by C-Ret {} }")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
