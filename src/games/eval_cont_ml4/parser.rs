use crate::core::CheckError;

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalContML4BinOp, EvalContML4Binding, EvalContML4ContFrame, EvalContML4Continuation,
    EvalContML4Derivation, EvalContML4Env, EvalContML4Expr, EvalContML4Judgment, EvalContML4Value,
};

pub fn parse_source(source: &str) -> Result<EvalContML4Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalContML4Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalContML4Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalContML4Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalContML4Judgment, CheckError> {
        let checkpoint = self.index;
        if let Ok(input) = self.parse_value() {
            if self.consume_cont_eval_arrow() {
                let continuation = self.parse_continuation_body()?;
                self.expect_evalto()?;
                let value = self.parse_value()?;
                return Ok(EvalContML4Judgment::ContEvalTo {
                    input,
                    continuation,
                    value,
                });
            }
        }
        self.index = checkpoint;

        if self.has_turnstile_before_by() {
            let env = self.parse_env()?;
            let expr = self.parse_expr()?;
            if self.consume_cont_arrow() {
                let continuation = self.parse_continuation_body()?;
                self.expect_evalto()?;
                let value = self.parse_value()?;
                return Ok(EvalContML4Judgment::EvalTo {
                    env,
                    expr,
                    continuation,
                    value,
                    has_continuation: true,
                });
            }

            self.expect_evalto()?;
            let value = self.parse_value()?;
            return Ok(EvalContML4Judgment::EvalTo {
                env,
                expr,
                continuation: EvalContML4Continuation::implicit_hole(),
                value,
                has_continuation: false,
            });
        }

        let expr = self.parse_expr()?;
        if let EvalContML4Expr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML4Judgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML4Judgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalContML4Judgment::TimesIs {
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
                return Ok(EvalContML4Judgment::LessThanIs {
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

    fn parse_env(&mut self) -> Result<EvalContML4Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(EvalContML4Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        self.expect_turnstile()?;
        Ok(EvalContML4Env(bindings))
    }

    fn parse_env_in_parens(&mut self) -> Result<EvalContML4Env, CheckError> {
        if self.at_rparen() {
            return Ok(EvalContML4Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        Ok(EvalContML4Env(bindings))
    }

    fn parse_binding(&mut self) -> Result<EvalContML4Binding, CheckError> {
        let name = self.parse_identifier()?;
        self.expect_equal()?;
        let value = self.parse_value()?;
        Ok(EvalContML4Binding { name, value })
    }

    fn parse_continuation_body(&mut self) -> Result<EvalContML4Continuation, CheckError> {
        if self.consume_underscore() {
            return Ok(EvalContML4Continuation::hole());
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

        Ok(EvalContML4Continuation {
            frames,
            explicit_ret,
        })
    }

    fn parse_continuation_frame(&mut self) -> Result<EvalContML4ContFrame, CheckError> {
        self.expect_lbrace()?;

        let frame = if self.has_turnstile_before_rbrace() {
            let env = self.parse_env()?;
            self.parse_env_prefixed_frame(Some(env))?
        } else {
            self.parse_non_env_frame()?
        };

        self.expect_rbrace()?;
        Ok(frame)
    }

    fn has_turnstile_before_rbrace(&self) -> bool {
        let mut index = self.index;
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;

        loop {
            let Some(token) = self.tokens.get(index) else {
                return false;
            };
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => paren = paren.saturating_sub(1),
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => bracket = bracket.saturating_sub(1),
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => {
                    if brace == 0 && paren == 0 && bracket == 0 {
                        return false;
                    }
                    brace = brace.saturating_sub(1);
                }
                TokenKind::Turnstile if paren == 0 && bracket == 0 && brace == 0 => {
                    return true;
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            index += 1;
        }
    }

    fn parse_env_prefixed_frame(
        &mut self,
        env: Option<EvalContML4Env>,
    ) -> Result<EvalContML4ContFrame, CheckError> {
        if self.consume_keyword_if() {
            self.expect_underscore()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::If {
                env,
                then_branch,
                else_branch,
            });
        }

        if self.consume_keyword_let() {
            let name = self.parse_identifier()?;
            self.expect_equal()?;
            self.expect_underscore()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::LetBody { env, name, body });
        }

        if self.consume_keyword_match() {
            self.expect_underscore()?;
            self.expect_keyword_with()?;
            self.expect_lbracket()?;
            self.expect_rbracket()?;
            self.expect_arrow()?;
            let nil_case = self.parse_expr()?;
            self.expect_bar()?;
            let head_name = self.parse_identifier()?;
            self.expect_cons_symbol()?;
            let tail_name = self.parse_identifier()?;
            self.expect_arrow()?;
            let cons_case = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::Match {
                env,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            });
        }

        self.expect_underscore()?;
        if let Some(op) = self.consume_binop_symbol() {
            let right = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::EvalR { env, op, right });
        }
        if self.consume_cons_symbol() {
            let tail_expr = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::EvalConsR { env, tail_expr });
        }

        let arg = self.parse_expr()?;
        Ok(EvalContML4ContFrame::EvalArg { env, arg })
    }

    fn parse_non_env_frame(&mut self) -> Result<EvalContML4ContFrame, CheckError> {
        if self.consume_keyword_if() {
            self.expect_underscore()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::If {
                env: None,
                then_branch,
                else_branch,
            });
        }

        if self.consume_keyword_let() {
            let name = self.parse_identifier()?;
            self.expect_equal()?;
            self.expect_underscore()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::LetBody {
                env: None,
                name,
                body,
            });
        }

        if self.consume_keyword_match() {
            self.expect_underscore()?;
            self.expect_keyword_with()?;
            self.expect_lbracket()?;
            self.expect_rbracket()?;
            self.expect_arrow()?;
            let nil_case = self.parse_expr()?;
            self.expect_bar()?;
            let head_name = self.parse_identifier()?;
            self.expect_cons_symbol()?;
            let tail_name = self.parse_identifier()?;
            self.expect_arrow()?;
            let cons_case = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::Match {
                env: None,
                nil_case,
                head_name,
                tail_name,
                cons_case,
            });
        }

        if self.consume_underscore() {
            if let Some(op) = self.consume_binop_symbol() {
                let right = self.parse_expr()?;
                return Ok(EvalContML4ContFrame::EvalR {
                    env: None,
                    op,
                    right,
                });
            }
            if self.consume_cons_symbol() {
                let tail_expr = self.parse_expr()?;
                return Ok(EvalContML4ContFrame::EvalConsR {
                    env: None,
                    tail_expr,
                });
            }
            let arg = self.parse_expr()?;
            return Ok(EvalContML4ContFrame::EvalArg { env: None, arg });
        }

        let value = self.parse_value()?;
        if let EvalContML4Value::Int(left) = value {
            if self.consume_plus_symbol() {
                self.expect_underscore()?;
                return Ok(EvalContML4ContFrame::Plus { left });
            }
            if self.consume_minus_symbol() {
                self.expect_underscore()?;
                return Ok(EvalContML4ContFrame::Minus { left });
            }
            if self.consume_times_symbol() {
                self.expect_underscore()?;
                return Ok(EvalContML4ContFrame::Times { left });
            }
            if self.consume_lt_symbol() {
                self.expect_underscore()?;
                return Ok(EvalContML4ContFrame::Lt { left });
            }

            if self.consume_cons_symbol() {
                self.expect_underscore()?;
                return Ok(EvalContML4ContFrame::Cons {
                    head: EvalContML4Value::Int(left),
                });
            }

            if self.consume_underscore() {
                return Ok(EvalContML4ContFrame::EvalFun {
                    func: EvalContML4Value::Int(left),
                });
            }

            return Err(self.error_here("expected continuation frame suffix"));
        }

        if self.consume_cons_symbol() {
            self.expect_underscore()?;
            return Ok(EvalContML4ContFrame::Cons { head: value });
        }
        if self.consume_underscore() {
            return Ok(EvalContML4ContFrame::EvalFun { func: value });
        }

        Err(self.error_here("expected continuation frame suffix"))
    }

    fn consume_binop_symbol(&mut self) -> Option<EvalContML4BinOp> {
        if self.consume_plus_symbol() {
            return Some(EvalContML4BinOp::Plus);
        }
        if self.consume_minus_symbol() {
            return Some(EvalContML4BinOp::Minus);
        }
        if self.consume_times_symbol() {
            return Some(EvalContML4BinOp::Times);
        }
        if self.consume_lt_symbol() {
            return Some(EvalContML4BinOp::Lt);
        }
        None
    }

    fn parse_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        if self.consume_keyword_let_cc() {
            let name = self.parse_identifier()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalContML4Expr::LetCc {
                name,
                body: Box::new(body),
            });
        }

        if self.consume_keyword_let() {
            if self.consume_keyword_rec() {
                let name = self.parse_identifier()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                let param = self.parse_identifier()?;
                self.expect_arrow()?;
                let fun_body = self.parse_expr()?;
                self.expect_keyword_in()?;
                let body = self.parse_expr()?;
                return Ok(EvalContML4Expr::LetRec {
                    name,
                    param,
                    fun_body: Box::new(fun_body),
                    body: Box::new(body),
                });
            }

            let name = self.parse_identifier()?;
            self.expect_equal()?;
            let bound_expr = self.parse_expr()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalContML4Expr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalContML4Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_fun_expr()
    }

    fn parse_fun_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        if self.consume_keyword_fun() {
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            return Ok(EvalContML4Expr::Fun {
                param,
                body: Box::new(body),
            });
        }

        self.parse_match_expr()
    }

    fn parse_match_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        if self.consume_keyword_match() {
            let scrutinee = self.parse_expr()?;
            self.expect_keyword_with()?;
            self.expect_lbracket()?;
            self.expect_rbracket()?;
            self.expect_arrow()?;
            let nil_case = self.parse_expr()?;
            self.expect_bar()?;
            let head_name = self.parse_identifier()?;
            self.expect_cons_symbol()?;
            let tail_name = self.parse_identifier()?;
            self.expect_arrow()?;
            let cons_case = self.parse_expr()?;
            return Ok(EvalContML4Expr::Match {
                scrutinee: Box::new(scrutinee),
                nil_case: Box::new(nil_case),
                head_name,
                tail_name,
                cons_case: Box::new(cons_case),
            });
        }

        self.parse_cons_expr()
    }

    fn parse_cons_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        let head = self.parse_lt_expr()?;
        if self.consume_cons_symbol() {
            let tail = self.parse_cons_expr()?;
            Ok(EvalContML4Expr::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_lt_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalContML4Expr::BinOp {
                op: EvalContML4BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalContML4Expr::BinOp {
                    op: EvalContML4BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalContML4Expr::BinOp {
                    op: EvalContML4BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        let mut expr = self.parse_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_app_expr()?;
            expr = EvalContML4Expr::BinOp {
                op: EvalContML4BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_app_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.starts_atom_expr() {
            let arg = self.parse_atom_expr()?;
            expr = EvalContML4Expr::App {
                func: Box::new(expr),
                arg: Box::new(arg),
            };
        }
        Ok(expr)
    }

    fn starts_atom_expr(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Identifier(_)
        )
    }

    fn parse_atom_expr(&mut self) -> Result<EvalContML4Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if self.consume_lbracket() {
            self.expect_rbracket()?;
            return Ok(EvalContML4Expr::Nil);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalContML4Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalContML4Expr::Bool(value));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalContML4Expr::Var(name));
        }

        if self.starts_low_precedence_expr() {
            return self.parse_let_expr();
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalContML4Value, CheckError> {
        self.parse_cons_value()
    }

    fn parse_cons_value(&mut self) -> Result<EvalContML4Value, CheckError> {
        let head = self.parse_atomic_value()?;
        if self.at_cons_symbol() && self.next_token_starts_value() {
            self.bump();
            let tail = self.parse_cons_value()?;
            Ok(EvalContML4Value::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_atomic_value(&mut self) -> Result<EvalContML4Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalContML4Value::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalContML4Value::Bool(value));
        }
        if self.consume_lbracket() {
            if self.consume_rbracket() {
                return Ok(EvalContML4Value::Nil);
            }
            let continuation = self.parse_continuation_body()?;
            self.expect_rbracket()?;
            return Ok(EvalContML4Value::Continuation(continuation));
        }

        if self.is_closure_value_start() {
            self.consume_lparen();
            let env = self.parse_env_in_parens()?;
            self.expect_rparen()?;
            self.expect_lbracket()?;

            if self.consume_keyword_rec() {
                let name = self.parse_identifier()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                let param = self.parse_identifier()?;
                self.expect_arrow()?;
                let body = self.parse_expr()?;
                self.expect_rbracket()?;
                return Ok(EvalContML4Value::RecClosure {
                    env,
                    name,
                    param,
                    body,
                });
            }

            self.expect_keyword_fun()?;
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            self.expect_rbracket()?;
            return Ok(EvalContML4Value::Closure { env, param, body });
        }

        if self.consume_lparen() {
            let value = self.parse_cons_value()?;
            self.expect_rparen()?;
            return Ok(value);
        }

        Err(self.error_here("expected value"))
    }

    fn is_closure_value_start(&self) -> bool {
        if !matches!(self.peek().kind, TokenKind::LParen) {
            return false;
        }

        let mut depth = 0usize;
        for index in self.index..self.tokens.len() {
            match self.tokens[index].kind {
                TokenKind::LParen => {
                    depth += 1;
                }
                TokenKind::RParen => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return matches!(
                            self.tokens.get(index + 1).map(|token| &token.kind),
                            Some(TokenKind::LBracket)
                        );
                    }
                }
                _ => {}
            }
        }

        false
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

    fn parse_subderivations(&mut self) -> Result<Vec<EvalContML4Derivation>, CheckError> {
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

    fn expect_keyword_fun(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_fun() {
            Ok(())
        } else {
            Err(self.error_here("expected 'fun'"))
        }
    }

    fn expect_keyword_with(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_with() {
            Ok(())
        } else {
            Err(self.error_here("expected 'with'"))
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

    fn expect_arrow(&mut self) -> Result<(), CheckError> {
        if self.consume_arrow() {
            Ok(())
        } else {
            Err(self.error_here("expected '->'"))
        }
    }

    fn expect_bar(&mut self) -> Result<(), CheckError> {
        if self.consume_bar() {
            Ok(())
        } else {
            Err(self.error_here("expected '|'"))
        }
    }

    fn expect_cons_symbol(&mut self) -> Result<(), CheckError> {
        if self.consume_cons_symbol() {
            Ok(())
        } else {
            Err(self.error_here("expected '::'"))
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

    fn expect_lbracket(&mut self) -> Result<(), CheckError> {
        if self.consume_lbracket() {
            Ok(())
        } else {
            Err(self.error_here("expected '['"))
        }
    }

    fn expect_rbracket(&mut self) -> Result<(), CheckError> {
        if self.consume_rbracket() {
            Ok(())
        } else {
            Err(self.error_here("expected ']'"))
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

    fn consume_keyword_let_cc(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LetCc))
    }

    fn consume_keyword_rec(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Rec))
    }

    fn consume_keyword_fun(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Fun))
    }

    fn consume_keyword_match(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Match))
    }

    fn consume_keyword_with(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::With))
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

    fn consume_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Arrow))
    }

    fn consume_bar(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Bar))
    }

    fn consume_cons_symbol(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ConsSymbol))
    }

    fn consume_turnstile(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Turnstile))
    }

    fn consume_cont_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ContArrow))
    }

    fn consume_cont_eval_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ContEvalArrow))
    }

    fn consume_equal(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Equal))
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_underscore(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Underscore))
    }

    fn consume_lparen(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LParen))
    }

    fn consume_rparen(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::RParen))
    }

    fn consume_lbracket(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::LBracket))
    }

    fn consume_rbracket(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::RBracket))
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

    fn consume_identifier(&mut self) -> Option<String> {
        if let TokenKind::Identifier(name) = &self.peek().kind {
            let name = name.clone();
            self.bump();
            Some(name)
        } else {
            None
        }
    }

    fn at_rbrace(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RBrace)
    }

    fn at_cons_symbol(&self) -> bool {
        matches!(self.peek().kind, TokenKind::ConsSymbol)
    }

    fn at_rparen(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RParen)
    }

    fn next_token_starts_value(&self) -> bool {
        matches!(
            self.tokens.get(self.index + 1).map(|token| &token.kind),
            Some(
                TokenKind::Int(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::LBracket
                    | TokenKind::LParen
            )
        )
    }

    fn starts_low_precedence_expr(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::Let | TokenKind::LetCc | TokenKind::If | TokenKind::Fun | TokenKind::Match
        )
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
    use crate::games::eval_cont_ml4::syntax::{
        EvalContML4BinOp, EvalContML4ContFrame, EvalContML4Continuation, EvalContML4Expr,
        EvalContML4Judgment, EvalContML4Value,
    };

    #[test]
    fn parses_fixture_130() {
        let source = include_str!("../../../copl/130.copl");
        let parsed = parse_source(source).expect("parse should succeed");
        assert_eq!(parsed.rule_name, "E-Let");
    }

    #[test]
    fn parses_fixture_140() {
        let source = include_str!("../../../copl/140.copl");
        let parsed = parse_source(source).expect("parse should succeed");
        assert_eq!(parsed.rule_name, "E-Let");
    }

    #[test]
    fn parses_continuation_application_judgment() {
        let source = "2 => {3 + _} >> _ evalto 5 by C-Plus { 3 plus 2 is 5 by B-Plus {}; 5 => _ evalto 5 by C-Ret {} }";
        let parsed = parse_source(source).expect("parse should succeed");
        let EvalContML4Judgment::ContEvalTo { continuation, .. } = parsed.judgment else {
            panic!("expected continuation judgment");
        };
        assert_eq!(continuation.frames.len(), 1);
        assert!(matches!(
            continuation.frames[0],
            EvalContML4ContFrame::Plus { .. }
        ));
    }

    #[test]
    fn rejects_invalid_env_frame_shape() {
        let source =
            "|- 1 >> { |- if 1 then 2 else 3 } evalto 1 by E-Int { 1 => _ evalto 1 by C-Ret {} }";
        let err = parse_source(source).expect_err("parse should fail");
        assert_eq!(err.kind(), CheckErrorKind::Parse);
        assert!(err.message().contains("expected '_'"));
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source("|- if 4 < 5 then 2 + 3 else 8 * 8 evalto 5")
            .expect("judgment should parse");

        assert_eq!(
            parsed,
            EvalContML4Judgment::EvalTo {
                env: crate::games::eval_cont_ml4::syntax::EvalContML4Env::default(),
                expr: EvalContML4Expr::If {
                    condition: Box::new(EvalContML4Expr::BinOp {
                        op: EvalContML4BinOp::Lt,
                        left: Box::new(EvalContML4Expr::Int(4)),
                        right: Box::new(EvalContML4Expr::Int(5)),
                    }),
                    then_branch: Box::new(EvalContML4Expr::BinOp {
                        op: EvalContML4BinOp::Plus,
                        left: Box::new(EvalContML4Expr::Int(2)),
                        right: Box::new(EvalContML4Expr::Int(3)),
                    }),
                    else_branch: Box::new(EvalContML4Expr::BinOp {
                        op: EvalContML4BinOp::Times,
                        left: Box::new(EvalContML4Expr::Int(8)),
                        right: Box::new(EvalContML4Expr::Int(8)),
                    }),
                },
                continuation: EvalContML4Continuation::implicit_hole(),
                value: EvalContML4Value::Int(5),
                has_continuation: false,
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- 3 evalto 3 by E-Int { 3 => _ evalto 3 by C-Ret {} }")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
