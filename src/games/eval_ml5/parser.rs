use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalML5BinOp, EvalML5Binding, EvalML5Derivation, EvalML5Env, EvalML5Expr, EvalML5Judgment,
    EvalML5MatchClause, EvalML5Pattern, EvalML5Value,
};

pub fn parse_source(source: &str) -> Result<EvalML5Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalML5Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalML5Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalML5Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalML5Judgment, CheckError> {
        if self.has_turnstile_before_by() {
            let env = self.parse_env()?;
            let expr = self.parse_expr()?;
            self.expect_evalto()?;
            let value = self.parse_value()?;
            return Ok(EvalML5Judgment::EvalTo { expr, value, env });
        }

        if self.has_pattern_relation_before_by() {
            let pattern = self.parse_pattern()?;
            if self.consume_keyword_matches() {
                let value = self.parse_value()?;
                self.expect_keyword_when()?;
                let bindings = self.parse_bindings_in_parens()?;
                return Ok(EvalML5Judgment::Matches {
                    pattern,
                    value,
                    bindings,
                });
            }
            if self.consume_keyword_doesnt() {
                self.expect_keyword_match()?;
                let value = self.parse_value()?;
                return Ok(EvalML5Judgment::NotMatch { pattern, value });
            }
            return Err(self.error_here("expected pattern relation judgment"));
        }

        let expr = self.parse_expr()?;
        if let EvalML5Expr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML5Judgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML5Judgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalML5Judgment::TimesIs {
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
                return Ok(EvalML5Judgment::LessThanIs {
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

    fn has_pattern_relation_before_by(&self) -> bool {
        let mut index = self.index;
        loop {
            match self.tokens.get(index).map(|token| &token.kind) {
                Some(TokenKind::Matches) | Some(TokenKind::Doesnt) => return true,
                Some(TokenKind::By) | Some(TokenKind::Eof) | None => return false,
                _ => {
                    index += 1;
                }
            }
        }
    }

    fn parse_env(&mut self) -> Result<EvalML5Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(EvalML5Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        self.expect_turnstile()?;
        Ok(EvalML5Env(bindings))
    }

    fn parse_env_in_parens(&mut self) -> Result<EvalML5Env, CheckError> {
        if self.at_rparen() {
            return Ok(EvalML5Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        Ok(EvalML5Env(bindings))
    }

    fn parse_bindings_in_parens(&mut self) -> Result<EvalML5Env, CheckError> {
        self.expect_lparen()?;
        let env = self.parse_env_in_parens()?;
        self.expect_rparen()?;
        Ok(env)
    }

    fn parse_binding(&mut self) -> Result<EvalML5Binding, CheckError> {
        let name = self.parse_identifier()?;
        self.expect_equal()?;
        let value = self.parse_value()?;
        Ok(EvalML5Binding { name, value })
    }

    fn parse_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
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
                return Ok(EvalML5Expr::LetRec {
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
            return Ok(EvalML5Expr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalML5Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_fun_expr()
    }

    fn parse_fun_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        if self.consume_keyword_fun() {
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            return Ok(EvalML5Expr::Fun {
                param,
                body: Box::new(body),
            });
        }

        self.parse_match_expr()
    }

    fn parse_match_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        if self.consume_keyword_match() {
            let scrutinee = self.parse_expr()?;
            self.expect_keyword_with()?;
            let mut clauses = vec![self.parse_match_clause()?];
            while self.consume_bar() {
                clauses.push(self.parse_match_clause()?);
            }
            return Ok(EvalML5Expr::Match {
                scrutinee: Box::new(scrutinee),
                clauses,
            });
        }

        self.parse_cons_expr()
    }

    fn parse_cons_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        let head = self.parse_lt_expr()?;
        if self.consume_cons_symbol() {
            let tail = self.parse_cons_expr()?;
            Ok(EvalML5Expr::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_lt_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalML5Expr::BinOp {
                op: EvalML5BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML5Expr::BinOp {
                    op: EvalML5BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalML5Expr::BinOp {
                    op: EvalML5BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        let mut expr = self.parse_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_app_expr()?;
            expr = EvalML5Expr::BinOp {
                op: EvalML5BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_app_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.starts_atom_expr() {
            self.reject_unparenthesized_negative_app_argument()?;
            let arg = self.parse_atom_expr()?;
            expr = EvalML5Expr::App {
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

    fn parse_atom_expr(&mut self) -> Result<EvalML5Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if self.consume_lbracket() {
            self.expect_rbracket()?;
            return Ok(EvalML5Expr::Nil);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML5Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML5Expr::Bool(value));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalML5Expr::Var(name));
        }

        Err(self.error_here("expected expression"))
    }

    fn reject_unparenthesized_negative_app_argument(&self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Int(value) if value < 0) {
            return Err(self.error_here(
                "negative integer application arguments must be parenthesized (use '(-n)')",
            ));
        }
        Ok(())
    }

    fn parse_value(&mut self) -> Result<EvalML5Value, CheckError> {
        self.parse_cons_value()
    }

    fn parse_match_clause(&mut self) -> Result<EvalML5MatchClause, CheckError> {
        let pattern = self.parse_pattern()?;
        self.expect_arrow()?;
        let body = self.parse_expr()?;
        Ok(EvalML5MatchClause { pattern, body })
    }

    fn parse_pattern(&mut self) -> Result<EvalML5Pattern, CheckError> {
        let head = self.parse_pattern_atom()?;
        if self.consume_cons_symbol() {
            let tail = self.parse_pattern()?;
            Ok(EvalML5Pattern::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_pattern_atom(&mut self) -> Result<EvalML5Pattern, CheckError> {
        if self.consume_lparen() {
            let pattern = self.parse_pattern()?;
            self.expect_rparen()?;
            return Ok(pattern);
        }

        if self.consume_lbracket() {
            self.expect_rbracket()?;
            return Ok(EvalML5Pattern::Nil);
        }

        if self.consume_underscore() {
            return Ok(EvalML5Pattern::Wildcard);
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalML5Pattern::Var(name));
        }

        Err(self.error_here("expected pattern"))
    }

    fn parse_cons_value(&mut self) -> Result<EvalML5Value, CheckError> {
        let head = self.parse_atomic_value()?;
        if self.consume_cons_symbol() {
            let tail = self.parse_cons_value()?;
            Ok(EvalML5Value::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_atomic_value(&mut self) -> Result<EvalML5Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalML5Value::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalML5Value::Bool(value));
        }
        if self.consume_lbracket() {
            self.expect_rbracket()?;
            return Ok(EvalML5Value::Nil);
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
                return Ok(EvalML5Value::RecClosure {
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
            return Ok(EvalML5Value::Closure { env, param, body });
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

    fn parse_subderivations(&mut self) -> Result<Vec<EvalML5Derivation>, CheckError> {
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

    fn expect_keyword_when(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_when() {
            Ok(())
        } else {
            Err(self.error_here("expected 'when'"))
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

    fn expect_keyword_match(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_match() {
            Ok(())
        } else {
            Err(self.error_here("expected 'match'"))
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

    fn expect_lparen(&mut self) -> Result<(), CheckError> {
        if self.consume_lparen() {
            Ok(())
        } else {
            Err(self.error_here("expected '('"))
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

    fn consume_keyword_matches(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Matches))
    }

    fn consume_keyword_doesnt(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Doesnt))
    }

    fn consume_keyword_when(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::When))
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

    fn consume_underscore(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Underscore))
    }

    fn consume_if(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn at_rparen(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RParen)
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
    use crate::games::eval_ml5::syntax::{
        EvalML5Env, EvalML5Expr, EvalML5Judgment, EvalML5MatchClause, EvalML5Pattern, EvalML5Value,
    };

    #[test]
    fn parses_fixture_078() {
        let source = include_str!("../../../copl/078.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "E-LetRec");
        assert_eq!(parsed.subderivations.len(), 1);
        assert_eq!(
            parsed.judgment,
            EvalML5Judgment::EvalTo {
                env: EvalML5Env::default(),
                expr: EvalML5Expr::LetRec {
                    name: "max".to_string(),
                    param: "l".to_string(),
                    fun_body: Box::new(EvalML5Expr::Match {
                        scrutinee: Box::new(EvalML5Expr::Var("l".to_string())),
                        clauses: vec![
                            EvalML5MatchClause {
                                pattern: EvalML5Pattern::Cons {
                                    head: Box::new(EvalML5Pattern::Var("x".to_string())),
                                    tail: Box::new(EvalML5Pattern::Nil),
                                },
                                body: EvalML5Expr::Var("x".to_string()),
                            },
                            EvalML5MatchClause {
                                pattern: EvalML5Pattern::Cons {
                                    head: Box::new(EvalML5Pattern::Var("x".to_string())),
                                    tail: Box::new(EvalML5Pattern::Cons {
                                        head: Box::new(EvalML5Pattern::Var("y".to_string())),
                                        tail: Box::new(EvalML5Pattern::Var("z".to_string())),
                                    }),
                                },
                                body: EvalML5Expr::If {
                                    condition: Box::new(EvalML5Expr::BinOp {
                                        op: crate::games::eval_ml5::syntax::EvalML5BinOp::Lt,
                                        left: Box::new(EvalML5Expr::Var("x".to_string())),
                                        right: Box::new(EvalML5Expr::Var("y".to_string())),
                                    }),
                                    then_branch: Box::new(EvalML5Expr::App {
                                        func: Box::new(EvalML5Expr::Var("max".to_string())),
                                        arg: Box::new(EvalML5Expr::Cons {
                                            head: Box::new(EvalML5Expr::Var("y".to_string())),
                                            tail: Box::new(EvalML5Expr::Var("z".to_string())),
                                        }),
                                    }),
                                    else_branch: Box::new(EvalML5Expr::App {
                                        func: Box::new(EvalML5Expr::Var("max".to_string())),
                                        arg: Box::new(EvalML5Expr::Cons {
                                            head: Box::new(EvalML5Expr::Var("x".to_string())),
                                            tail: Box::new(EvalML5Expr::Var("z".to_string())),
                                        }),
                                    }),
                                },
                            },
                        ],
                    }),
                    body: Box::new(EvalML5Expr::App {
                        func: Box::new(EvalML5Expr::Var("max".to_string())),
                        arg: Box::new(EvalML5Expr::Cons {
                            head: Box::new(EvalML5Expr::Int(9)),
                            tail: Box::new(EvalML5Expr::Cons {
                                head: Box::new(EvalML5Expr::Int(2)),
                                tail: Box::new(EvalML5Expr::Cons {
                                    head: Box::new(EvalML5Expr::Int(3)),
                                    tail: Box::new(EvalML5Expr::Nil),
                                }),
                            }),
                        }),
                    }),
                },
                value: EvalML5Value::Int(9),
            }
        );
    }

    #[test]
    fn parses_fixture_079() {
        let source = include_str!("../../../copl/079.copl");
        parse_source(source).expect("fixture should parse");
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "|- [] evalto [] by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "E-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
[] doesn't match 1 :: [] by NM-ConsNil {
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations.len(), 0);
    }

    #[test]
    fn parses_match_expression_shape() {
        let source =
            "|- match x with [] -> 0 | [] :: l' -> 1 | (y :: _) :: z -> y evalto 0 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let EvalML5Judgment::EvalTo { expr, .. } = parsed.judgment else {
            panic!("expected evalto");
        };
        assert_eq!(
            expr,
            EvalML5Expr::Match {
                scrutinee: Box::new(EvalML5Expr::Var("x".to_string())),
                clauses: vec![
                    EvalML5MatchClause {
                        pattern: EvalML5Pattern::Nil,
                        body: EvalML5Expr::Int(0),
                    },
                    EvalML5MatchClause {
                        pattern: EvalML5Pattern::Cons {
                            head: Box::new(EvalML5Pattern::Nil),
                            tail: Box::new(EvalML5Pattern::Var("l'".to_string())),
                        },
                        body: EvalML5Expr::Int(1),
                    },
                    EvalML5MatchClause {
                        pattern: EvalML5Pattern::Cons {
                            head: Box::new(EvalML5Pattern::Cons {
                                head: Box::new(EvalML5Pattern::Var("y".to_string())),
                                tail: Box::new(EvalML5Pattern::Wildcard),
                            }),
                            tail: Box::new(EvalML5Pattern::Var("z".to_string())),
                        },
                        body: EvalML5Expr::Var("y".to_string()),
                    },
                ],
            }
        );
    }

    #[test]
    fn parses_application_tighter_than_cons() {
        let source = "|- f 1::2::[] evalto [] by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let EvalML5Judgment::EvalTo { expr, .. } = parsed.judgment else {
            panic!("expected evalto");
        };
        assert_eq!(
            expr,
            EvalML5Expr::Cons {
                head: Box::new(EvalML5Expr::App {
                    func: Box::new(EvalML5Expr::Var("f".to_string())),
                    arg: Box::new(EvalML5Expr::Int(1)),
                }),
                tail: Box::new(EvalML5Expr::Cons {
                    head: Box::new(EvalML5Expr::Int(2)),
                    tail: Box::new(EvalML5Expr::Nil),
                }),
            }
        );
    }

    #[test]
    fn rejects_unparenthesized_negative_int_in_app_expr() {
        let source = "|- f -2 evalto -2 by E-App {}";
        let err = parse_source(source).expect_err("parse should fail");
        assert!(err
            .message()
            .contains("negative integer application arguments must be parenthesized"));
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source(
            "|- match x with [] -> 0 | [] :: l' -> 1 | (y :: _) :: z -> y evalto 1",
        )
        .expect("judgment should parse");
        assert_eq!(
            parsed,
            EvalML5Judgment::EvalTo {
                env: EvalML5Env::default(),
                expr: EvalML5Expr::Match {
                    scrutinee: Box::new(EvalML5Expr::Var("x".to_string())),
                    clauses: vec![
                        EvalML5MatchClause {
                            pattern: EvalML5Pattern::Nil,
                            body: EvalML5Expr::Int(0),
                        },
                        EvalML5MatchClause {
                            pattern: EvalML5Pattern::Cons {
                                head: Box::new(EvalML5Pattern::Nil),
                                tail: Box::new(EvalML5Pattern::Var("l'".to_string())),
                            },
                            body: EvalML5Expr::Int(1),
                        },
                        EvalML5MatchClause {
                            pattern: EvalML5Pattern::Cons {
                                head: Box::new(EvalML5Pattern::Cons {
                                    head: Box::new(EvalML5Pattern::Var("y".to_string())),
                                    tail: Box::new(EvalML5Pattern::Wildcard),
                                }),
                                tail: Box::new(EvalML5Pattern::Var("z".to_string())),
                            },
                            body: EvalML5Expr::Var("y".to_string()),
                        },
                    ],
                },
                value: EvalML5Value::Int(1),
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- [] evalto [] by E-Nil {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
