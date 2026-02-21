use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalRefML3BinOp, EvalRefML3Binding, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr,
    EvalRefML3Judgment, EvalRefML3Store, EvalRefML3StoreEntry, EvalRefML3Value,
};

pub fn parse_source(source: &str) -> Result<EvalRefML3Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalRefML3Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalRefML3Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalRefML3Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalRefML3Judgment, CheckError> {
        if self.has_turnstile_before_by() {
            return self.parse_eval_judgment();
        }

        let left = self.parse_int_literal()?;
        if self.consume_plus_word() {
            let right = self.parse_int_literal()?;
            self.expect_keyword_is()?;
            let result = self.parse_int_literal()?;
            return Ok(EvalRefML3Judgment::PlusIs {
                left,
                right,
                result,
            });
        }

        if self.consume_minus_word() {
            let right = self.parse_int_literal()?;
            self.expect_keyword_is()?;
            let result = self.parse_int_literal()?;
            return Ok(EvalRefML3Judgment::MinusIs {
                left,
                right,
                result,
            });
        }

        if self.consume_times_word() {
            let right = self.parse_int_literal()?;
            self.expect_keyword_is()?;
            let result = self.parse_int_literal()?;
            return Ok(EvalRefML3Judgment::TimesIs {
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
            return Ok(EvalRefML3Judgment::LessThanIs {
                left,
                right,
                result,
            });
        }

        Err(self.error_here("expected judgment"))
    }

    fn parse_eval_judgment(&mut self) -> Result<EvalRefML3Judgment, CheckError> {
        let (env, mut store) = if self.consume_turnstile() {
            (EvalRefML3Env::default(), EvalRefML3Store::default())
        } else if self.has_slash_before_turnstile() {
            let store = self.parse_store()?;
            self.expect_slash()?;
            let env = self.parse_env()?;
            (env, store)
        } else {
            let env = self.parse_env()?;
            (env, EvalRefML3Store::default())
        };

        let expr = self.parse_expr()?;
        if self.consume_slash() {
            store = self.parse_store()?;
        }

        self.expect_evalto()?;
        let value = self.parse_value()?;
        let result_store = if self.consume_slash() {
            self.parse_store()?
        } else {
            store.clone()
        };

        Ok(EvalRefML3Judgment::EvalTo {
            env,
            expr,
            store,
            value,
            result_store,
        })
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

    fn has_slash_before_turnstile(&self) -> bool {
        let mut index = self.index;
        loop {
            match self.tokens.get(index).map(|token| &token.kind) {
                Some(TokenKind::Slash) => return true,
                Some(TokenKind::Turnstile) | Some(TokenKind::By) | Some(TokenKind::Eof) | None => {
                    return false;
                }
                _ => {
                    index += 1;
                }
            }
        }
    }

    fn parse_env(&mut self) -> Result<EvalRefML3Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(EvalRefML3Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        self.expect_turnstile()?;
        Ok(EvalRefML3Env(bindings))
    }

    fn parse_env_in_parens(&mut self) -> Result<EvalRefML3Env, CheckError> {
        if self.at_rparen() {
            return Ok(EvalRefML3Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        Ok(EvalRefML3Env(bindings))
    }

    fn parse_binding(&mut self) -> Result<EvalRefML3Binding, CheckError> {
        let name = self.parse_identifier()?;
        self.expect_equal()?;
        let value = self.parse_value()?;
        Ok(EvalRefML3Binding { name, value })
    }

    fn parse_store(&mut self) -> Result<EvalRefML3Store, CheckError> {
        if self.consume_lparen() {
            self.expect_rparen()?;
            return Ok(EvalRefML3Store::default());
        }

        let mut entries = vec![self.parse_store_entry()?];
        while self.consume_comma() {
            entries.push(self.parse_store_entry()?);
        }
        Ok(EvalRefML3Store(entries))
    }

    fn parse_store_entry(&mut self) -> Result<EvalRefML3StoreEntry, CheckError> {
        let location = self.parse_location()?;
        self.expect_equal()?;
        let value = self.parse_value()?;
        Ok(EvalRefML3StoreEntry { location, value })
    }

    fn parse_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
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
                return Ok(EvalRefML3Expr::LetRec {
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
            return Ok(EvalRefML3Expr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalRefML3Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_fun_expr()
    }

    fn parse_fun_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        if self.consume_keyword_fun() {
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            return Ok(EvalRefML3Expr::Fun {
                param,
                body: Box::new(body),
            });
        }

        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let left = self.parse_lt_expr()?;
        if self.consume_assign() {
            let right = self.parse_assign_expr()?;
            Ok(EvalRefML3Expr::Assign {
                target: Box::new(left),
                value: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    fn parse_lt_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalRefML3Expr::BinOp {
                op: EvalRefML3BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalRefML3Expr::BinOp {
                    op: EvalRefML3BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }

            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalRefML3Expr::BinOp {
                    op: EvalRefML3BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }

            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let mut expr = self.parse_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_app_expr()?;
            expr = EvalRefML3Expr::BinOp {
                op: EvalRefML3BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_app_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let mut expr = self.parse_prefix_expr()?;
        while self.starts_app_arg() {
            self.reject_unparenthesized_negative_app_argument()?;
            let arg = self.parse_prefix_expr()?;
            expr = EvalRefML3Expr::App {
                func: Box::new(expr),
                arg: Box::new(arg),
            };
        }
        Ok(expr)
    }

    fn starts_app_arg(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::LParen
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Identifier(_)
                | TokenKind::Location(_)
                | TokenKind::Ref
                | TokenKind::Bang
        )
    }

    fn reject_unparenthesized_negative_app_argument(&self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Int(value) if value < 0) {
            return Err(self.error_here(
                "negative integer application arguments must be parenthesized (use '(-n)')",
            ));
        }
        Ok(())
    }

    fn parse_prefix_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        if self.consume_keyword_ref() {
            let expr = self.parse_prefix_expr()?;
            return Ok(EvalRefML3Expr::Ref {
                expr: Box::new(expr),
            });
        }

        if self.consume_bang() {
            let expr = self.parse_prefix_expr()?;
            return Ok(EvalRefML3Expr::Deref {
                expr: Box::new(expr),
            });
        }

        self.parse_atom_expr()
    }

    fn parse_atom_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        if self.consume_lparen() {
            if self.consume_rparen() {
                return Ok(EvalRefML3Expr::Unit);
            }

            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalRefML3Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalRefML3Expr::Bool(value));
        }

        if let Some(location) = self.consume_location() {
            return Ok(EvalRefML3Expr::Loc(location));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalRefML3Expr::Var(name));
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalRefML3Value, CheckError> {
        self.parse_atomic_value()
    }

    fn parse_atomic_value(&mut self) -> Result<EvalRefML3Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalRefML3Value::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalRefML3Value::Bool(value));
        }

        if let Some(location) = self.consume_location() {
            return Ok(EvalRefML3Value::Loc(location));
        }

        if self.is_closure_value_start() {
            self.expect_lparen()?;
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
                return Ok(EvalRefML3Value::RecClosure {
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
            return Ok(EvalRefML3Value::Closure { env, param, body });
        }

        if self.consume_lparen() {
            if self.consume_rparen() {
                return Ok(EvalRefML3Value::Unit);
            }

            let value = self.parse_atomic_value()?;
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
                TokenKind::LParen => depth += 1,
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

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        let mut name = self.parse_identifier()?;
        while self.consume_minus_symbol() {
            let part = self.parse_identifier()?;
            name.push('-');
            name.push_str(&part);
        }
        Ok(name)
    }

    fn parse_identifier(&mut self) -> Result<String, CheckError> {
        self.consume_identifier()
            .ok_or_else(|| self.error_here("expected identifier"))
    }

    fn parse_location(&mut self) -> Result<String, CheckError> {
        self.consume_location()
            .ok_or_else(|| self.error_here("expected location"))
    }

    fn parse_int_literal(&mut self) -> Result<i64, CheckError> {
        self.consume_int_literal()
            .ok_or_else(|| self.error_here("expected integer literal"))
    }

    fn parse_bool_literal(&mut self) -> Result<bool, CheckError> {
        self.consume_bool_literal()
            .ok_or_else(|| self.error_here("expected boolean literal"))
    }

    fn parse_subderivations(&mut self) -> Result<Vec<EvalRefML3Derivation>, CheckError> {
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

    fn consume_identifier(&mut self) -> Option<String> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.index += 1;
                Some(name)
            }
            _ => None,
        }
    }

    fn consume_location(&mut self) -> Option<String> {
        match &self.peek().kind {
            TokenKind::Location(name) => {
                let name = name.clone();
                self.index += 1;
                Some(name)
            }
            _ => None,
        }
    }

    fn consume_int_literal(&mut self) -> Option<i64> {
        match self.peek().kind {
            TokenKind::Int(value) => {
                self.index += 1;
                Some(value)
            }
            _ => None,
        }
    }

    fn consume_bool_literal(&mut self) -> Option<bool> {
        if self.consume_if(|kind| matches!(kind, TokenKind::True)) {
            return Some(true);
        }
        if self.consume_if(|kind| matches!(kind, TokenKind::False)) {
            return Some(false);
        }
        None
    }

    fn consume_keyword_if(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::If))
    }

    fn consume_keyword_then(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Then))
    }

    fn consume_keyword_else(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Else))
    }

    fn consume_keyword_fun(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Fun))
    }

    fn consume_keyword_let(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Let))
    }

    fn consume_keyword_rec(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Rec))
    }

    fn consume_keyword_in(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::In))
    }

    fn consume_keyword_ref(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Ref))
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

    fn consume_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Arrow))
    }

    fn consume_evalto(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::EvalTo))
    }

    fn consume_keyword_is(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Is))
    }

    fn consume_keyword_by(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::By))
    }

    fn consume_equal(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Equal))
    }

    fn consume_turnstile(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Turnstile))
    }

    fn consume_slash(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Slash))
    }

    fn consume_assign(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Assign))
    }

    fn consume_bang(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Bang))
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

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_semicolon(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Semicolon))
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

    fn consume_if(&mut self, predicate: impl FnOnce(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_by() {
            Ok(())
        } else {
            Err(self.error_here("expected 'by'"))
        }
    }

    fn expect_keyword_then(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_then() {
            Ok(())
        } else {
            Err(self.error_here("expected 'then'"))
        }
    }

    fn expect_keyword_else(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_else() {
            Ok(())
        } else {
            Err(self.error_here("expected 'else'"))
        }
    }

    fn expect_keyword_fun(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_fun() {
            Ok(())
        } else {
            Err(self.error_here("expected 'fun'"))
        }
    }

    fn expect_keyword_in(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_in() {
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
        if self.consume_keyword_is() {
            Ok(())
        } else {
            Err(self.error_here("expected 'is'"))
        }
    }

    fn expect_arrow(&mut self) -> Result<(), CheckError> {
        if self.consume_arrow() {
            Ok(())
        } else {
            Err(self.error_here("expected '->'"))
        }
    }

    fn expect_evalto(&mut self) -> Result<(), CheckError> {
        if self.consume_evalto() {
            Ok(())
        } else {
            Err(self.error_here("expected 'evalto'"))
        }
    }

    fn expect_equal(&mut self) -> Result<(), CheckError> {
        if self.consume_equal() {
            Ok(())
        } else {
            Err(self.error_here("expected '='"))
        }
    }

    fn expect_slash(&mut self) -> Result<(), CheckError> {
        if self.consume_slash() {
            Ok(())
        } else {
            Err(self.error_here("expected '/'"))
        }
    }

    fn expect_turnstile(&mut self) -> Result<(), CheckError> {
        if self.consume_turnstile() {
            Ok(())
        } else {
            Err(self.error_here("expected '|-'"))
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

    fn at_rbrace(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RBrace)
    }

    fn at_rparen(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RParen)
    }

    fn expect_eof(&self) -> Result<(), CheckError> {
        if !matches!(self.peek().kind, TokenKind::Eof) {
            return Err(
                CheckError::parse("expected end of input").with_span(self.peek().span.clone())
            );
        }
        Ok(())
    }

    fn consume_trailing_semicolons(&mut self) {
        while self.consume_semicolon() {}
    }

    fn peek(&self) -> &Token {
        let index = self.index.min(self.tokens.len().saturating_sub(1));
        &self.tokens[index]
    }

    fn error_here(&self, message: &str) -> CheckError {
        self.error_at(self.peek().span.clone(), message)
    }

    fn error_at(&self, span: SourceSpan, message: &str) -> CheckError {
        CheckError::parse(message.to_string()).with_span(span)
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_judgment_source, parse_source};
    use crate::games::eval_ref_ml3::syntax::{EvalRefML3Expr, EvalRefML3Judgment};

    #[test]
    fn parses_ref_eval_judgment() {
        let source = "|- ref 1 / () evalto @l0 / @l0 = 1";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert_eq!(judgment.to_string(), "|- ref 1 evalto @l0 / @l0 = 1");
    }

    #[test]
    fn parses_store_prefixed_eval_judgment() {
        let source = "@l = 2 / x = @l |- if true then !x + 1 else 0 evalto 3 / @l = 2";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert!(judgment.to_string().contains("if true then !x + 1 else 0"));
    }

    #[test]
    fn parses_b_plus_judgment() {
        let source = "2 plus 3 is 5";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert_eq!(judgment.to_string(), source);
    }

    #[test]
    fn parses_closure_value_judgment() {
        let source = "|- fun x -> x evalto ()[fun x -> x]";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert!(judgment.to_string().contains("()[fun x -> x]"));
    }

    #[test]
    fn parses_let_rec_judgment() {
        let source = "|- let rec f = fun x -> x in f 1 evalto 1";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert!(judgment
            .to_string()
            .contains("let rec f = fun x -> x in f 1"));
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
    fn parses_right_associative_assign_expression() {
        let parsed =
            parse_judgment_source("|- x := y := 1 evalto 1").expect("judgment should parse");
        let EvalRefML3Judgment::EvalTo { expr, .. } = parsed else {
            panic!("expected evalto judgment");
        };
        assert_eq!(
            expr,
            EvalRefML3Expr::Assign {
                target: Box::new(EvalRefML3Expr::Var("x".to_string())),
                value: Box::new(EvalRefML3Expr::Assign {
                    target: Box::new(EvalRefML3Expr::Var("y".to_string())),
                    value: Box::new(EvalRefML3Expr::Int(1)),
                }),
            }
        );
    }

    #[test]
    fn parses_app_and_deref_precedence() {
        let parsed = parse_judgment_source("|- !f x evalto 0").expect("judgment should parse");
        let EvalRefML3Judgment::EvalTo { expr, .. } = parsed else {
            panic!("expected evalto judgment");
        };
        assert_eq!(
            expr,
            EvalRefML3Expr::App {
                func: Box::new(EvalRefML3Expr::Deref {
                    expr: Box::new(EvalRefML3Expr::Var("f".to_string())),
                }),
                arg: Box::new(EvalRefML3Expr::Var("x".to_string())),
            }
        );

        let parsed = parse_judgment_source("|- !(f x) evalto 0").expect("judgment should parse");
        let EvalRefML3Judgment::EvalTo { expr, .. } = parsed else {
            panic!("expected evalto judgment");
        };
        assert_eq!(
            expr,
            EvalRefML3Expr::Deref {
                expr: Box::new(EvalRefML3Expr::App {
                    func: Box::new(EvalRefML3Expr::Var("f".to_string())),
                    arg: Box::new(EvalRefML3Expr::Var("x".to_string())),
                }),
            }
        );
    }

    #[test]
    fn preserves_parenthesized_binop_argument_in_application() {
        let judgment =
            parse_judgment_source("|- f (1 + 2) evalto 3").expect("judgment should parse");
        let rendered = judgment.to_string();
        assert!(rendered.contains("f (1 + 2)"));
        assert!(!rendered.contains("f 1 + 2"));
    }

    #[test]
    fn parses_eval_ref_ml3_fixtures_141_to_145_derivations() {
        for source in [
            include_str!("../../../copl/141.copl"),
            include_str!("../../../copl/142.copl"),
            include_str!("../../../copl/143.copl"),
            include_str!("../../../copl/144.copl"),
            include_str!("../../../copl/145.copl"),
        ] {
            parse_source(source).expect("fixture derivation should parse");
        }
    }

    #[test]
    fn parses_leaf_derivation() {
        let source = "|- 1 / () evalto 1 / () by E-Int {}";
        let derivation = parse_source(source).expect("derivation should parse");
        assert_eq!(derivation.rule_name, "E-Int");
        assert_eq!(derivation.judgment.to_string(), "|- 1 evalto 1");
    }

    #[test]
    fn rejects_trailing_tokens_in_judgment_mode() {
        let err = parse_judgment_source("|- 1 / () evalto 1 / () by E-Int {}")
            .expect_err("judgment parser should reject derivation input");
        assert!(err.message().contains("expected end of input"));
    }
}
