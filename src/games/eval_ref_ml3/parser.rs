use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalRefML3Binding, EvalRefML3Derivation, EvalRefML3Env, EvalRefML3Expr, EvalRefML3Judgment,
    EvalRefML3Store, EvalRefML3StoreEntry, EvalRefML3Value,
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
        if !self.has_turnstile_before_by() {
            return Err(self.error_here("expected judgment"));
        }

        let env = self.parse_env()?;
        let expr = self.parse_expr()?;
        self.expect_slash()?;
        let store = self.parse_store()?;
        self.expect_evalto()?;
        let value = self.parse_value()?;
        self.expect_slash()?;
        let result_store = self.parse_store()?;

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

        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> Result<EvalRefML3Expr, CheckError> {
        let left = self.parse_prefix_expr()?;
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

        if let Some(location) = self.consume_location() {
            return Ok(EvalRefML3Expr::Loc(location));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(EvalRefML3Expr::Var(name));
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalRefML3Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalRefML3Value::Int(value));
        }

        if let Some(location) = self.consume_location() {
            return Ok(EvalRefML3Value::Loc(location));
        }

        if self.consume_lparen() {
            self.expect_rparen()?;
            return Ok(EvalRefML3Value::Unit);
        }

        Err(self.error_here("expected value"))
    }

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        let mut name = self.parse_identifier()?;
        while self.consume_minus() {
            let part = self.parse_identifier()?;
            name.push('-');
            name.push_str(&part);
        }
        Ok(name)
    }

    fn parse_identifier(&mut self) -> Result<String, CheckError> {
        if let Some(name) = self.consume_identifier() {
            Ok(name)
        } else {
            Err(self.error_here("expected identifier"))
        }
    }

    fn parse_location(&mut self) -> Result<String, CheckError> {
        if let Some(location) = self.consume_location() {
            Ok(location)
        } else {
            Err(self.error_here("expected location"))
        }
    }

    fn parse_subderivations(&mut self) -> Result<Vec<EvalRefML3Derivation>, CheckError> {
        if self.at_rbrace() {
            return Ok(Vec::new());
        }

        let mut subderivations = vec![self.parse_derivation()?];
        while self.consume_semicolon() {
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

    fn consume_keyword_let(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Let))
    }

    fn consume_keyword_in(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::In))
    }

    fn consume_keyword_ref(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Ref))
    }

    fn consume_assign(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Assign))
    }

    fn consume_bang(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Bang))
    }

    fn consume_minus(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Minus))
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_semicolon(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Semicolon))
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

    fn consume_if(&mut self, predicate: impl FnOnce(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::By), "expected 'by'")
    }

    fn expect_keyword_in(&mut self) -> Result<(), CheckError> {
        if self.consume_keyword_in() {
            Ok(())
        } else {
            Err(self.error_here("expected 'in'"))
        }
    }

    fn expect_evalto(&mut self) -> Result<(), CheckError> {
        self.expect_token(
            |kind| matches!(kind, TokenKind::EvalTo),
            "expected 'evalto'",
        )
    }

    fn expect_equal(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Equal), "expected '='")
    }

    fn expect_slash(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Slash), "expected '/'")
    }

    fn expect_turnstile(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Turnstile), "expected '|-'")
    }

    fn expect_lbrace(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::LBrace), "expected '{'")
    }

    fn expect_rbrace(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::RBrace), "expected '}'")
    }

    fn expect_rparen(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::RParen), "expected ')'")
    }

    fn at_rbrace(&self) -> bool {
        matches!(self.peek().kind, TokenKind::RBrace)
    }

    fn expect_eof(&self) -> Result<(), CheckError> {
        if !matches!(self.peek().kind, TokenKind::Eof) {
            return Err(
                CheckError::parse("expected end of input").with_span(self.peek().span.clone())
            );
        }
        Ok(())
    }

    fn expect_token(
        &mut self,
        predicate: impl FnOnce(&TokenKind) -> bool,
        message: &'static str,
    ) -> Result<(), CheckError> {
        if predicate(&self.peek().kind) {
            self.index += 1;
            Ok(())
        } else {
            Err(self.error_here(message))
        }
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

    #[test]
    fn parses_ref_eval_judgment() {
        let source = "|- ref 1 / () evalto @l0 / @l0 = 1";
        let judgment = parse_judgment_source(source).expect("judgment should parse");
        assert_eq!(judgment.to_string(), source);
    }

    #[test]
    fn parses_leaf_derivation() {
        let source = "|- 1 / () evalto 1 / () by E-Int {}";
        let derivation = parse_source(source).expect("derivation should parse");
        assert_eq!(derivation.rule_name, "E-Int");
        assert_eq!(derivation.judgment.to_string(), "|- 1 / () evalto 1 / ()");
    }

    #[test]
    fn rejects_trailing_tokens_in_judgment_mode() {
        let err = parse_judgment_source("|- 1 / () evalto 1 / () by E-Int {}")
            .expect_err("judgment parser should reject derivation input");
        assert!(err.message().contains("expected end of input"));
    }
}
