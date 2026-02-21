use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Derivation, NamelessML3Env,
    NamelessML3Judgment,
};

pub fn parse_source(source: &str) -> Result<NamelessML3Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<NamelessML3Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<NamelessML3Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(NamelessML3Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<NamelessML3Judgment, CheckError> {
        let env = self.parse_env()?;
        let named = self.parse_named_expr()?;
        self.expect_translates()?;
        let nameless = self.parse_nameless_expr()?;
        Ok(NamelessML3Judgment::Translates {
            env,
            named,
            nameless,
        })
    }

    fn parse_env(&mut self) -> Result<NamelessML3Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(NamelessML3Env::default());
        }

        let mut names = vec![self.parse_identifier()?];
        while self.consume_comma() {
            names.push(self.parse_identifier()?);
        }
        self.expect_turnstile()?;

        Ok(NamelessML3Env(names))
    }

    fn parse_named_expr(&mut self) -> Result<NamedExpr, CheckError> {
        self.parse_named_let_expr()
    }

    fn parse_named_let_expr(&mut self) -> Result<NamedExpr, CheckError> {
        if self.consume_keyword_let() {
            if self.consume_keyword_rec() {
                let name = self.parse_identifier()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                let param = self.parse_identifier()?;
                self.expect_arrow()?;
                let fun_body = self.parse_named_expr()?;
                self.expect_keyword_in()?;
                let body = self.parse_named_expr()?;
                return Ok(NamedExpr::LetRec {
                    name,
                    param,
                    fun_body: Box::new(fun_body),
                    body: Box::new(body),
                });
            }

            let name = self.parse_identifier()?;
            self.expect_equal()?;
            let bound_expr = self.parse_named_expr()?;
            self.expect_keyword_in()?;
            let body = self.parse_named_expr()?;
            return Ok(NamedExpr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_named_if_expr()
    }

    fn parse_named_if_expr(&mut self) -> Result<NamedExpr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_named_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_named_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_named_expr()?;
            return Ok(NamedExpr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_named_fun_expr()
    }

    fn parse_named_fun_expr(&mut self) -> Result<NamedExpr, CheckError> {
        if self.consume_keyword_fun() {
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_named_expr()?;
            return Ok(NamedExpr::Fun {
                param,
                body: Box::new(body),
            });
        }

        self.parse_named_lt_expr()
    }

    fn parse_named_lt_expr(&mut self) -> Result<NamedExpr, CheckError> {
        let mut expr = self.parse_named_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_named_add_expr()?;
            expr = NamedExpr::BinOp {
                op: NamelessML3BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_named_add_expr(&mut self) -> Result<NamedExpr, CheckError> {
        let mut expr = self.parse_named_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_named_mul_expr()?;
                expr = NamedExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_named_mul_expr()?;
                expr = NamedExpr::BinOp {
                    op: NamelessML3BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }

        Ok(expr)
    }

    fn parse_named_mul_expr(&mut self) -> Result<NamedExpr, CheckError> {
        let mut expr = self.parse_named_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_named_app_expr()?;
            expr = NamedExpr::BinOp {
                op: NamelessML3BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_named_app_expr(&mut self) -> Result<NamedExpr, CheckError> {
        let mut expr = self.parse_named_atom_expr()?;
        while self.starts_named_atom_expr() {
            self.reject_unparenthesized_negative_app_argument()?;
            let arg = self.parse_named_atom_expr()?;
            expr = NamedExpr::App {
                func: Box::new(expr),
                arg: Box::new(arg),
            };
        }
        Ok(expr)
    }

    fn starts_named_atom_expr(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::LParen
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Identifier(_)
        )
    }

    fn parse_named_atom_expr(&mut self) -> Result<NamedExpr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_named_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(NamedExpr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(NamedExpr::Bool(value));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(NamedExpr::Var(name));
        }

        Err(self.error_here("expected named expression"))
    }

    fn parse_nameless_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        self.parse_nameless_let_expr()
    }

    fn parse_nameless_let_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        if self.consume_keyword_let() {
            if self.consume_keyword_rec() {
                self.expect_dot()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                self.expect_dot()?;
                self.expect_arrow()?;
                let fun_body = self.parse_nameless_expr()?;
                self.expect_keyword_in()?;
                let body = self.parse_nameless_expr()?;
                return Ok(NamelessExpr::LetRec {
                    fun_body: Box::new(fun_body),
                    body: Box::new(body),
                });
            }

            self.expect_dot()?;
            self.expect_equal()?;
            let bound_expr = self.parse_nameless_expr()?;
            self.expect_keyword_in()?;
            let body = self.parse_nameless_expr()?;
            return Ok(NamelessExpr::Let {
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_nameless_if_expr()
    }

    fn parse_nameless_if_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_nameless_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_nameless_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_nameless_expr()?;
            return Ok(NamelessExpr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_nameless_fun_expr()
    }

    fn parse_nameless_fun_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        if self.consume_keyword_fun() {
            self.expect_dot()?;
            self.expect_arrow()?;
            let body = self.parse_nameless_expr()?;
            return Ok(NamelessExpr::Fun {
                body: Box::new(body),
            });
        }

        self.parse_nameless_lt_expr()
    }

    fn parse_nameless_lt_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        let mut expr = self.parse_nameless_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_nameless_add_expr()?;
            expr = NamelessExpr::BinOp {
                op: NamelessML3BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_nameless_add_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        let mut expr = self.parse_nameless_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_nameless_mul_expr()?;
                expr = NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_nameless_mul_expr()?;
                expr = NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }

        Ok(expr)
    }

    fn parse_nameless_mul_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        let mut expr = self.parse_nameless_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_nameless_app_expr()?;
            expr = NamelessExpr::BinOp {
                op: NamelessML3BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_nameless_app_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        let mut expr = self.parse_nameless_atom_expr()?;
        while self.starts_nameless_atom_expr() {
            self.reject_unparenthesized_negative_app_argument()?;
            let arg = self.parse_nameless_atom_expr()?;
            expr = NamelessExpr::App {
                func: Box::new(expr),
                arg: Box::new(arg),
            };
        }
        Ok(expr)
    }

    fn starts_nameless_atom_expr(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::LParen
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Index(_)
        )
    }

    fn parse_nameless_atom_expr(&mut self) -> Result<NamelessExpr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_nameless_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(NamelessExpr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(NamelessExpr::Bool(value));
        }

        if let Some(value) = self.consume_index_literal() {
            return Ok(NamelessExpr::Index(value));
        }

        Err(self.error_here("expected nameless expression"))
    }

    fn reject_unparenthesized_negative_app_argument(&self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Int(value) if value < 0) {
            return Err(self.error_here(
                "negative integer application arguments must be parenthesized (use '(-n)')",
            ));
        }
        Ok(())
    }

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            _ => Err(self.error_here("expected rule name after 'by'")),
        }
    }

    fn parse_subderivations(&mut self) -> Result<Vec<NamelessML3Derivation>, CheckError> {
        if self.at_rbrace() {
            return Ok(Vec::new());
        }

        let mut derivations = Vec::new();
        loop {
            derivations.push(self.parse_derivation()?);
            if self.consume_semicolon() {
                if self.at_rbrace() {
                    break;
                }
                continue;
            }
            break;
        }

        Ok(derivations)
    }

    fn consume_trailing_semicolons(&mut self) {
        while self.consume_semicolon() {}
    }

    fn expect_eof(&self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Eof) {
            Ok(())
        } else {
            Err(self.error_here("unexpected token after derivation"))
        }
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::By), "expected 'by'")
    }

    fn expect_translates(&mut self) -> Result<(), CheckError> {
        self.expect_token(
            |kind| matches!(kind, TokenKind::Translates),
            "expected '==>'",
        )
    }

    fn expect_keyword_in(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::In), "expected 'in'")
    }

    fn expect_keyword_then(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Then), "expected 'then'")
    }

    fn expect_keyword_else(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Else), "expected 'else'")
    }

    fn expect_keyword_fun(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Fun), "expected 'fun'")
    }

    fn expect_turnstile(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Turnstile), "expected '|-'")
    }

    fn expect_equal(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Equal), "expected '='")
    }

    fn expect_arrow(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Arrow), "expected '->'")
    }

    fn expect_rparen(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::RParen), "expected ')'")
    }

    fn expect_lbrace(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::LBrace), "expected '{'")
    }

    fn expect_rbrace(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::RBrace), "expected '}'")
    }

    fn expect_dot(&mut self) -> Result<(), CheckError> {
        self.expect_token(|kind| matches!(kind, TokenKind::Dot), "expected '.'")
    }

    fn expect_token(
        &mut self,
        predicate: impl Fn(&TokenKind) -> bool,
        message: &str,
    ) -> Result<(), CheckError> {
        if predicate(&self.peek().kind) {
            self.bump();
            Ok(())
        } else {
            Err(self.error_here(message))
        }
    }

    fn parse_identifier(&mut self) -> Result<String, CheckError> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            _ => Err(self.error_here("expected identifier")),
        }
    }

    fn consume_identifier(&mut self) -> Option<String> {
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.bump();
                Some(name)
            }
            _ => None,
        }
    }

    fn consume_int_literal(&mut self) -> Option<i64> {
        match self.peek().kind {
            TokenKind::Int(value) => {
                self.bump();
                Some(value)
            }
            _ => None,
        }
    }

    fn consume_index_literal(&mut self) -> Option<usize> {
        match self.peek().kind {
            TokenKind::Index(value) => {
                self.bump();
                Some(value)
            }
            _ => None,
        }
    }

    fn consume_bool_literal(&mut self) -> Option<bool> {
        if self.consume_if(|kind| matches!(kind, TokenKind::True)) {
            Some(true)
        } else if self.consume_if(|kind| matches!(kind, TokenKind::False)) {
            Some(false)
        } else {
            None
        }
    }

    fn consume_keyword_let(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Let))
    }

    fn consume_keyword_rec(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Rec))
    }

    fn consume_keyword_if(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::If))
    }

    fn consume_keyword_fun(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Fun))
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_turnstile(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Turnstile))
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
    use crate::games::nameless_ml3::syntax::{
        NamedExpr, NamelessExpr, NamelessML3BinOp, NamelessML3Env, NamelessML3Judgment,
    };

    #[test]
    fn parses_fixture_054() {
        let source = include_str!("../../../copl/054.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "Tr-If");
        assert_eq!(parsed.subderivations.len(), 3);
    }

    #[test]
    fn parses_fixtures_056_to_068() {
        for source in [
            include_str!("../../../copl/056.copl"),
            include_str!("../../../copl/058.copl"),
            include_str!("../../../copl/060.copl"),
            include_str!("../../../copl/062.copl"),
            include_str!("../../../copl/064.copl"),
            include_str!("../../../copl/066.copl"),
            include_str!("../../../copl/068.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "|- 1 ==> 1 by Tr-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "Tr-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
|- let x = 3 in x ==> let . = 3 in #1 by Tr-Let {
  |- 3 ==> 3 by Tr-Int {};
  x |- x ==> #1 by Tr-Var1 {};
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }

    #[test]
    fn parses_let_rec_expression_shape() {
        let source =
            "|- let rec f = fun x -> x in f 1 ==> let rec . = fun . -> #1 in #1 1 by Tr-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(
            parsed.judgment,
            NamelessML3Judgment::Translates {
                env: NamelessML3Env::default(),
                named: NamedExpr::LetRec {
                    name: "f".to_string(),
                    param: "x".to_string(),
                    fun_body: Box::new(NamedExpr::Var("x".to_string())),
                    body: Box::new(NamedExpr::App {
                        func: Box::new(NamedExpr::Var("f".to_string())),
                        arg: Box::new(NamedExpr::Int(1)),
                    }),
                },
                nameless: NamelessExpr::LetRec {
                    fun_body: Box::new(NamelessExpr::Index(1)),
                    body: Box::new(NamelessExpr::App {
                        func: Box::new(NamelessExpr::Index(1)),
                        arg: Box::new(NamelessExpr::Int(1)),
                    }),
                },
            }
        );
    }

    #[test]
    fn preserves_named_and_nameless_operator_precedence() {
        let source = "x |- x + 1 * 2 ==> #1 + 1 * 2 by Tr-Plus { x |- x ==> #1 by Tr-Var1 {}; x |- 1 * 2 ==> 1 * 2 by Tr-Times { x |- 1 ==> 1 by Tr-Int {}; x |- 2 ==> 2 by Tr-Int {} } }";
        let parsed = parse_source(source).expect("parser should succeed");
        let NamelessML3Judgment::Translates {
            named, nameless, ..
        } = parsed.judgment;
        assert_eq!(
            named,
            NamedExpr::BinOp {
                op: NamelessML3BinOp::Plus,
                left: Box::new(NamedExpr::Var("x".to_string())),
                right: Box::new(NamedExpr::BinOp {
                    op: NamelessML3BinOp::Times,
                    left: Box::new(NamedExpr::Int(1)),
                    right: Box::new(NamedExpr::Int(2)),
                }),
            }
        );
        assert_eq!(
            nameless,
            NamelessExpr::BinOp {
                op: NamelessML3BinOp::Plus,
                left: Box::new(NamelessExpr::Index(1)),
                right: Box::new(NamelessExpr::BinOp {
                    op: NamelessML3BinOp::Times,
                    left: Box::new(NamelessExpr::Int(1)),
                    right: Box::new(NamelessExpr::Int(2)),
                }),
            }
        );
    }

    #[test]
    fn rejects_unparenthesized_negative_int_in_named_app_expr() {
        let source = "|- f -2 ==> #1 (-2) by Tr-App {}";
        let err = parse_source(source).expect_err("parse should fail");
        assert!(err
            .message()
            .contains("negative integer application arguments must be parenthesized"));
    }

    #[test]
    fn rejects_unparenthesized_negative_int_in_nameless_app_expr() {
        let source = "|- f (-2) ==> #1 -2 by Tr-App {}";
        let err = parse_source(source).expect_err("parse should fail");
        assert!(err
            .message()
            .contains("negative integer application arguments must be parenthesized"));
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source("|- let x = 3 in x ==> let . = 3 in #1")
            .expect("judgment should parse");
        assert_eq!(
            parsed,
            NamelessML3Judgment::Translates {
                env: NamelessML3Env::default(),
                named: NamedExpr::Let {
                    name: "x".to_string(),
                    bound_expr: Box::new(NamedExpr::Int(3)),
                    body: Box::new(NamedExpr::Var("x".to_string())),
                },
                nameless: NamelessExpr::Let {
                    bound_expr: Box::new(NamelessExpr::Int(3)),
                    body: Box::new(NamelessExpr::Index(1)),
                },
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- 1 ==> 1 by Tr-Int {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("unexpected token after derivation"));
    }
}
