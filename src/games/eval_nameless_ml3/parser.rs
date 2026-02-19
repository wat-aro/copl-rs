use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    EvalNamelessML3BinOp, EvalNamelessML3Derivation, EvalNamelessML3Env, EvalNamelessML3Expr,
    EvalNamelessML3Judgment, EvalNamelessML3Value,
};

pub fn parse_source(source: &str) -> Result<EvalNamelessML3Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<EvalNamelessML3Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<EvalNamelessML3Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(EvalNamelessML3Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<EvalNamelessML3Judgment, CheckError> {
        if self.has_turnstile_before_by() {
            let env = self.parse_env()?;
            let expr = self.parse_expr()?;
            self.expect_evalto()?;
            let value = self.parse_value()?;
            return Ok(EvalNamelessML3Judgment::EvalTo { env, expr, value });
        }

        let expr = self.parse_expr()?;
        if let EvalNamelessML3Expr::Int(left) = expr {
            if self.consume_plus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalNamelessML3Judgment::PlusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_minus_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalNamelessML3Judgment::MinusIs {
                    left,
                    right,
                    result,
                });
            }

            if self.consume_times_word() {
                let right = self.parse_int_literal()?;
                self.expect_keyword_is()?;
                let result = self.parse_int_literal()?;
                return Ok(EvalNamelessML3Judgment::TimesIs {
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
                return Ok(EvalNamelessML3Judgment::LessThanIs {
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

    fn parse_env(&mut self) -> Result<EvalNamelessML3Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(EvalNamelessML3Env::default());
        }

        let mut values = vec![self.parse_value()?];
        while self.consume_comma() {
            values.push(self.parse_value()?);
        }
        self.expect_turnstile()?;
        Ok(EvalNamelessML3Env(values))
    }

    fn parse_env_in_parens(&mut self) -> Result<EvalNamelessML3Env, CheckError> {
        if self.at_rparen() {
            return Ok(EvalNamelessML3Env::default());
        }

        let mut values = vec![self.parse_value()?];
        while self.consume_comma() {
            values.push(self.parse_value()?);
        }
        Ok(EvalNamelessML3Env(values))
    }

    fn parse_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        if self.consume_keyword_let() {
            if self.consume_keyword_rec() {
                self.expect_dot()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                self.expect_dot()?;
                self.expect_arrow()?;
                let fun_body = self.parse_expr()?;
                self.expect_keyword_in()?;
                let body = self.parse_expr()?;
                return Ok(EvalNamelessML3Expr::LetRec {
                    fun_body: Box::new(fun_body),
                    body: Box::new(body),
                });
            }

            self.expect_dot()?;
            self.expect_equal()?;
            let bound_expr = self.parse_expr()?;
            self.expect_keyword_in()?;
            let body = self.parse_expr()?;
            return Ok(EvalNamelessML3Expr::Let {
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(EvalNamelessML3Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_fun_expr()
    }

    fn parse_fun_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        if self.consume_fun() {
            self.expect_dot()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            return Ok(EvalNamelessML3Expr::Fun {
                body: Box::new(body),
            });
        }

        self.parse_lt_expr()
    }

    fn parse_lt_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = EvalNamelessML3Expr::BinOp {
                op: EvalNamelessML3BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalNamelessML3Expr::BinOp {
                    op: EvalNamelessML3BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = EvalNamelessML3Expr::BinOp {
                    op: EvalNamelessML3BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        let mut expr = self.parse_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_app_expr()?;
            expr = EvalNamelessML3Expr::BinOp {
                op: EvalNamelessML3BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_app_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.starts_atom_expr() {
            let arg = self.parse_atom_expr()?;
            expr = EvalNamelessML3Expr::App {
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
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Index(_)
        )
    }

    fn parse_atom_expr(&mut self) -> Result<EvalNamelessML3Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalNamelessML3Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalNamelessML3Expr::Bool(value));
        }

        if let Some(value) = self.consume_index_literal() {
            return Ok(EvalNamelessML3Expr::Index(value));
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_value(&mut self) -> Result<EvalNamelessML3Value, CheckError> {
        if let Some(value) = self.consume_int_literal() {
            return Ok(EvalNamelessML3Value::Int(value));
        }
        if let Some(value) = self.consume_bool_literal() {
            return Ok(EvalNamelessML3Value::Bool(value));
        }

        if self.consume_lparen() {
            let env = self.parse_env_in_parens()?;
            self.expect_rparen()?;
            self.expect_lbracket()?;

            if self.consume_keyword_rec() {
                self.expect_dot()?;
                self.expect_equal()?;
                self.expect_keyword_fun()?;
                self.expect_dot()?;
                self.expect_arrow()?;
                let body = self.parse_expr()?;
                self.expect_rbracket()?;
                return Ok(EvalNamelessML3Value::RecClosure { env, body });
            }

            self.expect_keyword_fun()?;
            self.expect_dot()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            self.expect_rbracket()?;
            return Ok(EvalNamelessML3Value::Closure { env, body });
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
        match &self.peek().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            _ => Err(self.error_here("expected rule name after 'by'")),
        }
    }

    fn parse_subderivations(&mut self) -> Result<Vec<EvalNamelessML3Derivation>, CheckError> {
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

    fn expect_keyword_is(&mut self) -> Result<(), CheckError> {
        if self.consume_is() {
            Ok(())
        } else {
            Err(self.error_here("expected 'is'"))
        }
    }

    fn expect_keyword_than(&mut self) -> Result<(), CheckError> {
        if self.consume_than() {
            Ok(())
        } else {
            Err(self.error_here("expected 'than'"))
        }
    }

    fn expect_keyword_fun(&mut self) -> Result<(), CheckError> {
        if self.consume_fun() {
            Ok(())
        } else {
            Err(self.error_here("expected 'fun'"))
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

    fn expect_arrow(&mut self) -> Result<(), CheckError> {
        if self.consume_arrow() {
            Ok(())
        } else {
            Err(self.error_here("expected '->'"))
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

    fn expect_rparen(&mut self) -> Result<(), CheckError> {
        if self.consume_rparen() {
            Ok(())
        } else {
            Err(self.error_here("expected ')'"))
        }
    }

    fn expect_dot(&mut self) -> Result<(), CheckError> {
        if self.consume_dot() {
            Ok(())
        } else {
            Err(self.error_here("expected '.'"))
        }
    }

    fn expect_semicolon_or_rbrace(&mut self) -> Result<(), CheckError> {
        if self.consume_semicolon() || self.at_rbrace() {
            Ok(())
        } else {
            Err(self.error_here("expected ';' or '}'"))
        }
    }

    fn expect_eof(&mut self) -> Result<(), CheckError> {
        if matches!(self.peek().kind, TokenKind::Eof) {
            Ok(())
        } else {
            Err(self.error_here("expected end of input"))
        }
    }

    fn consume_trailing_semicolons(&mut self) {
        while self.consume_semicolon() {}
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

    fn consume_index_literal(&mut self) -> Option<usize> {
        match self.peek().kind {
            TokenKind::Index(value) => {
                self.bump();
                Some(value)
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

    fn consume_then(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Then))
    }

    fn consume_else(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Else))
    }

    fn consume_in(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::In))
    }

    fn consume_fun(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Fun))
    }

    fn consume_evalto(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::EvalTo))
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

    fn consume_comma(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_equal(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Equal))
    }

    fn consume_arrow(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Arrow))
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

    fn consume_keyword_let(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Let))
    }

    fn consume_keyword_rec(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Rec))
    }

    fn consume_keyword_if(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::If))
    }

    fn consume_dot(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Dot))
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
    use crate::games::eval_nameless_ml3::syntax::{
        EvalNamelessML3BinOp, EvalNamelessML3Env, EvalNamelessML3Expr, EvalNamelessML3Judgment,
        EvalNamelessML3Value,
    };

    #[test]
    fn parses_fixture_055() {
        let source = include_str!("../../../copl/055.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "E-IfT");
        assert_eq!(parsed.subderivations.len(), 2);
    }

    #[test]
    fn parses_fixtures_057_to_069() {
        for source in [
            include_str!("../../../copl/057.copl"),
            include_str!("../../../copl/059.copl"),
            include_str!("../../../copl/061.copl"),
            include_str!("../../../copl/063.copl"),
            include_str!("../../../copl/065.copl"),
            include_str!("../../../copl/067.copl"),
            include_str!("../../../copl/069.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "|- 1 evalto 1 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "E-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
|- let . = 2 in fun . -> #1 + #2 evalto (2)[fun . -> #1 + #2] by E-Let {
  |- 2 evalto 2 by E-Int {};
  2 |- fun . -> #1 + #2 evalto (2)[fun . -> #1 + #2] by E-Fun {};
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
        let source = "|- let rec . = fun . -> #1 in #1 1 evalto 1 by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let EvalNamelessML3Judgment::EvalTo { expr, .. } = parsed.judgment else {
            panic!("expected evalto");
        };
        assert_eq!(
            expr,
            EvalNamelessML3Expr::LetRec {
                fun_body: Box::new(EvalNamelessML3Expr::Index(1)),
                body: Box::new(EvalNamelessML3Expr::App {
                    func: Box::new(EvalNamelessML3Expr::Index(1)),
                    arg: Box::new(EvalNamelessML3Expr::Int(1)),
                }),
            }
        );
    }

    #[test]
    fn parses_fun_value_shape() {
        let source = "|- fun . -> #1 + 1 evalto ()[fun . -> #1 + 1] by E-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(
            parsed.judgment,
            EvalNamelessML3Judgment::EvalTo {
                env: EvalNamelessML3Env::default(),
                expr: EvalNamelessML3Expr::Fun {
                    body: Box::new(EvalNamelessML3Expr::BinOp {
                        op: EvalNamelessML3BinOp::Plus,
                        left: Box::new(EvalNamelessML3Expr::Index(1)),
                        right: Box::new(EvalNamelessML3Expr::Int(1)),
                    }),
                },
                value: EvalNamelessML3Value::Closure {
                    env: EvalNamelessML3Env::default(),
                    body: EvalNamelessML3Expr::BinOp {
                        op: EvalNamelessML3BinOp::Plus,
                        left: Box::new(EvalNamelessML3Expr::Index(1)),
                        right: Box::new(EvalNamelessML3Expr::Int(1)),
                    },
                },
            }
        );
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed = parse_judgment_source("5 |- if #1 < 5 then #1 + 2 else 0 evalto 7")
            .expect("judgment should parse");

        assert_eq!(
            parsed,
            EvalNamelessML3Judgment::EvalTo {
                env: EvalNamelessML3Env(vec![EvalNamelessML3Value::Int(5)]),
                expr: EvalNamelessML3Expr::If {
                    condition: Box::new(EvalNamelessML3Expr::BinOp {
                        op: EvalNamelessML3BinOp::Lt,
                        left: Box::new(EvalNamelessML3Expr::Index(1)),
                        right: Box::new(EvalNamelessML3Expr::Int(5)),
                    }),
                    then_branch: Box::new(EvalNamelessML3Expr::BinOp {
                        op: EvalNamelessML3BinOp::Plus,
                        left: Box::new(EvalNamelessML3Expr::Index(1)),
                        right: Box::new(EvalNamelessML3Expr::Int(2)),
                    }),
                    else_branch: Box::new(EvalNamelessML3Expr::Int(0)),
                },
                value: EvalNamelessML3Value::Int(7),
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- 1 evalto 1 by E-Int {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("expected end of input"));
    }
}
