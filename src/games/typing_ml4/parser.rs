use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{
    TypingML4BinOp, TypingML4Binding, TypingML4Derivation, TypingML4Env, TypingML4Expr,
    TypingML4Judgment, TypingML4Type,
};

pub fn parse_source(source: &str) -> Result<TypingML4Derivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<TypingML4Judgment, CheckError> {
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

    fn parse_derivation(&mut self) -> Result<TypingML4Derivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;
        Ok(TypingML4Derivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<TypingML4Judgment, CheckError> {
        if !self.has_turnstile_before_by() {
            return Err(self.error_here("expected typing judgment"));
        }

        let env = self.parse_env()?;
        let expr = self.parse_expr()?;
        self.expect_colon()?;
        let ty = self.parse_type()?;
        Ok(TypingML4Judgment::HasType { env, expr, ty })
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

    fn parse_env(&mut self) -> Result<TypingML4Env, CheckError> {
        if self.consume_turnstile() {
            return Ok(TypingML4Env::default());
        }

        let mut bindings = vec![self.parse_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_binding()?);
        }
        self.expect_turnstile()?;
        Ok(TypingML4Env(bindings))
    }

    fn parse_binding(&mut self) -> Result<TypingML4Binding, CheckError> {
        let name = self.parse_identifier()?;
        self.expect_colon()?;
        let ty = self.parse_type()?;
        Ok(TypingML4Binding { name, ty })
    }

    fn parse_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
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
                return Ok(TypingML4Expr::LetRec {
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
            return Ok(TypingML4Expr::Let {
                name,
                bound_expr: Box::new(bound_expr),
                body: Box::new(body),
            });
        }

        self.parse_if_expr()
    }

    fn parse_if_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        if self.consume_keyword_if() {
            let condition = self.parse_expr()?;
            self.expect_keyword_then()?;
            let then_branch = self.parse_expr()?;
            self.expect_keyword_else()?;
            let else_branch = self.parse_expr()?;
            return Ok(TypingML4Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        self.parse_fun_expr()
    }

    fn parse_fun_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        if self.consume_keyword_fun() {
            let param = self.parse_identifier()?;
            self.expect_arrow()?;
            let body = self.parse_expr()?;
            return Ok(TypingML4Expr::Fun {
                param,
                body: Box::new(body),
            });
        }

        self.parse_match_expr()
    }

    fn parse_match_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
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
            return Ok(TypingML4Expr::Match {
                scrutinee: Box::new(scrutinee),
                nil_case: Box::new(nil_case),
                head_name,
                tail_name,
                cons_case: Box::new(cons_case),
            });
        }

        self.parse_cons_expr()
    }

    fn parse_cons_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        let head = self.parse_lt_expr()?;
        if self.consume_cons_symbol() {
            let tail = self.parse_cons_expr()?;
            Ok(TypingML4Expr::Cons {
                head: Box::new(head),
                tail: Box::new(tail),
            })
        } else {
            Ok(head)
        }
    }

    fn parse_lt_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        let mut expr = self.parse_add_expr()?;
        while self.consume_lt_symbol() {
            let right = self.parse_add_expr()?;
            expr = TypingML4Expr::BinOp {
                op: TypingML4BinOp::Lt,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            if self.consume_plus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = TypingML4Expr::BinOp {
                    op: TypingML4BinOp::Plus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            if self.consume_minus_symbol() {
                let right = self.parse_mul_expr()?;
                expr = TypingML4Expr::BinOp {
                    op: TypingML4BinOp::Minus,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        let mut expr = self.parse_app_expr()?;
        while self.consume_times_symbol() {
            let right = self.parse_app_expr()?;
            expr = TypingML4Expr::BinOp {
                op: TypingML4BinOp::Times,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_app_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        let mut expr = self.parse_atom_expr()?;
        while self.starts_atom_expr() {
            let arg = self.parse_atom_expr()?;
            expr = TypingML4Expr::App {
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

    fn parse_atom_expr(&mut self) -> Result<TypingML4Expr, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_expr()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if self.consume_lbracket() {
            self.expect_rbracket()?;
            return Ok(TypingML4Expr::Nil);
        }

        if let Some(value) = self.consume_int_literal() {
            return Ok(TypingML4Expr::Int(value));
        }

        if let Some(value) = self.consume_bool_literal() {
            return Ok(TypingML4Expr::Bool(value));
        }

        if let Some(name) = self.consume_identifier() {
            return Ok(TypingML4Expr::Var(name));
        }

        Err(self.error_here("expected expression"))
    }

    fn parse_type(&mut self) -> Result<TypingML4Type, CheckError> {
        self.parse_fun_type()
    }

    fn parse_fun_type(&mut self) -> Result<TypingML4Type, CheckError> {
        let param = self.parse_list_type()?;
        if self.consume_arrow() {
            let ret = self.parse_fun_type()?;
            Ok(TypingML4Type::Fun {
                param: Box::new(param),
                ret: Box::new(ret),
            })
        } else {
            Ok(param)
        }
    }

    fn parse_list_type(&mut self) -> Result<TypingML4Type, CheckError> {
        let mut ty = self.parse_atomic_type()?;
        while self.consume_keyword_list() {
            ty = TypingML4Type::List(Box::new(ty));
        }
        Ok(ty)
    }

    fn parse_atomic_type(&mut self) -> Result<TypingML4Type, CheckError> {
        if self.consume_lparen() {
            let ty = self.parse_type()?;
            self.expect_rparen()?;
            return Ok(ty);
        }

        if self.consume_keyword_int_type() {
            return Ok(TypingML4Type::Int);
        }

        if self.consume_keyword_bool_type() {
            return Ok(TypingML4Type::Bool);
        }

        Err(self.error_here("expected type"))
    }

    fn parse_identifier(&mut self) -> Result<String, CheckError> {
        self.consume_identifier()
            .ok_or_else(|| self.error_here("expected identifier"))
    }

    fn parse_rule_name(&mut self) -> Result<String, CheckError> {
        self.parse_identifier()
    }

    fn parse_subderivations(&mut self) -> Result<Vec<TypingML4Derivation>, CheckError> {
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

    fn expect_equal(&mut self) -> Result<(), CheckError> {
        if self.consume_equal() {
            Ok(())
        } else {
            Err(self.error_here("expected '='"))
        }
    }

    fn expect_colon(&mut self) -> Result<(), CheckError> {
        if self.consume_colon() {
            Ok(())
        } else {
            Err(self.error_here("expected ':'"))
        }
    }

    fn expect_turnstile(&mut self) -> Result<(), CheckError> {
        if self.consume_turnstile() {
            Ok(())
        } else {
            Err(self.error_here("expected '|-'"))
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
            Err(self.error_here("unexpected trailing tokens"))
        }
    }

    fn consume_trailing_semicolons(&mut self) {
        while self.consume_semicolon() {}
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

    fn consume_keyword_int_type(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::IntType))
    }

    fn consume_keyword_bool_type(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::BoolType))
    }

    fn consume_keyword_list(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::ListType))
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

    fn consume_colon(&mut self) -> bool {
        self.consume_if(|kind| matches!(kind, TokenKind::Colon))
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
    use crate::games::typing_ml4::syntax::{
        TypingML4BinOp, TypingML4Env, TypingML4Expr, TypingML4Judgment, TypingML4Type,
    };

    #[test]
    fn parses_fixture_080() {
        let source = include_str!("../../../copl/080.copl");
        let parsed = parse_source(source).expect("fixture should parse");
        assert_eq!(parsed.rule_name, "T-Plus");
        assert_eq!(parsed.subderivations.len(), 2);
        assert_eq!(
            parsed.judgment,
            TypingML4Judgment::HasType {
                env: TypingML4Env::default(),
                expr: TypingML4Expr::BinOp {
                    op: TypingML4BinOp::Plus,
                    left: Box::new(TypingML4Expr::Int(3)),
                    right: Box::new(TypingML4Expr::Int(5)),
                },
                ty: TypingML4Type::Int,
            }
        );
    }

    #[test]
    fn parses_fixtures_081_to_106() {
        for source in [
            include_str!("../../../copl/081.copl"),
            include_str!("../../../copl/082.copl"),
            include_str!("../../../copl/083.copl"),
            include_str!("../../../copl/084.copl"),
            include_str!("../../../copl/085.copl"),
            include_str!("../../../copl/086.copl"),
            include_str!("../../../copl/087.copl"),
            include_str!("../../../copl/088.copl"),
            include_str!("../../../copl/089.copl"),
            include_str!("../../../copl/090.copl"),
            include_str!("../../../copl/091.copl"),
            include_str!("../../../copl/092.copl"),
            include_str!("../../../copl/093.copl"),
            include_str!("../../../copl/094.copl"),
            include_str!("../../../copl/095.copl"),
            include_str!("../../../copl/096.copl"),
            include_str!("../../../copl/097.copl"),
            include_str!("../../../copl/098.copl"),
            include_str!("../../../copl/099.copl"),
            include_str!("../../../copl/100.copl"),
            include_str!("../../../copl/101.copl"),
            include_str!("../../../copl/102.copl"),
            include_str!("../../../copl/103.copl"),
            include_str!("../../../copl/104.copl"),
            include_str!("../../../copl/105.copl"),
            include_str!("../../../copl/106.copl"),
        ] {
            parse_source(source).expect("fixture should parse");
        }
    }

    #[test]
    fn accepts_unknown_rule_name_as_syntax() {
        let source = "|- 1 : int by T-Unknown {}";
        let parsed = parse_source(source).expect("parser should accept syntax");
        assert_eq!(parsed.rule_name, "T-Unknown");
    }

    #[test]
    fn records_derivation_spans_for_root_and_subderivations() {
        let source = r#"
|- if true then 1 else 2 : int by T-If {
  |- true : bool by T-Bool {};
  |- 1 : int by T-Int {};
  |- 2 : int by T-Int {};
}
"#;
        let parsed = parse_source(source).expect("parser should succeed");
        assert_eq!(parsed.span.line, 2);
        assert_eq!(parsed.span.column, 1);
        assert_eq!(parsed.subderivations[0].span.line, 3);
        assert_eq!(parsed.subderivations[0].span.column, 3);
    }

    #[test]
    fn parses_match_expression_shape() {
        let source = "|- match x with [] -> 0 | a :: b -> a : int by T-Unknown {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let TypingML4Judgment::HasType { expr, ty, .. } = parsed.judgment;
        assert_eq!(ty, TypingML4Type::Int);
        assert_eq!(
            expr,
            TypingML4Expr::Match {
                scrutinee: Box::new(TypingML4Expr::Var("x".to_string())),
                nil_case: Box::new(TypingML4Expr::Int(0)),
                head_name: "a".to_string(),
                tail_name: "b".to_string(),
                cons_case: Box::new(TypingML4Expr::Var("a".to_string())),
            }
        );
    }

    #[test]
    fn parses_function_and_list_types() {
        let source = "f : (int -> int) list -> bool |- f : (int -> int) list -> bool by T-Var {}";
        let parsed = parse_source(source).expect("parser should succeed");
        let TypingML4Judgment::HasType { env, ty, .. } = parsed.judgment;
        assert_eq!(env.0.len(), 1);
        assert_eq!(env.0[0].name, "f");
        assert_eq!(env.0[0].ty.to_string(), "(int -> int) list -> bool");
        assert_eq!(ty.to_string(), "(int -> int) list -> bool");
    }

    #[test]
    fn parses_judgment_only_input_for_prover() {
        let parsed =
            parse_judgment_source("|- fun x -> x + 1 : int -> int").expect("judgment should parse");
        assert_eq!(
            parsed,
            TypingML4Judgment::HasType {
                env: TypingML4Env::default(),
                expr: TypingML4Expr::Fun {
                    param: "x".to_string(),
                    body: Box::new(TypingML4Expr::BinOp {
                        op: TypingML4BinOp::Plus,
                        left: Box::new(TypingML4Expr::Var("x".to_string())),
                        right: Box::new(TypingML4Expr::Int(1)),
                    }),
                },
                ty: TypingML4Type::Fun {
                    param: Box::new(TypingML4Type::Int),
                    ret: Box::new(TypingML4Type::Int),
                },
            }
        );
    }

    #[test]
    fn rejects_derivation_input_in_judgment_only_parser() {
        let err = parse_judgment_source("|- 1 : int by T-Int {}")
            .expect_err("judgment-only parser should reject derivation");
        assert!(err.message().contains("unexpected trailing tokens"));
    }
}
