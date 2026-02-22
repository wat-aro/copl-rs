use crate::core::{CheckError, SourceSpan};

use super::lexer::{tokenize, Token, TokenKind};
use super::syntax::{Store, WhileAExp, WhileBExp, WhileCom, WhileDerivation, WhileJudgment};

pub fn parse_source(source: &str) -> Result<WhileDerivation, CheckError> {
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

pub(super) fn parse_judgment_source(source: &str) -> Result<WhileJudgment, CheckError> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CommandStop {
    Else,
    RBrace,
    Eof,
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_derivation(&mut self) -> Result<WhileDerivation, CheckError> {
        let span = self.peek().span.clone();
        let judgment = self.parse_judgment()?;
        self.expect_keyword_by()?;
        let rule_name = self.parse_rule_name()?;
        self.expect_lbrace()?;
        let subderivations = self.parse_subderivations()?;
        self.expect_rbrace()?;

        Ok(WhileDerivation {
            span,
            judgment,
            rule_name,
            subderivations,
        })
    }

    fn parse_judgment(&mut self) -> Result<WhileJudgment, CheckError> {
        let snapshot = self.index;
        if let Ok(store) = self.parse_store() {
            if self.consume_turnstile() {
                return self.parse_store_eval_judgment(store);
            }
        }
        self.index = snapshot;

        let command = self.parse_command(&[CommandStop::Eof, CommandStop::RBrace])?;
        self.expect_keyword_changes()?;
        let from = self.parse_store()?;
        self.expect_keyword_to()?;
        let to = self.parse_store()?;
        Ok(WhileJudgment::Changes { command, from, to })
    }

    fn parse_store_eval_judgment(&mut self, store: Store) -> Result<WhileJudgment, CheckError> {
        let snapshot = self.index;
        if let Ok(expr) = self.parse_aexp() {
            if self.consume_evalto() {
                let value = self.parse_int_literal()?;
                return Ok(WhileJudgment::AEval { store, expr, value });
            }
        }
        self.index = snapshot;

        let expr = self.parse_bexp()?;
        self.expect_evalto()?;
        let value = self.parse_bool_literal()?;
        Ok(WhileJudgment::BEval { store, expr, value })
    }

    fn parse_store(&mut self) -> Result<Store, CheckError> {
        if self.consume_dot() {
            return Ok(Store::empty());
        }

        let mut bindings = vec![self.parse_store_binding()?];
        while self.consume_comma() {
            bindings.push(self.parse_store_binding()?);
        }

        Ok(Store::new(bindings))
    }

    fn parse_store_binding(&mut self) -> Result<(String, i64), CheckError> {
        let name = self.parse_variable_name()?;
        self.expect_eq()?;
        let value = self.parse_int_literal()?;
        Ok((name, value))
    }

    fn parse_aexp(&mut self) -> Result<WhileAExp, CheckError> {
        self.parse_aexp_add_sub()
    }

    fn parse_aexp_add_sub(&mut self) -> Result<WhileAExp, CheckError> {
        let mut expr = self.parse_aexp_mul()?;
        loop {
            if self.consume_plus() {
                let right = self.parse_aexp_mul()?;
                expr = WhileAExp::Plus(Box::new(expr), Box::new(right));
                continue;
            }
            if self.consume_minus() {
                let right = self.parse_aexp_mul()?;
                expr = WhileAExp::Minus(Box::new(expr), Box::new(right));
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_aexp_mul(&mut self) -> Result<WhileAExp, CheckError> {
        let mut expr = self.parse_aexp_atom()?;
        while self.consume_times() {
            let right = self.parse_aexp_atom()?;
            expr = WhileAExp::Times(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_aexp_atom(&mut self) -> Result<WhileAExp, CheckError> {
        if self.consume_lparen() {
            let expr = self.parse_aexp()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        if let Some(value) = self.try_parse_int_literal()? {
            return Ok(WhileAExp::Int(value));
        }

        if let Some(name) = self.try_parse_variable_name() {
            return Ok(WhileAExp::Var(name));
        }

        Err(self.error_here("expected arithmetic expression"))
    }

    fn parse_bexp(&mut self) -> Result<WhileBExp, CheckError> {
        self.parse_bexp_or()
    }

    fn parse_bexp_or(&mut self) -> Result<WhileBExp, CheckError> {
        let mut expr = self.parse_bexp_and()?;
        while self.consume_or() {
            let right = self.parse_bexp_and()?;
            expr = WhileBExp::Or(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_bexp_and(&mut self) -> Result<WhileBExp, CheckError> {
        let mut expr = self.parse_bexp_not()?;
        while self.consume_and() {
            let right = self.parse_bexp_not()?;
            expr = WhileBExp::And(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn parse_bexp_not(&mut self) -> Result<WhileBExp, CheckError> {
        if self.consume_not() {
            let inner = self.parse_bexp_not()?;
            return Ok(WhileBExp::Not(Box::new(inner)));
        }
        self.parse_bexp_atom()
    }

    fn parse_bexp_atom(&mut self) -> Result<WhileBExp, CheckError> {
        if self.consume_true() {
            return Ok(WhileBExp::Bool(true));
        }
        if self.consume_false() {
            return Ok(WhileBExp::Bool(false));
        }
        if self.consume_lparen() {
            let expr = self.parse_bexp()?;
            self.expect_rparen()?;
            return Ok(expr);
        }

        let left = self.parse_aexp()?;
        if self.consume_less_equal() {
            let right = self.parse_aexp()?;
            return Ok(WhileBExp::Le(Box::new(left), Box::new(right)));
        }
        if self.consume_less() {
            let right = self.parse_aexp()?;
            return Ok(WhileBExp::Lt(Box::new(left), Box::new(right)));
        }
        if self.consume_eq() {
            let right = self.parse_aexp()?;
            return Ok(WhileBExp::Eq(Box::new(left), Box::new(right)));
        }

        Err(self.error_here("expected boolean expression"))
    }

    fn parse_command(&mut self, stoppers: &[CommandStop]) -> Result<WhileCom, CheckError> {
        self.parse_command_seq(stoppers)
    }

    fn parse_command_seq(&mut self, stoppers: &[CommandStop]) -> Result<WhileCom, CheckError> {
        let left = self.parse_command_atom(stoppers)?;
        if self.consume_semicolon() {
            if self.at_command_stop(stoppers) {
                return Err(self.error_here("expected command after ';'"));
            }
            let right = self.parse_command_seq(stoppers)?;
            Ok(WhileCom::Seq(Box::new(left), Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn parse_command_atom(&mut self, stoppers: &[CommandStop]) -> Result<WhileCom, CheckError> {
        if self.consume_skip() {
            return Ok(WhileCom::Skip);
        }

        if self.consume_if_keyword() {
            let cond = self.parse_bexp()?;
            self.expect_then()?;

            let mut then_stoppers = stoppers.to_vec();
            if !then_stoppers.contains(&CommandStop::Else) {
                then_stoppers.push(CommandStop::Else);
            }
            let then_branch = self.parse_command(&then_stoppers)?;
            self.expect_else()?;
            let else_branch = self.parse_command(stoppers)?;

            return Ok(WhileCom::If {
                cond,
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            });
        }

        if self.consume_while() {
            self.expect_lparen()?;
            let cond = self.parse_bexp()?;
            self.expect_rparen()?;
            self.expect_do()?;
            let body = self.parse_command(stoppers)?;

            return Ok(WhileCom::While {
                cond,
                body: Box::new(body),
            });
        }

        if self.at_command_stop(stoppers) {
            return Err(self.error_here("expected command"));
        }

        let name = self.parse_variable_name()?;
        self.expect_assign()?;
        let expr = self.parse_aexp()?;
        Ok(WhileCom::Assign { name, expr })
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

    fn parse_subderivations(&mut self) -> Result<Vec<WhileDerivation>, CheckError> {
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

    fn parse_variable_name(&mut self) -> Result<String, CheckError> {
        if let Some(name) = self.try_parse_variable_name() {
            Ok(name)
        } else {
            Err(self.error_here("expected variable name"))
        }
    }

    fn try_parse_variable_name(&mut self) -> Option<String> {
        let token = self.peek();
        let TokenKind::Identifier(name) = &token.kind else {
            return None;
        };
        let name = name.clone();
        self.bump();
        Some(name)
    }

    fn parse_int_literal(&mut self) -> Result<i64, CheckError> {
        self.try_parse_int_literal()?
            .ok_or_else(|| self.error_here("expected integer"))
    }

    fn try_parse_int_literal(&mut self) -> Result<Option<i64>, CheckError> {
        let negative = self.consume_minus();
        let token = self.peek();
        let TokenKind::Int(value) = token.kind else {
            if negative {
                return Err(self.error_here("expected digits after '-'"));
            }
            return Ok(None);
        };

        self.bump();
        if negative {
            value
                .checked_neg()
                .ok_or_else(|| self.error_here("integer is out of range"))
                .map(Some)
        } else {
            Ok(Some(value))
        }
    }

    fn parse_bool_literal(&mut self) -> Result<bool, CheckError> {
        if self.consume_true() {
            Ok(true)
        } else if self.consume_false() {
            Ok(false)
        } else {
            Err(self.error_here("expected boolean literal"))
        }
    }

    fn expect_keyword_by(&mut self) -> Result<(), CheckError> {
        if self.consume_by() {
            Ok(())
        } else {
            Err(self.error_here("expected 'by'"))
        }
    }

    fn expect_keyword_changes(&mut self) -> Result<(), CheckError> {
        if self.consume_changes() {
            Ok(())
        } else {
            Err(self.error_here("expected 'changes'"))
        }
    }

    fn expect_keyword_to(&mut self) -> Result<(), CheckError> {
        if self.consume_to() {
            Ok(())
        } else {
            Err(self.error_here("expected 'to'"))
        }
    }

    fn expect_evalto(&mut self) -> Result<(), CheckError> {
        if self.consume_evalto() {
            Ok(())
        } else {
            Err(self.error_here("expected 'evalto'"))
        }
    }

    fn expect_then(&mut self) -> Result<(), CheckError> {
        if self.consume_then() {
            Ok(())
        } else {
            Err(self.error_here("expected 'then'"))
        }
    }

    fn expect_else(&mut self) -> Result<(), CheckError> {
        if self.consume_else() {
            Ok(())
        } else {
            Err(self.error_here("expected 'else'"))
        }
    }

    fn expect_do(&mut self) -> Result<(), CheckError> {
        if self.consume_do() {
            Ok(())
        } else {
            Err(self.error_here("expected 'do'"))
        }
    }

    fn expect_assign(&mut self) -> Result<(), CheckError> {
        if self.consume_assign() {
            Ok(())
        } else {
            Err(self.error_here("expected ':='"))
        }
    }

    fn expect_eq(&mut self) -> Result<(), CheckError> {
        if self.consume_eq() {
            Ok(())
        } else {
            Err(self.error_here("expected '='"))
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

    fn at_command_stop(&self, stoppers: &[CommandStop]) -> bool {
        stoppers.iter().any(|stopper| match stopper {
            CommandStop::Else => matches!(self.peek().kind, TokenKind::Else),
            CommandStop::RBrace => matches!(self.peek().kind, TokenKind::RBrace),
            CommandStop::Eof => matches!(self.peek().kind, TokenKind::Eof),
        })
    }

    fn consume_by(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::By))
    }

    fn consume_changes(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Changes))
    }

    fn consume_to(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::To))
    }

    fn consume_evalto(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::EvalTo))
    }

    fn consume_then(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Then))
    }

    fn consume_else(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Else))
    }

    fn consume_do(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Do))
    }

    fn consume_skip(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Skip))
    }

    fn consume_kind(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        if predicate(&self.peek().kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn consume_if_keyword(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::If))
    }

    fn consume_while(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::While))
    }

    fn consume_true(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::True))
    }

    fn consume_false(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::False))
    }

    fn consume_turnstile(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Turnstile))
    }

    fn consume_assign(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Assign))
    }

    fn consume_less_equal(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::LessEqual))
    }

    fn consume_less(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Less))
    }

    fn consume_eq(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Eq))
    }

    fn consume_and(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::And))
    }

    fn consume_or(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Or))
    }

    fn consume_not(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Not))
    }

    fn consume_plus(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Plus))
    }

    fn consume_minus(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Minus))
    }

    fn consume_times(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Times))
    }

    fn consume_lparen(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::LParen))
    }

    fn consume_rparen(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::RParen))
    }

    fn consume_lbrace(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::LBrace))
    }

    fn consume_rbrace(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::RBrace))
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Comma))
    }

    fn consume_semicolon(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Semicolon))
    }

    fn consume_dot(&mut self) -> bool {
        self.consume_kind(|kind| matches!(kind, TokenKind::Dot))
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
    use crate::games::while_lang::syntax::{Store, WhileCom, WhileJudgment};

    #[test]
    fn parses_exam_judgments_151_to_160_for_prover() {
        for fixture in [
            include_str!("../../../copl/151.exam.copl"),
            include_str!("../../../copl/152.exam.copl"),
            include_str!("../../../copl/153.exam.copl"),
            include_str!("../../../copl/154.exam.copl"),
            include_str!("../../../copl/155.exam.copl"),
            include_str!("../../../copl/156.exam.copl"),
            include_str!("../../../copl/157.exam.copl"),
            include_str!("../../../copl/158.exam.copl"),
            include_str!("../../../copl/159.exam.copl"),
            include_str!("../../../copl/160.exam.copl"),
        ] {
            let parsed = parse_judgment_source(fixture).expect("fixture should parse");
            assert!(matches!(parsed, WhileJudgment::Changes { .. }));
        }
    }

    #[test]
    fn parses_store_eval_judgment() {
        let parsed =
            parse_judgment_source("x = 2, y = 1 |- x + y evalto 3").expect("judgment should parse");
        assert_eq!(
            parsed,
            WhileJudgment::AEval {
                store: Store::new(vec![("x".to_string(), 2), ("y".to_string(), 1)]),
                expr: crate::games::while_lang::syntax::WhileAExp::Plus(
                    Box::new(crate::games::while_lang::syntax::WhileAExp::Var(
                        "x".to_string()
                    )),
                    Box::new(crate::games::while_lang::syntax::WhileAExp::Var(
                        "y".to_string()
                    )),
                ),
                value: 3,
            }
        );
    }

    #[test]
    fn parses_derivation() {
        let source = "skip changes s = 0 to s = 0 by C-Skip {}";
        let derivation = parse_source(source).expect("derivation should parse");
        assert_eq!(derivation.rule_name, "C-Skip");
        assert_eq!(
            derivation.judgment,
            WhileJudgment::Changes {
                command: WhileCom::Skip,
                from: Store::new(vec![("s".to_string(), 0)]),
                to: Store::new(vec![("s".to_string(), 0)]),
            }
        );
    }
}
