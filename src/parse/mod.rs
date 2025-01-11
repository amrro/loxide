#![allow(dead_code)]
pub mod errors;

mod literal;
mod op;
mod token_cursor;

use crate::{
    lexer::tokenize,
    token::{Token, TokenKind},
};
use literal::LitVal;
use op::Op;
use std::fmt::{self};
use token_cursor::TokenCursor;

enum Expr {
    Binary {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },

    Unary {
        op: Op,
        expr: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(LitVal),
}

impl Expr {
    fn evalute(&self) -> miette::Result<LitVal> {
        match self {
            Expr::Binary { left, op, right } => {
                let left_literal = left.evalute()?;
                let right_literal = right.evalute()?;
                op.apply(right_literal, left_literal)
            }
            Expr::Unary { op, expr } => op.negate(expr.evalute()?),
            Expr::Grouping(expr) => expr.evalute(),
            Expr::Literal(literal) => Ok(literal.clone()),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary { left, op, right } => write!(f, "({op} {left} {right})"),
            Expr::Unary { op, expr } => write!(f, "({op} {expr})"),
            Expr::Grouping(expr) => write!(f, "(group {expr})"),
            Expr::Literal(value) => write!(f, "{value}"),
        }
    }
}

struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    cursor: TokenCursor<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            cursor: TokenCursor::new(tokens),
        }
    }

    pub fn expr(&mut self) -> miette::Result<Expr> {
        self.equality()
    }

    pub fn equality(&mut self) -> miette::Result<Expr> {
        let left = self.comparison()?;
        if let Some(token) = self.cursor.match_any(&[TokenKind::EqEq, TokenKind::BangEq]) {
            let op = Op::try_from(&token.kind)?;
            let right = Box::new(self.comparison()?);
            return Ok(Expr::Binary {
                left: Box::new(left),
                op,
                right,
            });
        }

        Ok(left)
    }

    pub fn comparison(&mut self) -> miette::Result<Expr> {
        let left = self.term()?;
        if let Some(token) = self.cursor.match_any(&[
            TokenKind::Less,
            TokenKind::LessEq,
            TokenKind::Greater,
            TokenKind::GreaterEq,
        ]) {
            let op = Op::try_from(&token.kind)?;
            let right = Box::new(self.term()?);
            return Ok(Expr::Binary {
                left: Box::new(left),
                op,
                right,
            });
        }

        Ok(left)
    }

    pub fn term(&mut self) -> miette::Result<Expr> {
        let left = self.factor()?;

        if let Some(op) = self
            .cursor
            .match_any(&[TokenKind::Plus, TokenKind::Minus])
            .map(|t| Op::try_from(&t.kind))
        {
            let right = Box::new(self.factor()?);
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: op?,
                right,
            });
        }

        Ok(left)
    }

    pub fn factor(&mut self) -> miette::Result<Expr> {
        let left = self.unary()?;
        if let Some(op) = self
            .cursor
            .match_any(&[TokenKind::Star, TokenKind::Slash])
            .map(|t| Op::try_from(&t.kind))
        {
            let right = Box::new(self.unary()?);
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: op?,
                right,
            });
        }

        Ok(left)
    }

    pub fn unary(&mut self) -> miette::Result<Expr> {
        if self.cursor.match_any(&[TokenKind::Minus]).is_some() {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary {
                op: Op::Minus,
                expr,
            });
        }

        if self.cursor.match_any(&[TokenKind::Bang]).is_some() {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary { op: Op::Bang, expr });
        }

        self.primary()
    }

    pub fn primary(&mut self) -> miette::Result<Expr> {
        let token = self.cursor.peek();

        if let TokenKind::Number(n) = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LitVal::Num(n)));
        }

        if self.cursor.match_any(&[TokenKind::True]).is_some() {
            return Ok(Expr::Literal(LitVal::Bool(true)));
        }

        if self.cursor.match_any(&[TokenKind::False]).is_some() {
            return Ok(Expr::Literal(LitVal::Bool(false)));
        }

        if self.cursor.match_any(&[TokenKind::Nil]).is_some() {
            return Ok(Expr::Literal(LitVal::Nil));
        }

        if self.cursor.match_any(&[TokenKind::OpenParen]).is_some() {
            let expr = self.expr()?;
            self.cursor.consume(&TokenKind::CloseParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(miette::miette!("Uknown Token: {:?}", self.cursor.peek()))
    }
}

pub fn interpret(source_code: &str) -> miette::Result<LitVal> {
    let tokens = tokenize(source_code);
    let mut parser = Parser::new(tokens);
    parser.expr()?.evalute()
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_parser_parse() {
        let expected = "(+ (group (- 5 (group (- 3 1)))) (- 1))";
        let tokens = tokenize("(5 - (3 - 1)) + -1");

        let mut parser = Parser::new(tokens);
        let expr = parser.expr().unwrap();
        println!("{expr}");

        assert_eq!(expr.to_string(), expected);
    }

    fn test_expr_evalute() {
        let literal = interpret("(5 - (3 - 1)) + -1").unwrap();
        if let LitVal::Num(num) = literal {
            assert_eq!(num, 2.0);
        }
    }
}
