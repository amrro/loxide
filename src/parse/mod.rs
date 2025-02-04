#![allow(dead_code)]
pub mod errors;

mod literal;
mod op;
pub mod stmt;
mod token_cursor;

use crate::{
    lexer::tokenize,
    token::{Token, TokenKind},
};
use literal::LitVal;
use op::Op;
use std::fmt::{self};
use stmt::Stmt;
use token_cursor::TokenCursor;

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
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
                op.apply(left_literal, right_literal)
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

    pub fn parse(&mut self) -> miette::Result<Vec<Stmt>> {
        let mut stmts = Vec::new();

        while !self.cursor.at_end() {
            stmts.push(self.advance_stmt()?);
        }
        Ok(stmts)
    }

    pub fn advance_stmt(&mut self) -> miette::Result<Stmt> {
        if self.cursor.match_any(&[TokenKind::Print]).is_some() {
            let expr = self.expr()?;
            self.cursor.consume(&TokenKind::Semi)?;
            return Ok(Stmt::Print(expr));
        }

        let expr = self.expr()?;
        self.cursor.consume(&TokenKind::Semi)?;
        Ok(Stmt::Expr(expr))
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
        let token = self.cursor.advance().unwrap();

        match token.kind {
            TokenKind::Number(n) => Ok(Expr::Literal(LitVal::Num(n))),
            TokenKind::String => Ok(Expr::Literal(LitVal::String(token.lexeme))),
            TokenKind::True => Ok(Expr::Literal(LitVal::Bool(true))),
            TokenKind::False => Ok(Expr::Literal(LitVal::Bool(false))),
            TokenKind::Nil => Ok(Expr::Literal(LitVal::Nil)),
            TokenKind::OpenParen => {
                let expr = self.expr()?;
                self.cursor.consume(&TokenKind::CloseParen)?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(miette::miette!("Unkown Token: {:?}", self.cursor.peek())),
        }
    }
}

pub fn interpret(source_code: &str) -> miette::Result<LitVal> {
    let tokens = tokenize(source_code);
    let mut parser = Parser::new(tokens);
    parser.expr()?.evalute()
}

pub fn parse(source_code: &str) -> miette::Result<()> {
    let tokens = tokenize(source_code);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse()?;

    Stmt::execute(&stmts)?;

    Ok(())
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

    #[test]
    fn test_parse_stmts() {
        let source_code = "print 5; 2 + 3;";
        let tokens = tokenize(source_code);
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().unwrap();
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn test_stmt_execute() {
        let source_code = "print 5; 2 + 3;";
        parse(source_code).unwrap();
    }
}
