#![allow(dead_code)]
mod token_cursor;

use crate::{
    lexer::tokenize,
    token::{Token, TokenKind},
};
use std::fmt::{self};
use token_cursor::TokenCursor;

/// represents the Operations
enum Op {
    Plus,
    Minus,
    Mul,
    Divide,
    Bang,
}

impl TryFrom<&TokenKind> for Op {
    type Error = miette::Error;

    fn try_from(kind: &TokenKind) -> miette::Result<Self> {
        match kind {
            TokenKind::Plus => Ok(Op::Plus),
            TokenKind::Minus => Ok(Op::Minus),
            TokenKind::Star => Ok(Op::Mul),
            TokenKind::Bang => Ok(Op::Bang),
            _ => Err(miette::miette!(
                "Token type {:?} cannot be convert into operator.",
                kind
            )),
        }
    }
}

impl Op {
    fn apply(&self, lhs: LiteralValue, rhs: LiteralValue) -> miette::Result<LiteralValue> {
        match self {
            Op::Plus => lhs.add(&rhs),
            Op::Minus => lhs.minus(&rhs),
            Op::Mul => lhs.mul(&rhs),
            Op::Divide => lhs.div(&rhs),
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }

    fn negate(&self, value: LiteralValue) -> miette::Result<LiteralValue> {
        match (self, value) {
            (Op::Minus, LiteralValue::Num(num)) => Ok(LiteralValue::Num(-num)),
            (Op::Bang, LiteralValue::Bool(flag)) => Ok(LiteralValue::Bool(!flag)),
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Bang => write!(f, "!"),
        }
    }
}

#[derive(Clone)]
pub enum LiteralValue {
    String(String),
    Num(f64),
    Bool(bool),
    Nil,
}

impl LiteralValue {
    fn add(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LiteralValue::String(first), LiteralValue::String(second)) => {
                Ok(LiteralValue::String(first.to_owned() + second))
            }
            (LiteralValue::Num(n_lhs), LiteralValue::Num(n_rhs)) => {
                Ok(LiteralValue::Num(*n_lhs + *n_rhs))
            }
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }

    fn minus(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LiteralValue::Num(n_rhs), LiteralValue::Num(n_lhs)) => {
                Ok(LiteralValue::Num(*n_rhs - *n_lhs))
            }
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }

    fn mul(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LiteralValue::Num(left), LiteralValue::Num(right)) => {
                Ok(LiteralValue::Num(left * right))
            }
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }

    fn div(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LiteralValue::Num(left), LiteralValue::Num(right)) => {
                Ok(LiteralValue::Num(left / right))
            }
            // TODO: A new error emerges: unsupportedOperation.
            _ => Err(miette::miette!("unsupported Operations")),
        }
    }

    fn negate(&self) -> miette::Result<Self> {
        if let LiteralValue::Bool(flag) = self {
            return Ok(LiteralValue::Bool(!flag));
        }

        // TODO: A new error emerges: unsupportedOperation.
        Err(miette::miette!("unsupported Operations"))
    }
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LiteralValue::String(string) => String::from(string),
            LiteralValue::Num(n) => n.clone().to_string(),
            LiteralValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            LiteralValue::Nil => "nil".to_string(),
        };

        write!(f, "{text}")
    }
}

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
    Literal(LiteralValue),
}

impl Expr {
    fn evalute(&self) -> miette::Result<LiteralValue> {
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
        self.term()
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

        self.primary()
    }

    pub fn primary(&mut self) -> miette::Result<Expr> {
        let token = self.cursor.peek();

        if let TokenKind::Number(n) = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LiteralValue::Num(n)));
        }

        if let TokenKind::True = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LiteralValue::Bool(true)));
        }

        if let TokenKind::False = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LiteralValue::Bool(false)));
        }

        if let TokenKind::Nil = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LiteralValue::Nil));
        }

        if self.cursor.match_any(&[TokenKind::OpenParen]).is_some() {
            let expr = self.expr()?;
            self.cursor.consume(&TokenKind::CloseParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(miette::miette!("Uknown Token: {:?}", self.cursor.peek()))
    }
}

pub fn interpret(source_code: &str) -> miette::Result<LiteralValue> {
    let tokens = tokenize(source_code);
    let mut parser = Parser::new(tokens);
    parser.expr()?.evalute()
}

#[cfg(test)]
mod tests {

    use crate::lexer::tokenize;

    use super::*;

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
        if let LiteralValue::Num(num) = literal {
            assert_eq!(num, 2.0);
        }
    }
}
