#![allow(dead_code)]
pub mod errors;

mod token_cursor;

use crate::{
    lexer::tokenize,
    token::{Token, TokenKind},
};
use errors::RuntimeError;
use miette::IntoDiagnostic;
use std::fmt::{self};
use token_cursor::TokenCursor;

/// Operations
#[derive(Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Mul,
    Divide,
    Bang,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    EqEq,
    BangEq,
}

impl TryFrom<&TokenKind> for Op {
    type Error = miette::Error;

    fn try_from(kind: &TokenKind) -> miette::Result<Self> {
        match kind {
            TokenKind::Plus => Ok(Op::Plus),
            TokenKind::Minus => Ok(Op::Minus),
            TokenKind::Star => Ok(Op::Mul),
            TokenKind::Bang => Ok(Op::Bang),
            TokenKind::EqEq => Ok(Op::EqEq),
            TokenKind::BangEq => Ok(Op::BangEq),
            TokenKind::Less => Ok(Op::Less),
            TokenKind::LessEq => Ok(Op::LessEq),
            TokenKind::Greater => Ok(Op::Greater),
            TokenKind::GreaterEq => Ok(Op::GreaterEq),
            _ => Err(miette::miette!(
                "Token type {:?} cannot be convert into operator.",
                kind
            )),
        }
    }
}

impl Op {
    fn apply(&self, lhs: LitVal, rhs: LitVal) -> miette::Result<LitVal> {
        match self {
            Op::Plus => lhs.add(&rhs),
            Op::Minus => lhs.minus(&rhs),
            Op::Mul => lhs.mul(&rhs),
            Op::Divide => lhs.div(&rhs),
            Op::EqEq => Ok(LitVal::Bool(lhs == rhs)),
            Op::BangEq => Ok(LitVal::Bool(lhs != rhs)),
            Op::Less => lhs.less(&rhs),
            Op::LessEq => lhs.less_eq(&rhs),
            Op::Greater => lhs.greater(&rhs),
            Op::GreaterEq => lhs.greater_eq(&rhs),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: *self,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn negate(&self, value: LitVal) -> miette::Result<LitVal> {
        match (self, value) {
            (Op::Minus, LitVal::Num(num)) => Ok(LitVal::Num(-num)),
            (Op::Bang, LitVal::Bool(flag)) => Ok(LitVal::Bool(!flag)),
            (op, literal) => {
                Err(RuntimeError::UnsupportedUnaryOp { op: *op, literal }).into_diagnostic()
            }
        }
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Bang => write!(f, "!"),
            Op::Greater => write!(f, ">"),
            Op::GreaterEq => write!(f, ">="),
            Op::Less => write!(f, "<"),
            Op::LessEq => write!(f, "<="),
            Op::EqEq => write!(f, "=="),
            Op::BangEq => write!(f, "!="),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, PartialEq)]
pub enum LitVal {
    String(String),
    Num(f64),
    Bool(bool),
    Nil,
}

impl LitVal {
    fn add(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::String(first), LitVal::String(second)) => {
                Ok(LitVal::String(first.to_owned() + second))
            }
            (LitVal::Num(n_lhs), LitVal::Num(n_rhs)) => Ok(LitVal::Num(*n_lhs + *n_rhs)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Plus,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn minus(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(n_rhs), LitVal::Num(n_lhs)) => Ok(LitVal::Num(*n_rhs - *n_lhs)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Minus,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn mul(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(left * right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Mul,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn div(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(left / right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Divide,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn compare_num<F>(&self, rhs: &Self, predicate: F) -> miette::Result<Self>
    where
        F: FnOnce(&f64, &f64) -> bool,
    {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Bool(predicate(right, left))),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Divide,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::eq)
    }

    fn less(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::lt)
    }

    fn less_eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::le)
    }

    fn greater(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::gt)
    }

    fn greater_eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::ge)
    }
}

impl fmt::Debug for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitVal::String(string) => write!(f, "(String) {}", string),
            LitVal::Num(n) => write!(f, "(Num) {}", n.clone()),
            LitVal::Bool(b) => write!(f, "(bool) {}", if *b { "true" } else { "false" }),
            LitVal::Nil => write!(f, "nil"),
        }
    }
}

impl fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LitVal::String(string) => String::from(string),
            LitVal::Num(n) => n.clone().to_string(),
            LitVal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            LitVal::Nil => "nil".to_string(),
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

        if let TokenKind::True = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LitVal::Bool(true)));
        }

        if let TokenKind::False = token.kind {
            self.cursor.advance();
            return Ok(Expr::Literal(LitVal::Bool(false)));
        }

        if let TokenKind::Nil = token.kind {
            self.cursor.advance();
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

#[test]
fn test_comparison() {
    let literal = tokenize("3 >= 3");
    for t in literal {
        println!("{t}");
    }
    assert!(false);
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
        if let LitVal::Num(num) = literal {
            assert_eq!(num, 2.0);
        }
    }
}
