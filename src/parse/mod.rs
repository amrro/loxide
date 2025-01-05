#![allow(dead_code)]
mod token_cursor;

use token_cursor::TokenCursor;

use crate::lexer::{Token, TokenKind};
use std::fmt::{self};
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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Mul => write!(f, "-"),
            Op::Divide => write!(f, "/"),
            Op::Bang => write!(f, "!"),
        }
    }
}

enum LiteralValue {
    String(String),
    Num(f64),
    Bool(bool),
    Nil,
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
        use TokenKind::*;

        let left = self.factor()?;

        if let Some(op) = self
            .cursor
            .match_any(&[Plus, Minus])
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
        use TokenKind::*;

        let left = self.unary()?;
        if let Some(op) = self
            .cursor
            .match_any(&[Star, Slash])
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
}
