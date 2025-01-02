#![allow(dead_code)]

use crate::{
    lex::{Token, TokenKind},
    token_cursor::TokenCursor,
};
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

enum LiteralValue<'a> {
    String(&'a str),
    Num(f64),
    Bool(bool),
    Nil,
}

impl fmt::Display for LiteralValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LiteralValue::String(string) => String::from(*string),
            LiteralValue::Num(n) => n.clone().to_string(),
            LiteralValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            LiteralValue::Nil => "nil".to_string(),
        };

        write!(f, "{text}")
    }
}

enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        op: Op,
        right: Box<Expr<'a>>,
    },

    Unary {
        op: Op,
        expr: Box<Expr<'a>>,
    },
    Grouping(Box<Expr<'a>>),
    Literal(LiteralValue<'a>),
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary { left, op, right } => write!(f, "({op} {left} {right})"),
            Expr::Unary { op, expr } => write!(f, "({op} {expr})"),
            Expr::Grouping(expr) => write!(f, "(group {expr})"),
            Expr::Literal(value) => write!(f, "{value}"),
        }
    }
}

struct Parser<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    cursor: TokenCursor<'a, I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            cursor: TokenCursor::new(tokens),
        }
    }

    pub fn expr(&mut self) -> miette::Result<Expr<'a>> {
        self.term()
    }

    pub fn term(&mut self) -> miette::Result<Expr<'a>> {
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

    pub fn factor(&mut self) -> miette::Result<Expr<'a>> {
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

    pub fn unary(&mut self) -> miette::Result<Expr<'a>> {
        if self.cursor.match_any(&[TokenKind::Minus]).is_some() {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary {
                op: Op::Minus,
                expr,
            });
        }

        self.primary()
    }

    pub fn primary(&mut self) -> miette::Result<Expr<'a>> {
        // TODO: Find a better way to match literal regardless of their value.
        if let Some(token) = self.cursor.peek() {
            if let TokenKind::Number(n) = token.kind {
                self.cursor.advance();
                return Ok(Expr::Literal(LiteralValue::Num(n)));
            }
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
    use crate::lex::Scanner;

    use super::*;

    #[test]
    fn test_parser_parse() {
        let source_code = "(5 - (3 - 1)) + -1";
        let expected = "(+ (group (- 5 (group (- 3 1)))) (- 1))";
        let scanner = Scanner::from(source_code);
        let tokens: miette::Result<Vec<Token>> = scanner.into_iter().collect();
        let tokens = tokens.unwrap();

        let mut parser = Parser::new(tokens.iter());
        let expr = parser.expr().unwrap();
        println!("{expr}");

        assert_eq!(expr.to_string(), expected);
    }
}
