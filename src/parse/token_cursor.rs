use std::iter::Peekable;

use crate::{lexer::TokenKind, Token};

#[derive(Clone)]
pub struct TokenCursor<I>
where
    I: Iterator<Item = Token>,
{
    pub tokens: Peekable<I>,
}

impl<I> TokenCursor<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> TokenCursor<I> {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    pub fn consume(&mut self, kind: &TokenKind) -> miette::Result<Option<Token>> {
        if let Some(token) = self.peek() {
            if *kind == token.kind {
                return Ok(self.advance());
            }
        }

        Err(miette::miette!(
            "expected: {:?}, found: {:?}",
            kind,
            self.peek().map(|t| t.kind)
        ))
    }

    pub fn match_any(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        if let Some(token) = self.peek() {
            if kinds.contains(&token.kind) {
                return self.advance();
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{tokenize, TokenKind};

    use super::*;

    #[test]
    fn test_cursor_advance() {
        let tokens = tokenize("(5 - ( 3 + 1))");

        let mut cursor = TokenCursor::new(tokens);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::OpenParen);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::Number(5.0));
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::Minus);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::OpenParen);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::Number(3.0));
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::Plus);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::Number(1.0));
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::CloseParen);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::CloseParen);
        assert_eq!(cursor.advance().unwrap().kind, TokenKind::EOF);
        assert!(cursor.advance().is_none());
    }
}
