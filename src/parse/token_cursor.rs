use std::iter::Peekable;

use crate::token::{Token, TokenKind};

const EOF_CHAR: char = '\0';

#[derive(Clone)]
pub struct TokenCursor<I>
where
    I: Iterator<Item = Token>,
{
    pub tokens: Peekable<I>,
    pub offset: usize,
}

impl<I> TokenCursor<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> TokenCursor<I> {
        Self {
            tokens: tokens.peekable(),
            offset: 0_usize,
        }
    }

    pub fn advance(&mut self) -> Option<Token> {
        let next = self.tokens.next()?;
        self.offset += next.span.len();
        Some(next)
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.peek().unwrap_or(&Token::eof()).clone()
    }

    pub fn consume(&mut self, kind: &TokenKind) -> miette::Result<Option<Token>> {
        if self.peek().kind == *kind {
            return Ok(self.advance());
        }

        Err(miette::miette!(
            "expected: {:?} after experession, found: {:?}",
            kind,
            self.peek().kind
        ))
    }

    pub fn match_any(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        if kinds.contains(&self.peek().kind) {
            return self.advance();
        }

        None
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().lexeme == EOF_CHAR.to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokenize;
    use crate::token::TokenKind;

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
