use crate::token::{Token, TokenKind, KEYWORDS};
use std::str::Chars;

const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    chars: Chars<'a>,
    /// Represent how many chars has been processed for better diagnositics.
    offset: usize,
}

impl<'a> Cursor<'a> {
    pub(crate) fn new(souce_code: &'a str) -> Self {
        Self {
            chars: souce_code.chars(),
            offset: 0_usize,
        }
    }

    pub(crate) fn advance(&mut self) -> Option<char> {
        let next = self.chars.next()?;
        self.offset += 1;
        Some(next)
    }

    pub(crate) fn first(&mut self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub fn second(&mut self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    fn line_comment(&mut self) {
        while let Some(ch) = self.advance() {
            if ch == '\n' {
                break;
            }
        }
    }

    fn is_match(&mut self, ch: char) -> bool {
        ch == self.first()
    }

    fn number(&mut self, first_digit: char) -> Token {
        let mut lexeme = String::from(first_digit);
        let mut found_decimal_point = false;
        loop {
            let ch = self.first();
            if ch.is_ascii_digit() {
                lexeme.push(self.advance().unwrap());
            } else if ch == '.' && !found_decimal_point && self.second().is_ascii_digit() {
                found_decimal_point = true;
                lexeme.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        let value = lexeme.parse::<f64>().unwrap();
        Token::new(lexeme, TokenKind::Number(value), self.offset)
    }

    fn string(&mut self) -> Token {
        let mut lexeme = String::new();
        while let Some(ch) = self.advance() {
            if ch == '"' {
                break;
            }
            lexeme.push(ch);
        }

        Token::new(lexeme, TokenKind::String, self.offset)
    }

    pub(crate) fn identifier(&mut self, first_ch: char) -> Token {
        let mut lexeme = String::from(first_ch);
        while !self.first().is_whitespace() && self.first() != '.' {
            lexeme.push(self.advance().unwrap());
        }

        if let Some(&kind) = KEYWORDS.get(&lexeme) {
            Token::new(lexeme, kind, self.offset)
        } else {
            Token::new(lexeme, TokenKind::Identifier, self.offset)
        }
    }

    fn eat_whitespace(&mut self) {
        while self.first().is_whitespace() {
            self.advance();
        }
    }

    pub(crate) fn advance_token(&mut self) -> Token {
        let ch = match self.advance() {
            Some(c) => c,
            None => return Token::eof(),
        };

        let ch_str = ch.to_string();

        match ch {
            ch if ch.is_whitespace() => {
                self.eat_whitespace();
                self.advance_token()
            }
            '(' => Token::new(ch_str, TokenKind::OpenParen, self.offset),
            ')' => Token::new(ch_str, TokenKind::CloseParen, self.offset),
            '{' => Token::new(ch_str, TokenKind::OpenBrace, self.offset),
            '}' => Token::new(ch_str, TokenKind::CloseBrace, self.offset),
            ',' => Token::new(ch_str, TokenKind::Comma, self.offset),
            '.' => Token::new(ch_str, TokenKind::Dot, self.offset),
            '-' => Token::new(ch_str, TokenKind::Minus, self.offset),
            '+' => Token::new(ch_str, TokenKind::Plus, self.offset),
            ';' => Token::new(ch_str, TokenKind::Semi, self.offset),
            '*' => Token::new(ch_str, TokenKind::Star, self.offset),
            '<' => Token::new(ch_str, TokenKind::Less, self.offset),
            '>' => Token::new(ch_str, TokenKind::Greater, self.offset),
            '=' => Token::new(ch_str, TokenKind::Eq, self.offset),
            '!' => Token::new(ch_str, TokenKind::Bang, self.offset),
            '"' => self.string(),
            '/' => {
                if self.is_match('/') {
                    self.line_comment();
                    return self.advance_token();
                }
                Token::new(ch.to_string(), TokenKind::Slash, self.offset)
            }
            '0'..='9' => self.number(ch),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(ch),
            _ => Token::new(ch_str, TokenKind::Unkown, self.offset),
        }
    }
}
