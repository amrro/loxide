use super::{Token, TokenKind, KEYWORDS};
use std::str::Chars;

const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub(crate) fn new(souce_code: &'a str) -> Self {
        Self {
            chars: souce_code.chars(),
        }
    }

    pub(crate) fn advance(&mut self) -> Option<char> {
        self.chars.next()
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
        Token::new(lexeme, TokenKind::Number(value))
    }

    fn string(&mut self) -> Token {
        let mut lexeme = String::new();
        while let Some(ch) = self.advance() {
            if ch == '"' {
                break;
            }
            lexeme.push(ch);
        }

        Token::new(lexeme, TokenKind::String)
    }

    pub(crate) fn identifier(&mut self, first_ch: char) -> Token {
        let mut lexeme = String::from(first_ch);
        while !self.first().is_whitespace() && self.first() != '.' {
            lexeme.push(self.advance().unwrap());
        }

        if let Some(&kind) = KEYWORDS.get(&lexeme) {
            Token::new(lexeme, kind)
        } else {
            Token::new(lexeme, TokenKind::Identifier)
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
            '(' => Token::new(ch_str, TokenKind::OpenParen),
            ')' => Token::new(ch_str, TokenKind::CloseParen),
            '{' => Token::new(ch_str, TokenKind::OpenBrace),
            '}' => Token::new(ch_str, TokenKind::CloseBrace),
            ',' => Token::new(ch_str, TokenKind::Comma),
            '.' => Token::new(ch_str, TokenKind::Dot),
            '-' => Token::new(ch_str, TokenKind::Minus),
            '+' => Token::new(ch_str, TokenKind::Plus),
            ';' => Token::new(ch_str, TokenKind::Semi),
            '*' => Token::new(ch_str, TokenKind::Star),
            '<' => Token::new(ch_str, TokenKind::Less),
            '>' => Token::new(ch_str, TokenKind::Greater),
            '=' => Token::new(ch_str, TokenKind::Eq),
            '!' => Token::new(ch_str, TokenKind::Bang),
            '"' => self.string(),
            '/' => {
                if self.is_match('/') {
                    self.line_comment();
                    return self.advance_token();
                }
                Token::new(ch.to_string(), TokenKind::Slash)
            }
            '0'..='9' => self.number(ch),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(ch),
            _ => Token::new(ch_str, TokenKind::Unkown),
        }
    }
}
