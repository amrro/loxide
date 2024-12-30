use std::{
    collections::HashMap,
    fmt::{self, Formatter},
};

use miette::LabeledSpan;
use once_cell::sync::Lazy;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Dot,

    Minus,
    Plus,
    /// `;`
    Semi,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEq,
    /// `=`
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    // Literals.
    Identifier,
    Number(f64),
    String,
    // Keywords.
    And,
    Or,
    Class,
    If,
    Else,
    Fun,
    For,
    While,
    Nil,
    Print,
    Return,
    Super,
    This,
    True,
    False,
    Var,
    EOF,
}

static KEYWORDS: Lazy<HashMap<String, TokenKind>> = Lazy::new(|| {
    HashMap::from_iter(vec![
        ("and".to_string(), TokenKind::And),
        ("class".to_string(), TokenKind::Class),
        ("else".to_string(), TokenKind::Else),
        ("for".to_string(), TokenKind::For),
        ("fun".to_string(), TokenKind::Fun),
        ("if".to_string(), TokenKind::If),
        ("nil".to_string(), TokenKind::Nil),
        ("or".to_string(), TokenKind::Or),
        ("print".to_string(), TokenKind::Print),
        ("return".to_string(), TokenKind::Return),
        ("super".to_string(), TokenKind::Super),
        ("this".to_string(), TokenKind::This),
        ("true".to_string(), TokenKind::True),
        ("var".to_string(), TokenKind::Var),
        ("while".to_string(), TokenKind::While),
        ("false".to_string(), TokenKind::False),
    ])
});

pub struct Token<'a> {
    pub lexeme: &'a str,
    pub kind: TokenKind,
}

impl Token<'_> {
    #[inline]
    fn trim_quotes(text: &str) -> &str {
        text.trim_matches('"')
    }

    pub fn eof() -> Self {
        Self {
            lexeme: "\0",
            kind: TokenKind::EOF,
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexeme = self.lexeme;
        match self.kind {
            TokenKind::OpenParen => write!(f, "LEFT_PAREN {lexeme} null"),
            TokenKind::CloseParen => write!(f, "RIGHT_PAREN {lexeme} null"),
            TokenKind::OpenBrace => write!(f, "LEFT_BRACE {lexeme} null"),
            TokenKind::CloseBrace => write!(f, "RIGHT_BRACE {lexeme} null"),
            TokenKind::Comma => write!(f, "COMMA {lexeme} null"),
            TokenKind::Dot => write!(f, "DOT {lexeme} null"),
            TokenKind::Minus => write!(f, "MINUS {lexeme} null"),
            TokenKind::Plus => write!(f, "PLUS {lexeme} null"),
            TokenKind::Semi => write!(f, "SEMICOLON {lexeme} null"),
            TokenKind::Slash => write!(f, "SLASH {lexeme} null"),
            TokenKind::Star => write!(f, "STAR {lexeme} null"),
            TokenKind::Bang => write!(f, "BANG {lexeme} null"),
            TokenKind::BangEq => write!(f, "BANG_EQUAL {lexeme} null"),
            TokenKind::Eq => write!(f, "EQUAL {lexeme} null"),
            TokenKind::EqEq => write!(f, "EQUAL_EQUAL {lexeme} null"),
            TokenKind::Greater => write!(f, "GREATER {lexeme} null"),
            TokenKind::GreaterEq => write!(f, "GREATER_EQAUL {lexeme} null"),
            TokenKind::Less => write!(f, "LESS {lexeme} null"),
            TokenKind::LessEq => write!(f, "LESS_EQUAL {lexeme} null"),
            TokenKind::Identifier => write!(f, "IDENTIEIFIER {lexeme} null"),
            TokenKind::Number(n) => write!(f, "NUMBER {n} {lexeme}"),
            TokenKind::String => write!(f, "STRING {lexeme} {}", Token::trim_quotes(lexeme)),
            TokenKind::And => write!(f, "AND {lexeme} null"),
            TokenKind::Class => write!(f, "CLASS {lexeme} null"),
            TokenKind::Else => write!(f, "ELSE {lexeme} null"),
            TokenKind::False => write!(f, "FALSE {lexeme} null"),
            TokenKind::Fun => write!(f, "FUN {lexeme} null"),
            TokenKind::For => write!(f, "FOR {lexeme} null"),
            TokenKind::If => write!(f, "IF {lexeme} null"),
            TokenKind::Nil => write!(f, "NIL {lexeme} null"),
            TokenKind::Or => write!(f, "OR {lexeme} null"),
            TokenKind::Print => write!(f, "PRINT {lexeme} null"),
            TokenKind::Return => write!(f, "RETURN {lexeme} null"),
            TokenKind::Super => write!(f, "SUPER {lexeme} null"),
            TokenKind::This => write!(f, "THIS {lexeme} null"),
            TokenKind::True => write!(f, "TRUE {lexeme} null"),
            TokenKind::Var => write!(f, "VAR {lexeme} null"),
            TokenKind::While => write!(f, "WHILE {lexeme} null"),
            TokenKind::EOF => write!(f, "EOF eof null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    pub source: &'a str,
    pub offset: usize,
    pub len: usize,
}

impl<'a> From<&'a str> for Scanner<'a> {
    fn from(source: &'a str) -> Self {
        Self {
            source,
            offset: 0,
            len: source.len(),
        }
    }
}

impl<'a> Scanner<'a> {
    fn tokenize(&mut self, kind: TokenKind, len: usize) -> Token<'a> {
        let start = self.offset;
        self.offset += len;
        Token {
            lexeme: &self.source[start..self.offset],
            kind,
        }
    }

    fn string(&mut self) -> Token<'a> {
        let quote = self.source.chars().next().unwrap();
        let byte = quote.len_utf8();
        let rest = &self.source[self.offset + byte..];
        let mut lexeme_len = byte;
        for ch in rest.chars() {
            lexeme_len += ch.len_utf8();
            if ch == '"' {
                break;
            }
        }

        self.tokenize(TokenKind::String, lexeme_len)
    }

    fn slash_or_comment(&mut self) -> Option<Token<'a>> {
        let slash = &self.source[self.offset..].chars().next().unwrap();
        let byte = slash.len_utf8();
        let rest = &self.source[self.offset + 1..];
        if rest.starts_with('/') {
            let comment_len = rest.find('\n').unwrap_or(rest.len());
            self.offset += comment_len + byte;
            None
        } else {
            // this is slash divide token.
            Some(self.tokenize(TokenKind::Slash, byte))
        }
    }

    fn if_equal(&mut self, yes: TokenKind, no: TokenKind) -> Token<'a> {
        let byte = self.source.chars().next().unwrap().len_utf8();
        let rest = &self.source[self.offset + byte..];
        let rest_trimmed = rest.trim_start();
        if rest_trimmed.starts_with('=') {
            self.tokenize(yes, byte + (rest.len() - rest_trimmed.len()) + 1)
        } else {
            self.tokenize(no, byte)
        }
    }

    fn number(&mut self) -> miette::Result<Token<'a>> {
        let rest = &self.source[self.offset..];
        let end = rest
            .find(|c| !matches!(c, '.' | '0'..='9'))
            .unwrap_or(rest.len());
        let mut literal = &rest[..end];

        if literal.ends_with('.') {
            literal = if let Some((idx, _)) = literal.char_indices().next_back() {
                &literal[..idx]
            } else {
                literal
            }
        }
        let bytes = literal.chars().count();

        match literal.parse::<f64>() {
            Ok(n) => Ok(self.tokenize(TokenKind::Number(n), bytes)),
            Err(e) => Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(self.offset..self.offset + bytes, "this numeric literal"),
                ],
                "{e}",
            }
            .with_source_code(self.source.to_string())),
        }
    }

    fn identifier(&mut self) -> miette::Result<Token<'a>> {
        let rest = &self.source[self.offset..];
        let end = rest.find(|c: char| c.is_whitespace()).unwrap_or(rest.len());
        let identifier = &rest[..end];
        let bytes = identifier.chars().count();

        if let Some(&kind) = KEYWORDS.get(identifier) {
            return Ok(self.tokenize(kind, bytes));
        }

        // TODO: use regex crate to ensure the validity of the identifier's name.
        if identifier
            .chars()
            .any(|c| !matches!(c,'a'..='z' | 'A'..='Z' | '0'..='9' | '_' ))
        {
            return Err(miette::miette! {
            labels = vec![
                LabeledSpan::at(self.offset..self.offset + bytes, "Invalid identifier's name"),
            ],
            "Ensure Valid identifier's name"
                        }
            .with_source_code(self.source.to_string()));
        }

        Ok(self.tokenize(TokenKind::Identifier, identifier.chars().count()))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = miette::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let rest = &self.source[self.offset..];
            let ch = rest.chars().next()?;
            let bytes = ch.len_utf8();
            match ch {
                ch if ch.is_whitespace() => {
                    self.offset += bytes;
                    continue;
                }
                '(' => return Some(Ok(self.tokenize(TokenKind::OpenParen, bytes))),
                ')' => return Some(Ok(self.tokenize(TokenKind::CloseParen, bytes))),
                '{' => return Some(Ok(self.tokenize(TokenKind::OpenParen, bytes))),
                '}' => return Some(Ok(self.tokenize(TokenKind::CloseBrace, bytes))),
                ',' => return Some(Ok(self.tokenize(TokenKind::Comma, bytes))),
                '.' => return Some(Ok(self.tokenize(TokenKind::Dot, bytes))),
                '-' => return Some(Ok(self.tokenize(TokenKind::Minus, bytes))),
                '+' => return Some(Ok(self.tokenize(TokenKind::Plus, bytes))),
                ';' => return Some(Ok(self.tokenize(TokenKind::Semi, bytes))),
                '*' => return Some(Ok(self.tokenize(TokenKind::Star, bytes))),
                '<' => return Some(Ok(self.if_equal(TokenKind::LessEq, TokenKind::Less))),
                '>' => return Some(Ok(self.if_equal(TokenKind::GreaterEq, TokenKind::Greater))),
                '=' => return Some(Ok(self.if_equal(TokenKind::EqEq, TokenKind::Eq))),
                '!' => return Some(Ok(self.if_equal(TokenKind::BangEq, TokenKind::Bang))),
                '"' => return Some(Ok(self.string())),
                '/' => {
                    if let Some(token) = self.slash_or_comment() {
                        return Some(Ok(token));
                    }
                }
                '0'..='9' => return Some(self.number()),
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.identifier()),

                c => {
                    return Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(self.offset..self.offset + bytes, format!("Invalid token {c}")),
                    ],
                    "Invalid token detected."
                                }
                    .with_source_code(self.source.to_string())));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use std::path::Path;

    use super::*;

    fn parse_file<P: AsRef<Path>>(file_path: P) {
        let snapshot_name = format!(
            "parse__{}",
            Path::new(file_path.as_ref())
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
        );

        let file_content = std::fs::read_to_string(&file_path).unwrap();
        let scanner = Scanner::from(file_content.as_str());
        let tokens: miette::Result<Vec<Token>> = scanner.into_iter().collect();
        let tokens = tokens.unwrap();

        insta::with_settings!({
            description => &file_content,
            input_file => file_path,
            omit_expression => true,
        }, {
                insta::assert_debug_snapshot!(snapshot_name, tokens);
            })
    }

    #[test]
    fn test_strings() {
        parse_file("lox/scanning/strings.lox");
    }

    #[test]
    fn test_punctuators() {
        parse_file("lox/scanning/punctuators.lox");
    }

    #[test]
    fn test_identifiers() {
        parse_file("lox/scanning/identifiers.lox");
    }

    #[test]
    fn test_whitespace() {
        parse_file("lox/scanning/whitespace.lox");
    }

    #[test]
    fn test_keywords() {
        parse_file("lox/scanning/keywords.lox");
    }

    #[test]
    fn test_numbers() {
        parse_file("lox/scanning/numbers.lox");
    }
}
