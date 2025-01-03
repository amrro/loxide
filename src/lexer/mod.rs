mod cursor;

use std::{
    collections::HashMap,
    fmt::{self, Formatter},
};

use cursor::Cursor;
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
    Unkown,
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

#[derive(Clone)]
pub struct Token {
    pub lexeme: String,
    pub kind: TokenKind,
}

impl Token {
    fn new(lexeme: String, kind: TokenKind) -> Self {
        Self { lexeme, kind }
    }

    pub fn eof() -> Self {
        Self {
            lexeme: "\0".to_string(),
            kind: TokenKind::EOF,
        }
    }

    pub fn fromat_f64(value: f64) -> String {
        if value.fract() == 0.0 {
            format!("{value:.1}").to_string()
        } else {
            value.to_string()
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexeme = self.lexeme.clone();
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
            TokenKind::Number(n) => write!(f, "NUMBER {n} {}", Self::fromat_f64(n)),
            TokenKind::String => {
                write!(f, "STRING \"{lexeme}\" {lexeme}",)
            }
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
            TokenKind::Unkown => write!(f, "UNKOWN {lexeme} null"),
        }
    }
}

pub fn tokenize(source_code: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(source_code);
    let mut eof_reached = false;
    std::iter::from_fn(move || {
        if eof_reached {
            return None;
        }

        let token = cursor.advance_token();
        if token.kind == TokenKind::EOF {
            eof_reached = true;
        }
        Some(token)
    })
}

#[cfg(test)]
mod tests {

    use std::path::Path;

    use super::*;

    fn parse_file<P: AsRef<Path>>(file_path: P) {
        let snapshot_name = Path::new(file_path.as_ref())
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let file_content = std::fs::read_to_string(&file_path).unwrap();
        let tokens: Vec<Token> = tokenize(&file_content).collect();

        insta::with_settings!({
            description => &file_content,
            input_file => file_path,
            omit_expression => true,
            snapshot_path => "../snapshots/",
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
