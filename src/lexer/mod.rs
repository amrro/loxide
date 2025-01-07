mod cursor;

use crate::token::{Token, TokenKind};
use cursor::Cursor;

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
