pub mod lexer;
pub mod token;

pub mod parse;
pub mod span;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LxError {
    #[error("[Line {0}] Error: {1}")]
    SyntaxError(usize, String),
}
