pub mod lexer;
pub mod span;
pub mod token;

pub mod parse;
pub use parse::interpret;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LxError {
    #[error("[Line {0}] Error: {1}")]
    SyntaxError(usize, String),
}
