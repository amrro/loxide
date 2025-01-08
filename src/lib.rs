pub mod lexer;
pub mod span;
pub mod token;

pub mod parse;
pub use parse::interpret;

use thiserror::Error;
use token::Token;

#[derive(Debug, Error)]
pub enum LxError {
    #[error("[Line {0}] Error: {1}")]
    SyntaxError(usize, String),

    #[error("Operation can't be done to: {0:?} {0:?}")]
    UnsupportedOperation(Token, Token),
}
