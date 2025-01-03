pub mod lexer;
use crate::lexer::Token;

pub mod parse;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LxError {
    #[error("[Line {0}] Error: {1}")]
    SyntaxError(usize, String),
}
