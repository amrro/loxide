pub mod lex;
use crate::lex::Token;

pub mod parse;

pub mod token_cursor;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LxError {
    #[error("[Line {0}] Error: {1}")]
    SyntaxError(usize, String),
}
