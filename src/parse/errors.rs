use super::{LitVal, Op};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Cannot perform operation {op} on types {lhs} and {rhs}")]
    UnsupportedBinaryOp { op: Op, lhs: LitVal, rhs: LitVal },

    #[error("Cannot apply operation {op} on type {literal:?}")]
    UnsupportedUnaryOp { op: Op, literal: LitVal },
}
