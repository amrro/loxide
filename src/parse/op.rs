use std::fmt;

use miette::IntoDiagnostic;

use crate::token::TokenKind;

use super::{errors::RuntimeError, LitVal};

/// Operations
#[derive(Clone, Copy, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Mul,
    Divide,
    Bang,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    EqEq,
    BangEq,
}

impl TryFrom<&TokenKind> for Op {
    type Error = miette::Error;

    fn try_from(kind: &TokenKind) -> miette::Result<Self> {
        match kind {
            TokenKind::Plus => Ok(Op::Plus),
            TokenKind::Minus => Ok(Op::Minus),
            TokenKind::Star => Ok(Op::Mul),
            TokenKind::Slash => Ok(Op::Divide),
            TokenKind::Bang => Ok(Op::Bang),
            TokenKind::EqEq => Ok(Op::EqEq),
            TokenKind::BangEq => Ok(Op::BangEq),
            TokenKind::Less => Ok(Op::Less),
            TokenKind::LessEq => Ok(Op::LessEq),
            TokenKind::Greater => Ok(Op::Greater),
            TokenKind::GreaterEq => Ok(Op::GreaterEq),
            _ => Err(miette::miette!(
                "Token type {:?} cannot be convert into operator.",
                kind
            )),
        }
    }
}

impl Op {
    pub(crate) fn apply(&self, lhs: LitVal, rhs: LitVal) -> miette::Result<LitVal> {
        match self {
            Op::Plus => lhs.add(&rhs),
            Op::Minus => lhs.minus(&rhs),
            Op::Mul => lhs.mul(&rhs),
            Op::Divide => lhs.div(&rhs),
            Op::EqEq => Ok(LitVal::Bool(lhs == rhs)),
            Op::BangEq => Ok(LitVal::Bool(lhs != rhs)),
            Op::Less => lhs.less(&rhs),
            Op::LessEq => lhs.less_eq(&rhs),
            Op::Greater => lhs.greater(&rhs),
            Op::GreaterEq => lhs.greater_eq(&rhs),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: *self,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    pub(crate) fn negate(&self, value: LitVal) -> miette::Result<LitVal> {
        match (self, value) {
            (Op::Minus, LitVal::Num(num)) => Ok(LitVal::Num(-num)),
            (Op::Bang, LitVal::Bool(flag)) => Ok(LitVal::Bool(!flag)),
            (op, literal) => {
                Err(RuntimeError::UnsupportedUnaryOp { op: *op, literal }).into_diagnostic()
            }
        }
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Bang => write!(f, "!"),
            Op::Greater => write!(f, ">"),
            Op::GreaterEq => write!(f, ">="),
            Op::Less => write!(f, "<"),
            Op::LessEq => write!(f, "<="),
            Op::EqEq => write!(f, "=="),
            Op::BangEq => write!(f, "!="),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
