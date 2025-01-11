use std::fmt;

use miette::IntoDiagnostic as _;

use super::{errors::RuntimeError, op::Op};

#[derive(Clone, PartialEq)]
pub enum LitVal {
    String(String),
    Num(f64),
    Bool(bool),
    Nil,
}

impl LitVal {
    pub(crate) fn add(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::String(first), LitVal::String(second)) => {
                Ok(LitVal::String(first.to_owned() + second))
            }
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(*left + *right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Plus,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    pub(crate) fn minus(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(*left - *right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Minus,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    pub(crate) fn mul(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(left * right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Mul,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    pub(crate) fn div(&self, rhs: &Self) -> miette::Result<Self> {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Num(left / right)),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Divide,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    fn compare_num<F>(&self, rhs: &Self, predicate: F) -> miette::Result<Self>
    where
        F: FnOnce(&f64, &f64) -> bool,
    {
        match (self, rhs) {
            (LitVal::Num(left), LitVal::Num(right)) => Ok(LitVal::Bool(predicate(right, left))),
            _ => Err(RuntimeError::UnsupportedBinaryOp {
                op: Op::Divide,
                lhs: self.clone(),
                rhs: rhs.clone(),
            })
            .into_diagnostic(),
        }
    }

    pub(crate) fn eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::eq)
    }

    pub(crate) fn less(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::lt)
    }

    pub(crate) fn less_eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::le)
    }

    pub(crate) fn greater(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::gt)
    }

    pub(crate) fn greater_eq(&self, rhs: &Self) -> miette::Result<Self> {
        self.compare_num(rhs, f64::ge)
    }
}

impl fmt::Debug for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitVal::String(string) => write!(f, "(String) {}", string),
            LitVal::Num(n) => write!(f, "(Num) {}", n.clone()),
            LitVal::Bool(b) => write!(f, "(bool) {}", if *b { "true" } else { "false" }),
            LitVal::Nil => write!(f, "nil"),
        }
    }
}

impl fmt::Display for LitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            LitVal::String(string) => String::from(string),
            LitVal::Num(n) => n.clone().to_string(),
            LitVal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            LitVal::Nil => "nil".to_string(),
        };

        write!(f, "{text}")
    }
}
