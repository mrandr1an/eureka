use std::fmt::{Debug, Display};

use super::identifier::Identifier;

#[derive(PartialEq)]
pub enum Value<'val> {
    Id(Identifier<'val>),
    Real(i64),
    Char(char),
}

impl<'val> Display for Value<'val> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "<Value Char {}>", c),
            Value::Real(r) => write!(f, "<Value Real {}>", r),
            Value::Id(r) => write!(f, "<Value {}>", r),
        }
    }
}

impl<'val> Debug for Value<'val> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "<Value Char {}>", c),
            Value::Real(r) => write!(f, "<Value Real {}>", r),
            Value::Id(r) => write!(f, "<Value {}>", r),
        }
    }
}
