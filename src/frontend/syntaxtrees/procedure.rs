use std::fmt::{Debug, Display};

use super::{exprs::Expr, identifier::Identifier, variable::Variable};

pub struct Procedure<'a> {
    pub id: Identifier<'a>,
    pub params: Vec<Variable<'a>>,
    pub body: Expr<'a>,
}

impl<'a> Display for Procedure<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "<Procedure {} ( ", self.id);
        for param in self.params.iter() {
            let _ = write!(f, "{}", param);
        }
        write!(f, ") {}", self.body)
    }
}

impl<'a> Debug for Procedure<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "<Procedure {} ", self.id);
        for param in self.params.iter() {
            let _ = write!(f, "{}", param);
        }
        write!(f, "{}", self.body)
    }
}
