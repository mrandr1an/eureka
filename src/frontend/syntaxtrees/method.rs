use std::fmt::{Debug, Display};

use super::{exprs::Expr, identifier::Identifier, types::Type, variable::Variable};

pub struct Method<'a> {
    pub _type: Option<Type>,
    pub id: Identifier<'a>,
    pub params: Vec<Variable<'a>>,
    pub body: Expr<'a>,
}

impl<'a> Display for Method<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(_type) = &self._type {
            let _ = write!(f, "<Method {}: {} ( ", _type, self.id);
            for param in self.params.iter() {
                let _ = write!(f, "{}", param);
            }
            write!(f, ") {}", self.body)
        } else {
            let _ = write!(f, "<Method {} ( ", self.id);
            for param in self.params.iter() {
                let _ = write!(f, "{}", param);
            }
            write!(f, ") {}", self.body)
        }
    }
}

impl<'a> Debug for Method<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(_type) = &self._type {
            let _ = write!(f, "<Method {}: {} ( ", _type, self.id);
            for param in self.params.iter() {
                let _ = write!(f, "{}", param);
            }
            write!(f, ") {}", self.body)
        } else {
            let _ = write!(f, "<Method {} ( ", self.id);
            for param in self.params.iter() {
                let _ = write!(f, "{}", param);
            }
            write!(f, ") {}", self.body)
        }
    }
}
