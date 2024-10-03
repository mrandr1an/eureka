use std::fmt::{Debug, Display};

use super::{exprs::Expr, identifier::Identifier, types::Type, variable::Variable};

pub struct Function<'a> {
    pub _type: Type,
    pub id: Identifier<'a>,
    pub params: Vec<Variable<'a>>,
    pub body: Expr<'a>,
}

impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "<Function {}: {} ( ", self._type, self.id);
        for param in self.params.iter() {
            let _ = write!(f, "{}", param);
        }
        write!(f, ") {}", self.body)
    }
}

impl<'a> Debug for Function<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "<Function {} ", self.id);
        for param in self.params.iter() {
            let _ = write!(f, "{}", param);
        }
        write!(f, "{}", self.body)
    }
}
