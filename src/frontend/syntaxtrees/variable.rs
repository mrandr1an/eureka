use std::fmt::{Debug, Display};

use super::{identifier::Identifier, types::Type};

#[derive(PartialEq)]
pub struct Variable<'var> {
    pub name: Identifier<'var>,
    pub _type: Type,
}

impl<'var> Display for Variable<'var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self._type)
    }
}

impl<'var> Debug for Variable<'var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Variable {}: {}>", self.name, self._type)
    }
}
