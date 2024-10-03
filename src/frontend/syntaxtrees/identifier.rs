use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::frontend::lexers::token::Lexeme;

#[derive(Clone)]
pub struct Identifier<'id>(Lexeme<'id>);

impl<'id> Display for Identifier<'id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.slice)
    }
}

impl<'id> Debug for Identifier<'id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id<{}>", self.0.slice)
    }
}

impl<'id> PartialEq for Identifier<'id> {
    fn eq(&self, other: &Self) -> bool {
        self.0.slice == other.0.slice
    }
}

impl<'id> Eq for Identifier<'id> {}

impl<'id> Hash for Identifier<'id> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.slice.hash(state)
    }
}
