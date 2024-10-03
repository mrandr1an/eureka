use crate::frontend::parsers::symboltable::Table;

use super::{function::Function, method::Method, procedure::Procedure};

pub struct Source<'a>(Vec<Node<'a>>);

pub struct Node<'a> {
    data: NodeKind<'a>,
    scope: Table<'a>,
}

pub enum NodeKind<'a> {
    Routine(Routine<'a>),
}

pub enum Routine<'a> {
    Function(Function<'a>),
    Method(Method<'a>),
    Procedure(Procedure<'a>),
}
