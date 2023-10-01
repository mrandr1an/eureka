use super::lex::Lex;

use crate::components::line::scope::Scope;

use std::rc::Rc;
/// - Bottom->Up
/// - Left->Right
/// Iterator implementation for Lex 
struct IterLex
{
    lex: Lex,
}
