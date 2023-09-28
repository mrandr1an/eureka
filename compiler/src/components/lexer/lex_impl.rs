use super::lex::Lex;

use crate::components::line::scope::Scope;

use std::rc::Rc;
/// - Bottom->Up
/// - Left->Right
/// Iterator implementation for Lex 
impl Iterator for Lex
{
    type Item = Scope;
    fn next(&mut self) -> Option<Self::Item>
    {
        todo!()
    }
}
