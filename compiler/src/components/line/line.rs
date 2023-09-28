use std::collections::VecDeque;
use crate::components::token::tokentype::*;

/// A line is a collection of TokenTypes that also holds its position on the source code (on the module level,starting from one)
/// and its indentation for being sorted in a line familly.
#[derive(Clone)]
pub struct Line{
    pub number: usize,
    pub indent: usize,
    pub contents: VecDeque<TokenType>,
}

impl Line
{
    pub fn new(number: usize, indent: usize, contents: VecDeque<TokenType>) -> Self
    {
        Self
        {
            number,
            indent,
            contents,
        }
    }
}

