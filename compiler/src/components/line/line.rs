use std::collections::VecDeque;
use crate::components::token::tokentype::*;
use crate::components::lexer::lex;

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

    pub fn init() -> Self
    {
        Self
        {
            number: 0,
            indent: 0,
            contents: VecDeque::new(),
        }
    }
}

