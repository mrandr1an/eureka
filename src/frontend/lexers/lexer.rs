use std::iter::Peekable;

use super::{
    greek::Greek,
    token::{Greek as G, Token},
};

pub struct Tokens<'lexer, Language, Lexer: Iterator<Item = Token<'lexer, Language>>> {
    pub name: &'lexer str,
    pub len: usize,
    pub src: &'lexer str,
    inner: Peekable<Lexer>,
}

impl<'lexer> Tokens<'lexer, G, Greek<'lexer>> {
    pub fn new(input: &'lexer str, name: &'lexer str) -> Self {
        Self {
            name,
            len: name.len(),
            src: input,
            inner: Greek::new(input).peekable(),
        }
    }
}

impl<'lexer, Language, Lexer: Iterator<Item = Token<'lexer, Language>>>
    Tokens<'lexer, Language, Lexer>
{
    pub fn next(&mut self) -> Option<Token<'lexer, Language>> {
        self.inner.next()
    }

    pub fn peek(&mut self) -> Option<&Token<'lexer, Language>> {
        self.inner.peek()
    }
}
