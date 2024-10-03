use miette::Diagnostic;
use std::error::Error as Err;

use crate::frontend::{
    lexers::{
        lexer::Lexer,
        token::{Token, TokenVariant},
    },
    syntaxtrees::value::Value,
};

pub trait Parser<'parser, Input: 'parser, Error: Err + Diagnostic> {
    type Output;
    fn parse(
        &'parser mut self,
        input: &'parser mut Input,
    ) -> miette::Result<(Self::Output, &'parser mut Input), Error>;

    fn map<F, O>(
        &'parser mut self,
        input: &'parser mut Input,
        map_fn: F,
    ) -> miette::Result<(O, &'parser mut Input), Error>
    where
        F: Fn(Self::Output) -> O,
    {
        self.parse(input)
            .map(|(output, rest)| (map_fn(output), rest))
    }
}

pub trait ValueParser<'parser> {
    type Error: Err + Diagnostic;
    fn parse<Language, L: Lexer<Item = Token<'parser, Language>>>(
        &mut self,
        input: &'parser mut L,
    ) -> miette::Result<(Value, &'parser mut L), Self::Error> {
        todo!()
    }

    fn parse_real<Language, L: Lexer<Item = Token<'parser, Language>>>(
        &mut self,
        input: &mut L,
    ) -> miette::Result<(Value, &'parser mut L), Self::Error>;

    fn parse_char<Language, L: Lexer<Item = Token<'parser, Language>>>(
        &mut self,
        input: &mut L,
    ) -> miette::Result<(Value, &'parser mut L), Self::Error>;

    fn parse_id<Language, L: Lexer<Item = Token<'parser, Language>>>(
        &mut self,
        input: &mut L,
    ) -> miette::Result<(Value, &'parser mut L), Self::Error>;
}

impl<'parser, Language, L: Lexer<Item = Token<'parser, Language>> + 'parser, V>
    Parser<'parser, L, V::Error> for V
where
    V: ValueParser<'parser>,
{
    type Output = Value<'parser>;
    fn parse(
        &'parser mut self,
        input: &'parser mut L,
    ) -> miette::Result<(Self::Output, &'parser mut L), V::Error> {
        self.parse(input)
    }
}
