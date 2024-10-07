use std::{error::Error, fmt::Debug, ops::Range};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::frontend::lexers::{lexer::Lexer, token::TokenKind};

pub type ParserR<'parser, Output, Error> =
    miette::Result<(Output, &'parser mut Lexer<'parser>), Error>;

#[derive(Error, Diagnostic, Debug)]
pub enum Either<E1: Error + Diagnostic, E2: Error + Diagnostic> {
    #[error(transparent)]
    Left(E1),
    #[error(transparent)]
    Right(E2),
}

impl<E1: Error + Diagnostic, E2: Error + Diagnostic> Either<E1, E2> {}

#[derive(Error, Diagnostic, Debug)]
#[error("Parser Error")]
pub struct Expected<O: std::fmt::Debug> {
    expected: O,
    #[source_code]
    src: NamedSource<String>,
    #[label("Here")]
    bad_bit: SourceSpan,
}

impl<O: std::fmt::Debug> Expected<O> {
    pub fn new(expected: O, name: &str, src: &str, range: Range<usize>) -> Self {
        Self {
            expected,
            src: NamedSource::new(name, src.to_string()),
            bad_bit: range.into(),
        }
    }
}

pub trait Parser<'parser> {
    type Output;
    type Perr: Error + Diagnostic;

    fn parse(
        &self,
        input: &'parser mut Lexer<'parser>,
    ) -> ParserR<'parser, Self::Output, Self::Perr>;

    fn pair<P2>(
        &'parser self,
        p2: P2,
    ) -> impl Parser<'parser, Output = (Self::Output, P2::Output), Perr = Either<Self::Perr, P2::Perr>>
    where
        P2: Parser<'parser>,
    {
        move |input| match self.parse(input) {
            Ok((output1, rest1)) => match p2.parse(rest1) {
                Ok((output2, rest2)) => Ok(((output1, output2), rest2)),
                Err(err) => Err(Either::Right(err)),
            },
            Err(err) => Err(Either::Left(err)),
        }
    }

    fn expect(
        &'parser self,
        kind: Self::Output,
    ) -> impl Parser<'parser, Output = Self::Output, Perr = Either<Self::Perr, Expected<Self::Output>>>
    where
        Self::Output: PartialEq + std::fmt::Debug,
    {
        move |input: &'parser mut Lexer<'parser>| {
            let cloned = input.clone();
            match self.parse(input) {
                Ok((output, rest)) => {
                    if output == kind {
                        Ok((output, rest))
                    } else {
                        Err(Either::Right(Expected::new(
                            output,
                            rest.name,
                            rest.src,
                            cloned.offset..rest.offset,
                        )))
                    }
                }
                Err(err) => Err(Either::Left(err)),
            }
        }
    }
}

impl<'parser, F, O, E: Error + Diagnostic> Parser<'parser> for F
where
    F: Fn(&'parser mut Lexer<'parser>) -> ParserR<'parser, O, E>,
{
    type Output = O;
    type Perr = E;
    fn parse(
        &self,
        input: &'parser mut Lexer<'parser>,
    ) -> ParserR<'parser, Self::Output, Self::Perr> {
        self(input)
    }
}
