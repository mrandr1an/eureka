use std::{error::Error, fmt::Debug, ops::Range};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::frontend::lexers::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

/* Parser Result */
pub type ParserR<'parser, Output, Error> =
    miette::Result<(Output, &'parser mut Lexer<'parser>), Error>;

/* Parser Error Types */
#[derive(Error, Diagnostic, Debug)]
pub enum Either<E1: Error + Diagnostic, E2: Error + Diagnostic> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Left(E1),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Right(E2),
}

impl<E1: Error + Diagnostic, E2: Error + Diagnostic> Either<E1, E2> {
    pub fn lmap<F, O>(self, map_fn: F) -> Result<O, E2>
    where
        F: Fn(E1) -> O,
    {
        match self {
            Self::Left(left) => Ok(map_fn(left)),
            Self::Right(right) => Err(right),
        }
    }

    pub fn rmap<F, O>(self, map_fn: F) -> Result<O, E1>
    where
        F: Fn(E2) -> O,
    {
        match self {
            Self::Right(right) => Ok(map_fn(right)),
            Self::Left(left) => Err(left),
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("EOF Error")]
pub struct UnexpectedEOF {
    #[source_code]
    src: NamedSource<String>,
    #[label("Here")]
    bad_bit: SourceSpan,
}

impl UnexpectedEOF {
    pub fn new(name: &str, src: &str, range: Range<usize>) -> Self {
        Self {
            src: NamedSource::new(name, src.to_string()),
            bad_bit: range.into(),
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Parsing Error")]
#[diagnostic(code(parser::expected), severity(Error))]
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

    fn map<F, O, P>(&self, map_fn: F) -> impl Parser<'parser, Output = O, Perr = Self::Perr>
    where
        F: Fn(Self::Output) -> O,
    {
        move |input| match self.parse(input) {
            Ok((output, rest)) => {
                Ok::<(O, &mut Lexer<'parser>), Self::Perr>((map_fn(output), rest))
            }
            Err(err) => Err(err),
        }
    }
}

/* Generic parser implementations */

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

/* Primitive Parsers */

pub fn expect<'parser>(
    kind: TokenKind,
) -> impl Parser<'parser, Output = TokenKind, Perr = Either<UnexpectedEOF, Expected<TokenKind>>> {
    move |input: &'parser mut Lexer<'parser>| match input.next() {
        Some(token) => {
            if token.kind == kind {
                Ok((token.kind, input))
            } else {
                Err(Either::Right(Expected::new(
                    token.kind,
                    input.name,
                    input.src,
                    token.lexeme.range,
                )))
            }
        }
        None => Err(Either::Left(UnexpectedEOF::new(
            input.name,
            input.src,
            input.offset..input.offset / 2,
        ))),
    }
}

/* Parser Blankets*/

pub trait TokenParser<'parser>: Parser<'parser, Output = TokenKind> {
    fn then_expect(
        &self,
        kind: TokenKind,
    ) -> impl Parser<
        'parser,
        Output = Token<'parser>,
        Perr = Either<Self::Perr, Either<UnexpectedEOF, Expected<TokenKind>>>,
    > {
        move |input: &'parser mut Lexer<'parser>| {
            let input = match self.parse(input) {
                Ok((_, rest)) => rest,
                Err(err) => return Err(Either::Left(err)),
            };

            match input.next() {
                Some(token) => {
                    if token.kind == kind {
                        Ok((token, input))
                    } else {
                        Err(Either::Right(Either::Right(Expected::new(
                            token.kind,
                            input.name,
                            input.src,
                            token.lexeme.range,
                        ))))
                    }
                }
                None => Err(Either::Right(Either::Left(UnexpectedEOF::new(
                    input.name,
                    input.src,
                    input.offset..input.offset / 2,
                )))),
            }
        }
    }
}

impl<'parser, E: Error + Diagnostic, P> TokenParser<'parser> for P where
    P: Parser<'parser, Output = TokenKind, Perr = E>
{
}
