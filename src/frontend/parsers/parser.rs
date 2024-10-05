use std::{error::Error, ops::Range};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::frontend::lexers::{lexer::Tokens, token::Token};

pub type ParserResult<'parser, Output, Rest, Err> = miette::Result<(Output, Rest), Err>;

pub type Input<'parser, Language, Lexer> = Tokens<'parser, Language, Lexer>;

pub trait Parser<
    'parser,
    Language,
    Lexer: Iterator<Item = Token<'parser, Language>>,
    Output,
    Err: Error + Diagnostic,
>
{
    fn parse(
        &self,
        input: &'parser mut Input<'parser, Language, Lexer>,
    ) -> ParserResult<'parser, Output, &'parser mut Input<'parser, Language, Lexer>, Err>;

    fn map<F, MappedOutput>(
        &self,
        input: &'parser mut Input<'parser, Language, Lexer>,
        map_fn: F,
    ) -> ParserResult<'parser, MappedOutput, &'parser mut Input<'parser, Language, Lexer>, Err>
    where
        F: Fn(Output) -> MappedOutput,
    {
        self.parse(input)
            .map(|(output, rest)| (map_fn(output), rest))
    }
}

impl<
        'parser,
        F,
        Language: 'parser,
        Lexer: Iterator<Item = Token<'parser, Language>> + 'parser,
        Output,
        Err: Error + Diagnostic,
    > Parser<'parser, Language, Lexer, Output, Err> for F
where
    F: Fn(
        &'parser mut Input<'parser, Language, Lexer>,
    ) -> ParserResult<'parser, Output, &'parser mut Input<'parser, Language, Lexer>, Err>,
{
    fn parse(
        &self,
        input: &'parser mut Input<'parser, Language, Lexer>,
    ) -> ParserResult<'parser, Output, &'parser mut Input<'parser, Language, Lexer>, Err> {
        self(input)
    }
}

pub fn pair<
    'parser,
    P1,
    P2,
    L: 'parser,
    Lexer: Iterator<Item = Token<'parser, L>> + 'parser,
    O1,
    O2,
    E: Error + Diagnostic,
>(
    p1: P1,
    p2: P2,
) -> impl Parser<'parser, L, Lexer, (O1, O2), E>
where
    P1: Parser<'parser, L, Lexer, O1, E>,
    P2: Parser<'parser, L, Lexer, O2, E>,
{
    move |input| match p1.parse(input) {
        Ok((output1, rest1)) => match p2.parse(rest1) {
            Ok((output2, rest2)) => Ok(((output1, output2), rest2)),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Error parsing file")]
pub struct UnexpectedEOF {
    #[source_code]
    src: NamedSource<String>,
    #[label("Here")]
    bad_bit: SourceSpan,
}

impl UnexpectedEOF {
    pub fn new(name: &str, src: &str, range: Range<usize>) -> Self {
        UnexpectedEOF {
            src: NamedSource::new(name, src.to_string()),
            bad_bit: range.into(),
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Error parsing value")]
pub enum ValueError {
    #[diagnostic(
        code(Parser::Value::UnexpectedToken),
        help("Enter a character instead")
    )]
    ExpectedChar(
        #[source_code] NamedSource<String>,
        #[label("Here")] SourceSpan,
    ),
    #[diagnostic(
        code(Parser::Value::UnexpectedToken),
        help("Enter a real number instead")
    )]
    ExpectedReal(
        #[source_code] NamedSource<String>,
        #[label("Here")] SourceSpan,
    ),
    #[diagnostic(code(Parser::Value::UnexpectedToken), help("Enter an id instead"))]
    ExpectedId(
        #[source_code] NamedSource<String>,
        #[label("Here")] SourceSpan,
    ),
    #[diagnostic(code(Parser::Value::UnexpectedToken), help("Enter an ' instead"))]
    ExpectedSingleQ(
        #[source_code] NamedSource<String>,
        #[label("Here")] SourceSpan,
    ),
    #[diagnostic(transparent, code(Parser::Value::UnexpectedEOF))]
    Eof(#[from] UnexpectedEOF),
}
