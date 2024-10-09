use std::ops::Range;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::frontend::lexers::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

/* Parser Result */
pub type ParserR<'parser, Output, Error> =
    miette::Result<(Output, &'parser mut Lexer<'parser>), Error>;

/* General Parser Trait */
pub trait Parser<'parser> {
    type Output;
    fn parse(
        &self,
        input: &'parser mut Lexer<'parser>,
    ) -> ParserR<'parser, Self::Output, ParserError>;

    fn map<F, O>(&self, map_fn: F) -> impl Parser<'parser, Output = O>
    where
        F: Fn(Self::Output) -> O,
    {
        move |input| match self.parse(input) {
            Ok((output, rest)) => Ok((map_fn(output), rest)),
            Err(err) => Err(err),
        }
    }

    fn pair<P>(&self, p: P) -> impl Parser<'parser, Output = (Self::Output, P::Output)>
    where
        P: Parser<'parser>,
    {
        move |input| match self.parse(input) {
            Ok((output1, rest)) => match p.parse(rest) {
                Ok((output2, rest)) => Ok(((output1, output2), rest)),
                Err(err) => Err(err),
            },
            Err(err) => Err(err),
        }
    }
}

/* Parser Errors */
type ParserError = Box<dyn Diagnostic + Send + Sync>;

#[derive(Error, Diagnostic, Debug)]
#[error("Parser 'Expected' Error")]
#[diagnostic(help("Expected {:#?}", self.expected))]
pub struct Expected {
    #[source_code]
    src: NamedSource<String>,
    #[label]
    bad_bit: SourceSpan,
    expected: TokenKind,
}

impl Expected {
    pub fn new(name: &str, src: &str, range: Range<usize>, expected: TokenKind) -> Self {
        Self {
            src: NamedSource::new(name, src.to_string()),
            bad_bit: range.into(),
            expected,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Parser 'EOF' Error")]
#[diagnostic(help("Expected {:#?} but got EOF (end of file)", self.expected))]
pub struct UnexpectedEOF {
    #[source_code]
    src: NamedSource<String>,
    #[label]
    bad_bit: SourceSpan,
    expected: TokenKind,
}

impl UnexpectedEOF {
    pub fn new(name: &str, src: &str, range: Range<usize>, expected: TokenKind) -> Self {
        Self {
            src: NamedSource::new(name, src.to_string()),
            bad_bit: range.into(),
            expected,
        }
    }
}

/* Parser Implementors */
impl<'parser, F, O> Parser<'parser> for F
where
    F: Fn(&'parser mut Lexer<'parser>) -> ParserR<'parser, O, ParserError>,
{
    type Output = O;
    fn parse(&self, input: &'parser mut Lexer<'parser>) -> ParserR<'parser, O, ParserError> {
        self(input)
    }
}

/* Primitive Parsers */
fn expect<'parser>(kind: TokenKind) -> impl Parser<'parser, Output = Token<'parser>> {
    move |input: &'parser mut Lexer<'parser>| match input.peek() {
        Some(token) => {
            if token.kind == kind.clone() {
                input.next();
                Ok((token, input))
            } else {
                Err(Box::new(Expected::new(
                    input.name,
                    input.src,
                    token.lexeme.range,
                    kind.clone(),
                )) as ParserError)
            }
        }
        None => Err(Box::new(UnexpectedEOF::new(
            input.name,
            input.src,
            input.offset..input.offset / 2,
            kind.clone(),
        )) as ParserError),
    }
}

/* Parser Blankets and Extension traits */
pub trait TokenParser<'parser>: Parser<'parser> {
    fn then_expect_ignore(&self, kind: TokenKind) -> impl Parser<'parser, Output = Self::Output> {
        move |input: &'parser mut Lexer<'parser>| {
            self.parse(input)
                .map_or_else(Err, |(output, rest)| match rest.peek() {
                    Some(token) => {
                        if token.kind == kind.clone() {
                            rest.next();
                            Ok((output, rest))
                        } else {
                            Err(Box::new(Expected::new(
                                rest.name,
                                rest.src,
                                rest.offset..rest.offset / 2,
                                kind.clone(),
                            )) as ParserError)
                        }
                    }
                    None => Err(Box::new(UnexpectedEOF::new(
                        rest.name,
                        rest.src,
                        rest.offset..rest.offset / 2,
                        kind.clone(),
                    )) as ParserError),
                })
        }
    }
}

impl<'parser, P: Parser<'parser>> TokenParser<'parser> for P {}

pub trait Paired<'parser, Left, Right>: Parser<'parser, Output = (Left, Right)> {
    fn left(&self) -> impl Parser<'parser, Output = Left> {
        self.map(|(left, _)| left)
    }

    fn right(&self) -> impl Parser<'parser, Output = Right> {
        self.map(|(_, right)| right)
    }
}

impl<'parser, Left, Right, P: Parser<'parser, Output = (Left, Right)>> Paired<'parser, Left, Right>
    for P
{
}

/*Parser Lib Tests*/
#[cfg(test)]
mod test {
    use std::ops::Range;

    use miette::{Diagnostic, NamedSource, SourceSpan};
    use thiserror::Error;

    use crate::frontend::lexers::{lexer::Lexer, token::TokenKind};

    use super::{expect, Paired, Parser, ParserError, ParserR, TokenParser};

    fn some_parser<'parser>(
        input: &'parser mut Lexer<'parser>,
    ) -> ParserR<'parser, String, ParserError> {
        #[derive(Error, Diagnostic, Debug)]
        #[error("whhooops!")]
        #[diagnostic(help("Skill issue"))]
        pub struct SomeParserError {
            #[source_code]
            src: NamedSource<String>,
            #[label("This bit")]
            bad_bit: SourceSpan,
        }

        impl SomeParserError {
            fn new(name: &str, src: &str, range: Range<usize>) -> Self {
                Self {
                    src: NamedSource::new(name, src.to_string()),
                    bad_bit: range.into(),
                }
            }
        }

        match input.next() {
            Some(token) => match token.kind {
                TokenKind::Unknown => Ok((String::from(token.lexeme.slice), input)),
                _ => Err(Box::new(SomeParserError::new(
                    input.name,
                    input.src,
                    token.lexeme.range,
                ))),
            },
            None => panic!("This should never happen"),
        }
    }

    #[test]
    fn some_parser_test() -> miette::Result<()> {
        let mut lexer = Lexer::new("main.eur", "Hello world");
        match some_parser.parse(&mut lexer) {
            Ok((string, _rest)) => assert_eq!("Hello".to_string(), string),
            Err(err) => return Err(miette::Report::new_boxed(err)),
        }
        Ok(())
    }

    #[test]
    fn map_some_parser() -> miette::Result<()> {
        let mut lexer = Lexer::new("main.eur", "Hello world");
        match some_parser.map(|output| output.len()).parse(&mut lexer) {
            Ok((len, _rest)) => assert_eq!(5, len),
            Err(err) => return Err(miette::Report::new_boxed(err)),
        }
        Ok(())
    }

    #[test]
    fn pair_parser() -> miette::Result<()> {
        let mut lexer = Lexer::new("main.eur", "Hello world");
        match some_parser
            .map(|output| output.len())
            .pair(some_parser)
            .parse(&mut lexer)
        {
            Ok(((len, string), _rest)) => assert_eq!((5, "world".to_string()), (len, string)),
            Err(err) => return Err(miette::Report::new_boxed(err)),
        }
        Ok(())
    }

    #[test]
    fn then_ignore_parser() -> miette::Result<()> {
        let mut lexer = Lexer::new("main.eur", "Hello friend ignored_token");
        match some_parser
            .map(|output| output.len())
            .pair(some_parser)
            .then_expect_ignore(TokenKind::Unknown)
            .parse(&mut lexer)
        {
            Ok(((len, string), _rest)) => assert_eq!((5, "friend".to_string()), (len, string)),
            Err(err) => return Err(miette::Report::new_boxed(err)),
        }
        Ok(())
    }

    #[test]
    fn paired_parser() -> miette::Result<()> {
        let mut lexer = Lexer::new("main.eur", "Hello friend ignored_token");
        match some_parser
            .map(|output| output.len())
            .pair(some_parser)
            .then_expect_ignore(TokenKind::Unknown)
            .left()
            .parse(&mut lexer)
        {
            Ok((len, _rest)) => assert_eq!(5, len),
            Err(err) => return Err(miette::Report::new_boxed(err)),
        }
        Ok(())
    }

    #[test]
    #[should_panic]
    fn expect_parser() {
        let mut lexer = Lexer::new("main.eur", "5 friend ignored_token");
        let res = match expect(TokenKind::Unknown).parse(&mut lexer) {
            Ok((token, _)) => Ok(println!("{:#?}", token)),
            Err(err) => Err(miette::Report::new_boxed(err)),
        };
        res.unwrap()
    }
}
