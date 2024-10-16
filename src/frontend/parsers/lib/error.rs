use std::{
    fmt::{Debug, Display},
    ops::Range,
};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::frontend::lexers::{lexer::Lexer, token::TokenKind};

pub type Perror = Box<dyn Diagnostic + Send + Sync>;

pub trait ParserError: Send + Sync {
    fn report(self) -> miette::Report;
    fn boxed(self) -> Box<dyn Diagnostic + Send + Sync>;
}

impl ParserError for Box<dyn Diagnostic + Send + Sync> {
    fn report(self) -> miette::Report {
        miette::Report::new_boxed(self)
    }

    fn boxed(self) -> Box<dyn Diagnostic + Send + Sync> {
        self
    }
}

impl<E, G> ParserError for Expected<E, G>
where
    E: Into<TokenKind> + Debug + Display + Send + Sync + 'static,
    G: Into<TokenKind> + Debug + Display + Send + Sync + 'static,
{
    fn report(self) -> miette::Report {
        miette::Report::new(self)
    }

    fn boxed(self) -> Box<dyn Diagnostic + Send + Sync> {
        Box::new(self)
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("ParserError")]
#[diagnostic(
    code(eureka::parser),
    help("I expected {:#?} but got {:#?} instead", expected, got)
)]
pub struct Expected<E, G>
where
    E: Into<TokenKind> + Debug + Display,
    G: Into<TokenKind> + Debug + Display,
{
    #[source_code]
    src: NamedSource<String>,
    #[label("here")]
    bad_bit: SourceSpan,
    expected: E,
    got: G,
}

impl<E, G> Expected<E, G>
where
    E: Into<TokenKind> + Debug + Display,
    G: Into<TokenKind> + Debug + Display,
{
    pub fn new<'a, 'b: 'a>(lexer: &'a Lexer<'b>, range: Range<usize>, expected: E, got: G) -> Self {
        Self {
            src: NamedSource::new(lexer.name, lexer.src.to_string()),
            bad_bit: range.into(),
            expected,
            got,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("ParserError")]
#[diagnostic(
    code(eureka::parser),
    help("I expected {} but got EOF (end of file) instead", expected)
)]
pub struct UnexpectedEOF<E>
where
    E: Into<TokenKind> + Debug + Display,
{
    #[source_code]
    src: NamedSource<String>,
    #[label("here")]
    bad_bit: SourceSpan,
    expected: E,
}

impl<E> UnexpectedEOF<E>
where
    E: Into<TokenKind> + Debug + Display,
{
    pub fn new<'a, 'b: 'a>(lexer: &'a Lexer<'b>, expected: E) -> Self {
        Self {
            src: NamedSource::new(lexer.name, lexer.src.to_string()),
            bad_bit: (lexer.offset / 2..lexer.offset).into(),
            expected,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Multiple possible parser errors")]
#[diagnostic(help("One those errors corresponds to what you need to fix to parse correctly"))]
pub struct Multiple {
    #[related]
    others: Vec<Box<dyn Diagnostic + Send + Sync>>,
}

impl Multiple {
    pub fn new() -> Self {
        Self { others: Vec::new() }
    }

    pub fn add(&mut self, error: Perror) {
        self.others.push(error);
    }
}
