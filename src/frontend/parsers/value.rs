use miette::Diagnostic;
use thiserror::Error;

use crate::frontend::{
    lexers::{lexer::Lexer, token::TokenKind},
    syntaxtrees::{identifier::Identifier, value::Value},
};

use super::lib::{
    error::{Expected, ParserError, Perror, UnexpectedEOF},
    parser::{either, just, Combinator, Flatten, Pair, Parser},
    result::ParserR,
};

pub fn real(mut input: Lexer) -> ParserR<Value, Perror> {
    match input.peek() {
        Some(token) if token.is_num() => {
            input.next();
            match token.kind {
                TokenKind::Number(num) => ParserR::Ok((Value::Real(num), input)),
                _ => panic!("will never happebn"),
            }
        }
        Some(token) => ParserR::Err(Box::new(Expected::new(
            &input,
            token.lexeme.range,
            TokenKind::Number(0),
            token.kind,
        )) as Perror),
        None => ParserR::Err(Box::new(UnexpectedEOF::new(&input, TokenKind::Number(0))) as Perror),
    }
}

pub fn id(input: Lexer) -> ParserR<Value, Perror> {
    just(TokenKind::Unknown)
        .map(|token| Value::Id(Identifier(token.lexeme)))
        .parse(input)
}

#[derive(Debug, Error, Diagnostic)]
#[error("expected a char you idiot")]
#[diagnostic(help("get good"))]
pub struct CharError {}

impl ParserError for CharError {
    fn boxed(self) -> Box<dyn Diagnostic + Send + Sync> {
        Box::new(self)
    }

    fn report(self) -> miette::Report {
        miette::Report::new(self)
    }
}

pub fn char(input: Lexer) -> ParserR<Value, Perror> {
    just("'")
        .then(just(TokenKind::Unknown))
        .right()
        .then_ignore(just("'"))
        .map_or(|token| match token.lexeme.slice.parse::<char>() {
            Ok(c) => Ok(Value::Char(c)),
            Err(_) => Err(CharError {}),
        })
        .parse(input)
}

pub fn value(input: Lexer) -> ParserR<Value, Perror> {
    either(real, id).flatten().or(char).flatten().parse(input)
}

#[cfg(test)]
mod test {
    use super::{char, id, real};
    use crate::frontend::parsers::lib::error::ParserError;
    use crate::frontend::parsers::lib::parser::Parser;
    use crate::frontend::{lexers::lexer::Lexer, parsers::lib::result::ParserR};

    #[test]
    fn parse_id() -> miette::Result<()> {
        let lexer = Lexer::new("main", "hello");
        match id.parse(lexer) {
            ParserR::Ok((id, _rest)) => assert_eq!(id.to_string(), "<Value hello>"),
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    #[should_panic]
    fn parse_id_fail() {
        let lexer = Lexer::new("main", "32");
        let wrapped = match id.parse(lexer) {
            ParserR::Ok((id, _rest)) => Ok(assert_eq!(id.to_string(), "<Value hello>")),
            ParserR::Err(err) => Err(err.report()),
        };
        wrapped.unwrap()
    }

    #[test]
    fn parse_real() -> miette::Result<()> {
        let lexer = Lexer::new("main", "451");
        match real.parse(lexer) {
            ParserR::Ok((id, _rest)) => assert_eq!(id.to_string(), "<Value Real 451>"),
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn parse_char() -> miette::Result<()> {
        let lexer = Lexer::new("main", "'h'");
        match char.parse(lexer) {
            ParserR::Ok((id, _rest)) => assert_eq!(id.to_string(), "<Value Char h>"),
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }
}
