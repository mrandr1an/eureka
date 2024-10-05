use miette::NamedSource;

use crate::frontend::{
    lexers::token::{Operator, Reserved, Token, TokenVariant},
    syntaxtrees::{identifier::Identifier, value::Value},
};

use super::parser::{Input, ParserResult, UnexpectedEOF, ValueError};

pub fn real<'parser, Language, Lexer: Iterator<Item = Token<'parser, Language>>>(
    input: &'parser mut Input<'parser, Language, Lexer>,
) -> ParserResult<'parser, Value, &'parser mut Input<'parser, Language, Lexer>, ValueError> {
    match input.next() {
        Some(token) => match token.kind.variant {
            TokenVariant::Number(num) => Ok((Value::Real(num), input)),
            _other => Err(ValueError::ExpectedReal(
                NamedSource::new(input.name, input.src.to_string()),
                token.lexeme.range.into(),
            )),
        },
        None => Err(ValueError::Eof(UnexpectedEOF::new(
            input.name,
            input.src,
            input.len..input.len,
        ))),
    }
}

pub fn id<'parser, Language, Lexer: Iterator<Item = Token<'parser, Language>>>(
    input: &'parser mut Input<'parser, Language, Lexer>,
) -> ParserResult<'parser, Value, &'parser mut Input<'parser, Language, Lexer>, ValueError> {
    match input.next() {
        Some(token) => match token.kind.variant {
            TokenVariant::Unknown => Ok((Value::Id(Identifier(token.lexeme)), input)),
            _other => Err(ValueError::ExpectedId(
                NamedSource::new(input.name, input.src.to_string()),
                token.lexeme.range.into(),
            )),
        },
        None => Err(ValueError::Eof(UnexpectedEOF::new(
            input.name,
            input.src,
            input.len..input.len,
        ))),
    }
}

pub fn single_q<'parser, Language, Lexer: Iterator<Item = Token<'parser, Language>>>(
    input: &'parser mut Input<'parser, Language, Lexer>,
) -> ParserResult<'parser, Value, &'parser mut Input<'parser, Language, Lexer>, ValueError> {
    match input.next() {
        Some(token) => match token.kind.variant {
            TokenVariant::Reserved(Reserved::Op(Operator::SingleQ)) => char(input),
            _other => Err(ValueError::ExpectedSingleQ(
                NamedSource::new(input.name, input.src.to_string()),
                token.lexeme.range.into(),
            )),
        },
        None => Err(ValueError::Eof(UnexpectedEOF::new(
            input.name,
            input.src,
            input.len..input.len,
        ))),
    }
}

pub fn char<'parser, Language, Lexer: Iterator<Item = Token<'parser, Language>>>(
    input: &'parser mut Input<'parser, Language, Lexer>,
) -> ParserResult<'parser, Value, &'parser mut Input<'parser, Language, Lexer>, ValueError> {
    match input.next() {
        Some(token) => match token.kind.variant {
            TokenVariant::Unknown | TokenVariant::Number(_) => match input.next() {
                Some(end_char) => match end_char.kind.variant {
                    TokenVariant::Reserved(Reserved::Op(Operator::SingleQ)) => {
                        match token.lexeme.slice.parse::<char>() {
                            Ok(c) => Ok((Value::Char(c), input)),
                            Err(_) => Err(ValueError::ExpectedChar(
                                NamedSource::new(input.name, input.src.to_string()),
                                token.lexeme.range.into(),
                            )),
                        }
                    }
                    _ => Err(ValueError::ExpectedSingleQ(
                        NamedSource::new(input.name, input.src.to_string()),
                        end_char.lexeme.range.into(),
                    )),
                },
                None => Err(ValueError::Eof(UnexpectedEOF::new(
                    input.name,
                    input.src,
                    input.len..input.len,
                ))),
            },
            _other => Err(ValueError::ExpectedChar(
                NamedSource::new(input.name, input.src.to_string()),
                token.lexeme.range.into(),
            )),
        },
        None => Err(ValueError::Eof(UnexpectedEOF::new(
            input.name,
            input.src,
            input.len..input.len,
        ))),
    }
}

#[cfg(test)]
mod test {
    use crate::frontend::{
        lexers::lexer::Tokens,
        parsers::{
            parser::{pair, Parser},
            value::single_q,
        },
        syntaxtrees::value::Value,
    };

    use super::{char, id, real};

    #[test]
    fn parse_value() {
        let mut lexer = Tokens::new("534", "main.eur");
        assert_eq!(real.parse(&mut lexer).unwrap().0, Value::Real(534))
    }

    #[test]
    fn parse_char() -> miette::Result<()> {
        let mut lexer = Tokens::new("'5'", "main.eur");
        assert_eq!(single_q.parse(&mut lexer).unwrap().0, Value::Char('5'));
        Ok(())
    }

    #[test]
    fn parse_both() -> miette::Result<()> {
        let mut lexer = Tokens::new("someid 'a'", "main.eur");
        match pair(id, single_q).parse(&mut lexer) {
            Ok(i) => println!("{} {}", i.0 .0, i.0 .1),
            Err(err) => return Err(err.into()),
        }
        Ok(())
    }
}
