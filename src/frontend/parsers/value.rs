use crate::frontend::{
    lexers::{
        lexer::Lexer,
        token::{Operator, Reserved, TokenKind},
    },
    syntaxtrees::{identifier::Identifier, value::Value},
};

use super::parser::{
    expect, Expected, Paired, Parser, ParserError, ParserR, TokenParser, UnexpectedEOF,
};

fn real(mut input: Lexer) -> ParserR<Value, ParserError> {
    match input.next() {
        Some(token) if token.is_num() => match token.kind {
            TokenKind::Number(num) => Ok((Value::Real(num), input)),
            _ => panic!("This will never happebn"),
        },
        Some(token) => Err(Box::new(Expected::new(
            input.name,
            input.src,
            token.lexeme.range,
            token.kind,
        )) as ParserError),
        None => Err(Box::new(UnexpectedEOF::new(
            input.name,
            input.src,
            input.offset..input.offset / 5,
            TokenKind::Unknown,
        )) as ParserError),
    }
}

fn id(mut input: Lexer) -> ParserR<Value, ParserError> {
    match input.next() {
        Some(token) if token.is_unknown() => Ok((Value::Id(Identifier(token.lexeme)), input)),
        Some(token) => Err(Box::new(Expected::new(
            input.name,
            input.src,
            token.lexeme.range,
            token.kind,
        )) as ParserError),
        None => Err(Box::new(UnexpectedEOF::new(
            input.name,
            input.src,
            input.offset..input.offset / 5,
            TokenKind::Unknown,
        )) as ParserError),
    }
}

fn char(input: Lexer) -> ParserR<Value, ParserError> {
    expect("'")
        .pair(expect(TokenKind::Unknown))
        .then_expect_ignore(TokenKind::Reserved(Reserved::Op(Operator::SingleQ)))
        .right()
        .map(|token| match token.lexeme.slice.parse::<char>() {
            Ok(c) => Value::Char(c),
            Err(e) => todo!(),
        })
        .parse(input)
}

#[cfg(test)]
mod test {

    use super::char;
    use super::id;
    use super::Parser;
    use crate::frontend::lexers::lexer::Lexer;
    use crate::frontend::syntaxtrees::value::Value;

    #[test]
    fn parse_id() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "hello");
        match id.parse(lexer) {
            Ok((Value::Id(id), _)) => assert_eq!(id.0.slice, "hello"),
            Err(err) => return Err(miette::Report::new_boxed(err)),
            _ => todo!(),
        };
        Ok(())
    }

    #[test]
    fn parse_char() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "'c'");
        match char.parse(lexer) {
            Ok((Value::Char(ch), _)) => assert_eq!(ch, 'c'),
            Err(err) => return Err(miette::Report::new_boxed(err)),
            _ => todo!(),
        };
        Ok(())
    }
}
