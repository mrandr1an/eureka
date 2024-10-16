use miette::Diagnostic;

use crate::frontend::lexers::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

use super::{
    error::{Expected, Multiple, ParserError, Perror, UnexpectedEOF},
    result::{Either, ParserR},
};

pub trait Parser<'parser>: Sized {
    type Output;
    type Error: ParserError;
    fn parse(self, input: Lexer<'parser>) -> ParserR<'parser, Self::Output, Self::Error>;

    fn map<F, O>(self, map_fn: F) -> impl Parser<'parser, Output = O, Error = Self::Error>
    where
        F: Fn(Self::Output) -> O,
    {
        move |input: Lexer<'parser>| match self.parse(input) {
            ParserR::Ok((output, err)) => ParserR::Ok((map_fn(output), err)),
            ParserR::Err(err) => ParserR::Err(err),
        }
    }

    fn map_err<F, O: ParserError>(self, map_fn: F) -> impl Parser<'parser, Error = O>
    where
        F: Fn(Self::Error) -> O,
    {
        move |input: Lexer<'parser>| match self.parse(input) {
            ParserR::Ok((output, err)) => ParserR::Ok((output, err)),
            ParserR::Err(err) => ParserR::Err(map_fn(err)),
        }
    }

    fn map_or<F, O, E: ParserError>(
        self,
        map_fn: F,
    ) -> impl Parser<'parser, Output = O, Error = Perror>
    where
        F: Fn(Self::Output) -> Result<O, E>,
    {
        move |input: Lexer<'parser>| match self.parse(input) {
            ParserR::Ok((output, rest)) => match map_fn(output) {
                Ok(result) => ParserR::Ok((result, rest)),
                Err(err) => ParserR::Err(err.boxed()),
            },
            ParserR::Err(err) => ParserR::Err(err.boxed()),
        }
    }
}

/* Implementors */
impl<'parser, F, O, E: ParserError> Parser<'parser> for F
where
    F: FnOnce(Lexer<'parser>) -> ParserR<'parser, O, E>,
{
    type Output = O;
    type Error = E;
    fn parse(self, input: Lexer<'parser>) -> ParserR<'parser, Self::Output, Self::Error> {
        self(input)
    }
}

/* Primitive Parsers and Combinators*/
pub fn just<'parser, T: Into<TokenKind>>(
    expected: T,
) -> impl Parser<'parser, Output = Token<'parser>, Error = Box<dyn Diagnostic + Send + Sync>> {
    move |mut input: Lexer<'parser>| match input.peek() {
        Some(token) => {
            let kind = expected.into();
            if token.kind == kind {
                input.next();
                return ParserR::Ok((token, input));
            } else {
                return ParserR::Err(Box::new(Expected::new(
                    &input,
                    token.lexeme.range,
                    kind,
                    token.kind,
                )) as Box<dyn Diagnostic + Send + Sync>);
            }
        }
        None => ParserR::Err(Box::new(UnexpectedEOF::new(&input, expected.into()))
            as Box<dyn Diagnostic + Send + Sync>),
    }
}

pub fn expect<'parser, P, E>(parser: P) -> impl Combinator<'parser>
where
    P: Parser<'parser, Error = E>,
    E: Diagnostic + Send + Sync + 'static,
{
    parser.map_err(|err| Box::new(err) as Perror)
}

pub fn pair<'parser, P1, P2>(p1: P1, p2: P2) -> impl Pair<'parser, P1::Output, P2::Output>
where
    P1: Combinator<'parser>,
    P2: Combinator<'parser>,
{
    p1.then(p2)
}

pub fn either<'parser, P1, P2>(
    p1: P1,
    p2: P2,
) -> impl Combinator<'parser, Output = Either<P1::Output, P2::Output>>
where
    P1: Combinator<'parser>,
    P2: Combinator<'parser>,
{
    p1.or(p2)
}

pub fn until<'parser, T: Into<TokenKind>, P, O>(
    expected: T,
    parser: P,
) -> impl Combinator<'parser, Output = Vec<O>>
where
    P: Fn(Lexer<'parser>) -> ParserR<'parser, O, Perror>,
{
    move |mut input: Lexer<'parser>| {
        let expected_kind = expected.into();
        let mut tokens = Vec::new();
        while let Some(token) = input.peek() {
            if token.kind == expected_kind {
                input.next();
                return ParserR::Ok((tokens, input));
            } else {
                match parser(input) {
                    ParserR::Ok((output, rest)) => {
                        tokens.push(output);
                        input = rest;
                    }
                    ParserR::Err(err) => return ParserR::Err(err),
                }
            }
        }
        ParserR::Err(Box::new(UnexpectedEOF::new(&input, expected_kind)) as Perror)
    }
}

/* Extension Traits */

pub trait Combinator<'parser>: Parser<'parser, Error = Perror> {
    fn then<P>(self, p: P) -> impl Pair<'parser, Self::Output, P::Output>
    where
        P: Combinator<'parser>,
    {
        move |input| match self.parse(input) {
            ParserR::Ok((output1, rest1)) => match p.parse(rest1) {
                ParserR::Ok((output2, rest2)) => ParserR::Ok(((output1, output2), rest2)),
                ParserR::Err(err) => ParserR::Err(err as Perror),
            },
            ParserR::Err(err) => ParserR::Err(err as Perror),
        }
    }

    fn then_ignore<P>(self, p: P) -> impl Combinator<'parser, Output = Self::Output>
    where
        P: Combinator<'parser>,
    {
        move |input| match self.parse(input) {
            ParserR::Ok((output, rest)) => match p.parse(rest) {
                ParserR::Ok((_, rest)) => ParserR::Ok((output, rest)),
                ParserR::Err(err) => ParserR::Err(err),
            },
            ParserR::Err(err) => ParserR::Err(err),
        }
    }

    fn or<P>(self, p: P) -> impl Combinator<'parser, Output = Either<Self::Output, P::Output>>
    where
        P: Combinator<'parser>,
    {
        move |input: Lexer<'parser>| {
            let cloned = input.clone();
            match self.parse(input) {
                ParserR::Ok((output, rest)) => ParserR::Ok((Either::Left(output), rest)),
                ParserR::Err(err1) => match p.parse(cloned) {
                    ParserR::Ok((output, rest)) => ParserR::Ok((Either::Right(output), rest)),
                    ParserR::Err(err2) => {
                        let mut errors = Multiple::new();
                        errors.add(err1);
                        errors.add(err2);
                        ParserR::Err(Box::new(errors) as Perror)
                    }
                },
            }
        }
    }

    fn then_all_until<T: Into<TokenKind>>(
        self,
        expected: T,
    ) -> impl Pair<'parser, Self::Output, Vec<Token<'parser>>> {
        self.then(move |mut input: Lexer<'parser>| {
            let expected_kind = expected.into();
            let mut tokens = Vec::<Token>::new();
            while let Some(token) = input.peek() {
                if token.kind == expected_kind {
                    input.next();
                    return ParserR::Ok((tokens, input));
                } else {
                    input.next();
                    tokens.push(token);
                }
            }
            ParserR::Err(Box::new(UnexpectedEOF::new(&input, expected_kind)) as Perror)
        })
    }

    fn then_all_until_eof(self) -> impl Pair<'parser, Self::Output, Vec<Token<'parser>>> {
        self.then(move |mut input: Lexer<'parser>| {
            let mut tokens = Vec::<Token>::new();
            while let Some(token) = input.peek() {
                input.next();
                tokens.push(token);
            }
            ParserR::Ok((tokens, input))
        })
    }
}

impl<'parser, O, P: Parser<'parser, Output = O, Error = Perror>> Combinator<'parser> for P {}

pub trait Pair<'parser, O1, O2>: Combinator<'parser, Output = (O1, O2)> {
    fn left(self) -> impl Combinator<'parser, Output = O1> {
        move |input| match self.parse(input) {
            ParserR::Ok(((left, _right), rest)) => ParserR::Ok((left, rest)),
            ParserR::Err(err) => ParserR::Err(err),
        }
    }

    fn right(self) -> impl Combinator<'parser, Output = O2> {
        move |input| match self.parse(input) {
            ParserR::Ok(((_left, right), rest)) => ParserR::Ok((right, rest)),
            ParserR::Err(err) => ParserR::Err(err),
        }
    }
}

impl<'parser, O1, O2, P: Combinator<'parser, Output = (O1, O2)>> Pair<'parser, O1, O2> for P {}

pub trait Flatten<'parser, T>: Combinator<'parser, Output = Either<T, T>> {
    fn flatten(self) -> impl Combinator<'parser, Output = T> {
        move |input: Lexer<'parser>| match self.parse(input) {
            ParserR::Ok((output, rest)) => match output {
                Either::Left(left) => ParserR::Ok((left, rest)),
                Either::Right(right) => ParserR::Ok((right, rest)),
            },
            ParserR::Err(err) => ParserR::Err(err),
        }
    }
}

impl<'parser, T, P: Combinator<'parser, Output = Either<T, T>>> Flatten<'parser, T> for P {}

#[cfg(test)]
mod test {

    use crate::frontend::{
        lexers::{lexer::Lexer, token::TokenKind},
        parsers::{
            lib::{error::ParserError, result::ParserR},
            value::value,
        },
        syntaxtrees::value::Value,
    };

    use super::{either, just, pair, until, Combinator, Flatten, Pair, Parser};

    #[test]
    fn just_test() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45");
        match just("45").parse(lexer) {
            ParserR::Ok((token, _rest)) => assert_eq!(token.kind, TokenKind::Number(45)),
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn then_test() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match just("45").then(just("5")).parse(lexer) {
            ParserR::Ok(((token1, token2), _rest)) => {
                assert_eq!(token1.kind, TokenKind::Number(45));
                assert_eq!(token2.kind, TokenKind::Number(5));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn paired_test() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match just("45").then(just("5")).left().parse(lexer) {
            ParserR::Ok((token1, _rest)) => {
                assert_eq!(token1.kind, TokenKind::Number(45));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn or_test1() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match just("45").or(just("5")).flatten().parse(lexer) {
            ParserR::Ok((token, _rest)) => {
                assert_eq!(token.kind, TokenKind::Number(45));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn or_test2() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "5 45");
        match just("45").or(just("5")).flatten().parse(lexer) {
            ParserR::Ok((token, _rest)) => {
                assert_eq!(token.kind, TokenKind::Number(5));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn then_ignore() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match just("45").then_ignore(just("5")).parse(lexer) {
            ParserR::Ok((token, _rest)) => {
                assert_eq!(token.kind, TokenKind::Number(45));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn pair_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match pair(just("45"), just("5")).parse(lexer) {
            ParserR::Ok(((token1, token2), _rest)) => {
                assert_eq!(token1.kind, TokenKind::Number(45));
                assert_eq!(token2.kind, TokenKind::Number(5));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn either_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 5");
        match either(just("45"), just("5")).flatten().parse(lexer) {
            ParserR::Ok((token, _rest)) => {
                assert_eq!(token.kind, TokenKind::Number(45));
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn all_until_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 1 2 3 4 5");
        match just("45").then_all_until("5").parse(lexer) {
            ParserR::Ok(((token, rest), _)) => {
                assert_eq!(token.kind, TokenKind::Number(45));
                let rest_kind = rest
                    .into_iter()
                    .map(|token| token.kind)
                    .collect::<Vec<TokenKind>>();
                assert_eq!(
                    rest_kind,
                    vec![
                        TokenKind::Number(1),
                        TokenKind::Number(2),
                        TokenKind::Number(3),
                        TokenKind::Number(4),
                    ]
                );
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }

    #[test]
    fn until_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main.eur", "45 1 1 1 1 5");
        let a = just("1");
        match just("45").then(until("5", value)).parse(lexer) {
            ParserR::Ok(((token, rest), _)) => {
                assert_eq!(token.kind, TokenKind::Number(45));
                let rest_kind = rest.into_iter().collect::<Vec<Value>>();
                assert_eq!(
                    rest_kind,
                    vec![
                        Value::Real(1),
                        Value::Real(1),
                        Value::Real(1),
                        Value::Real(1),
                    ]
                );
            }
            ParserR::Err(err) => return Err(err.report()),
        };
        Ok(())
    }
}
