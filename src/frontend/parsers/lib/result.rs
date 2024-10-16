use crate::frontend::lexers::lexer::Lexer;

pub enum ParserR<'parser, Output, Error> {
    Ok((Output, Lexer<'parser>)),
    Err(Error),
}

impl<'parser, Output, Error> ParserR<'parser, Output, Error> {}

pub enum Either<Left, Right> {
    Left(Left),
    Right(Right),
}

impl<'parser, Left, Right, Error> ParserR<'parser, Either<Left, Right>, Error> {}

impl<'parser, T, Error> ParserR<'parser, Either<T, T>, Error> {}
