pub trait Lexer: Iterator {
    fn origin(&self) -> String;
    fn len(&self) -> usize;
    fn input(&self) -> String;
}
