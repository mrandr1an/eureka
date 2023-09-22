
#[derive(Debug, PartialEq,Clone)]
pub enum TokenType
{
    Pos(Result<Value, LexerError>),
    Sep(Result<Operator,Separator>),
    Done,
}

#[derive(Debug, PartialEq,Clone)]
pub enum Operator
{
    /* Arithmetic */
    Plus,    // +
    Minus,   // -
    Mul,     // *
    Div,     // /
    Mod,     // mod
    Power,   // ^
    Abs,     // |
    Fact,    // !
    Equal,   // =
    /* CompScience, abstractions */
    Dot,     // .
    Tik,     // ` 
    Almost,  // ~
    Ref,     // &
    Pointer, // @
    Deref,   // deref
    SingleQ, // '
    DoubleQ, // "
    LParen, //(
    RParen, // ) 
    LSParen, // [
    RSParen, // ]
    LCParen, // {
    RCParen, // }
    MatchAgainst,   // =>
    PointTo, // ->
    Comment, // ;
    Range, // ..
    PlaceHolder, // _
    Help, // ?
    Comma, // ,
    /* Boolean */
    And, // and
    Or, // or
    Not, // not
    Is, // is
    /* Comparison */
    Gr, // >
    Le, // <
    GrEq, // >=
    LeEq, // <=
    /* Compiler Specific */
    Meta, // Meta
    Define, // define
    When,  // when
    Otherwise, // otherwise
    Halt, // halt
    Data, // data
    NotOp, // this is not part of the language and returns error
}            

#[derive(Debug, PartialEq,Clone)]
pub enum Separator
{
    Space,
    Indent(usize),
    Endline,
    NotSep,
}

#[derive(Debug, PartialEq,Clone)]
pub enum Value
{
    Character(Vec<char>),
    Array(Vec<Value>),
    Num(Number),
    Id(String),
}

#[derive(Debug, PartialEq,Clone)]
pub enum Number
{
    Float(Vec<char>, Vec<char>),
    Real(Vec<char>), 
    PercisionFloat,
    Imaginary,
}

#[derive(Debug, PartialEq,Clone)]
pub enum LexerError
{
    Er(Box<TokenType>,String, String),
    Done,
}
