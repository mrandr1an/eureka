use std::{marker::PhantomData, ops::Range};

#[derive(Debug)]
pub struct Greek;
#[derive(Debug)]
pub struct English;

pub trait TokenDelim {
    fn is_delim(&self) -> bool;
}

impl TokenDelim for &str {
    fn is_delim(&self) -> bool {
        matches!(
            self,
            &"+" | &"-"
                | &"*"
                | &"/"
                | &"!"
                | &"("
                | &")"
                | &"{"
                | &"}"
                | &"["
                | &"]"
                | &"."
                | &","
                | &" "
                | &"\""
                | &"'"
                | &"="
        )
    }
}

///Contains the range of the original string where the substring "lexeme" resides
#[derive(Debug, Clone)]
pub struct Lexeme<'a> {
    //a..b where a is the first character and b is the last character of lexeme
    pub range: Range<usize>,
    pub slice: &'a str,
}

impl<'a> Lexeme<'a> {
    fn new(slice: &'a str, range: Range<usize>) -> Self {
        Self { range, slice }
    }
}

#[derive(Debug)]
pub struct Token<'a, Language> {
    pub lexeme: Lexeme<'a>,
    pub kind: TokenKind<Language>,
}

impl<'a> Token<'a, Greek> {
    pub fn new(range: Range<usize>, slice: &'a str) -> Self {
        Self {
            lexeme: Lexeme::new(slice, range),
            kind: TokenKind::from(slice),
        }
    }
}

impl From<&str> for TokenKind<Greek> {
    fn from(value: &str) -> Self {
        match value {
            /* Reserved Operators */
            "+" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Infix(
                Infix::Add,
            )))),
            "-" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Infix(
                Infix::Sub,
            )))),
            "*" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Infix(
                Infix::Mul,
            )))),
            "/" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Infix(
                Infix::Div,
            )))),
            "!" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Postfix(
                Postfix::Fact,
            )))),
            "," => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Postfix(
                Postfix::Comma,
            )))),
            "." => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Postfix(
                Postfix::Dot,
            )))),
            ".." => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Postfix(
                Postfix::DotDot,
            )))),
            "&" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::Prefix(
                Prefix::AdrOf,
            )))),
            "(" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::ParenL))),
            ")" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::ParenR))),
            "[" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::BracketL))),
            "]" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::BracketR))),
            "{" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::CBracketL))),
            "}" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::CBracketR))),
            "\"" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::DoubleQ))),
            "'" => TokenKind::new(TokenVariant::Reserved(Reserved::Op(Operator::SingleQ))),
            /* Reserved keywords */
            "ΣΥΝΑΡΤΗΣΗ" => {
                TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::Function)))
            }
            "ΜΕΘΟΔΟΣ" => {
                TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::Method)))
            }
            "ΔΙΑΔΙΚΑΣΙΑ" => TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(
                Keyword::Procedure,
            ))),
            "ΣΥΝΕΝΩΣΗ" => TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(
                Keyword::Aggregate,
            ))),
            "ΑΘΡΟΙΣΜΑ" => {
                TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::Sum)))
            }
            "ΕΣΤΩ" => TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::Let))),
            "ΑΠΟ" => TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::From))),
            "ϰ" => TokenKind::new(TokenVariant::Reserved(Reserved::Keyword(Keyword::Goto))),
            /* Reserved types */
            "ΠΡΑΓΜΑΤΙΚΟΣ" => {
                TokenKind::new(TokenVariant::Builtin(Builtin::Type(Type::Real)))
            }
            "ΧΑΡΑΚΤΗΡΑΣ" => {
                TokenKind::new(TokenVariant::Builtin(Builtin::Type(Type::Char)))
            }
            /* Reserved procedure/functions/methods */
            "ΕΚΤΥΠΩΣΕ" => {
                TokenKind::new(TokenVariant::Builtin(Builtin::Function(Function::Print)))
            }
            /*Non reserved at lexing time */
            token => match token.parse::<i64>() {
                Ok(i) => TokenKind::new(TokenVariant::Number(i)),
                Err(_err) => TokenKind::new(TokenVariant::Unknown),
            },
        }
    }
}

#[derive(Debug)]
pub struct TokenKind<Language> {
    _mark: PhantomData<Language>,
    pub variant: TokenVariant,
}

impl TokenKind<Greek> {
    fn new(variant: TokenVariant) -> Self {
        Self {
            _mark: PhantomData,
            variant,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenVariant {
    //Reserved Keywords
    Reserved(Reserved),
    //Types, values, functions generated "automagically" from the compiler
    Builtin(Builtin),
    //Identifier, value or type,
    Unknown,
    Number(i64),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Reserved {
    Op(Operator),
    Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operator {
    Infix(Infix),
    Postfix(Postfix),
    Prefix(Prefix),
    //Other
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    CBracketL,
    CBracketR,
    DoubleQ,
    SingleQ,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Postfix {
    Fact,
    Dot,
    DotDot,
    Comma,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Prefix {
    AdrOf,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Builtin {
    Type(Type),
    Function(Function),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Real,           //ΠΡΑΓΜΑΤΙΚΟΣ
    Char,           //ΧΑΡΑΚΤΗΡΑΣ
    Ptr(Box<Type>), //ex. ΠΡΑΓΜΑΤΙΚΟΣ*
}

#[derive(Debug, Eq, PartialEq)]
pub enum Function {
    Print, //ΕΚΤΥΠΩΣΕ
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    Procedure, //ΔΙΑΔΙΚΑΣΙΑ
    Method,    // ΜΕΘΟΔΟΣ
    Function,  // ΣΥΝΑΡΤΗΣΗ
    Let,       //ΕΣΤΩ
    From,      //ΑΠΟ
    Goto,      //ϰ
    Aggregate, //ΣΥΝΕΝΩΣΗ
    Sum,       //ΑΘΡΟΙΣΜΑ
}
