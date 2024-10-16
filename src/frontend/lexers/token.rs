use std::{fmt::Display, ops::Range};

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
                | &"&"
        )
    }
}

///Contains the range of the original string where the substring "lexeme" resides
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub lexeme: Lexeme<'a>,
    pub kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn new(range: Range<usize>, slice: &'a str) -> Self {
        Self {
            lexeme: Lexeme::new(slice, range),
            kind: TokenKind::from(slice),
        }
    }

    pub fn is_op(&self) -> bool {
        matches!(self.kind, TokenKind::Reserved(Reserved::Op(_)))
    }

    pub fn is_prefix(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Reserved(Reserved::Op(Operator::Prefix(_)))
        )
    }

    pub fn is_postfix(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Reserved(Reserved::Op(Operator::Postfix(_)))
        )
    }

    pub fn is_midfix(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Reserved(Reserved::Op(Operator::Infix(_)))
        )
    }

    pub fn is_reserved(&self) -> bool {
        matches!(self.kind, TokenKind::Reserved(_))
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self.kind, TokenKind::Reserved(Reserved::Keyword(_)))
    }

    pub fn is_num(&self) -> bool {
        matches!(self.kind, TokenKind::Number(_))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, TokenKind::Unknown)
    }
}

impl From<&str> for TokenKind {
    fn from(value: &str) -> Self {
        match value {
            /* Reserved Operators */
            "+" => TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Add))),
            "-" => TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Sub))),
            "*" => TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Mul))),
            "/" => TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Div))),
            "!" => TokenKind::Reserved(Reserved::Op(Operator::Postfix(Postfix::Fact))),
            "," => TokenKind::Reserved(Reserved::Op(Operator::Postfix(Postfix::Comma))),
            "." => TokenKind::Reserved(Reserved::Op(Operator::Postfix(Postfix::Dot))),
            ".." => TokenKind::Reserved(Reserved::Op(Operator::Postfix(Postfix::DotDot))),
            "&" => TokenKind::Reserved(Reserved::Op(Operator::Prefix(Prefix::AdrOf))),
            "(" => TokenKind::Reserved(Reserved::Op(Operator::ParenL)),
            ")" => TokenKind::Reserved(Reserved::Op(Operator::ParenR)),
            "[" => TokenKind::Reserved(Reserved::Op(Operator::BracketL)),
            "]" => TokenKind::Reserved(Reserved::Op(Operator::BracketR)),
            "{" => TokenKind::Reserved(Reserved::Op(Operator::CBracketL)),
            "}" => TokenKind::Reserved(Reserved::Op(Operator::CBracketR)),
            "\"" => TokenKind::Reserved(Reserved::Op(Operator::DoubleQ)),
            "'" => TokenKind::Reserved(Reserved::Op(Operator::SingleQ)),
            /* Reserved keywords */
            "ΣΥΝΑΡΤΗΣΗ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Function)),
            "ΜΕΘΟΔΟΣ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Method)),
            "ΔΙΑΔΙΚΑΣΙΑ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Procedure)),
            "ΣΥΝΕΝΩΣΗ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Aggregate)),
            "ΑΘΡΟΙΣΜΑ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Sum)),
            "ΕΣΤΩ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Let)),
            "ΑΠΟ" => TokenKind::Reserved(Reserved::Keyword(Keyword::From)),
            "ϰ" => TokenKind::Reserved(Reserved::Keyword(Keyword::Goto)),
            /* Reserved types */
            "ΠΡΑΓΜΑΤΙΚΟΣ" => TokenKind::Builtin(Builtin::Type(Type::Real)),
            "ΧΑΡΑΚΤΗΡΑΣ" => TokenKind::Builtin(Builtin::Type(Type::Char)),
            /* Reserved procedure/functions/methods */
            "ΕΚΤΥΠΩΣΕ" => TokenKind::Builtin(Builtin::Function(Function::Print)),
            /*Non reserved at lexing time */
            token => match token.parse::<i64>() {
                Ok(i) => TokenKind::Number(i),
                Err(_err) => TokenKind::Unknown,
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenKind {
    //Reserved Keywords
    Reserved(Reserved),
    //Types, values, functions generated "automagically" from the compiler
    Builtin(Builtin),
    //Identifier, value or type,
    Unknown,
    Number(i64),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Reserved {
    Op(Operator),
    Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Postfix {
    Fact,
    Dot,
    DotDot,
    Comma,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Prefix {
    AdrOf,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Builtin {
    Type(Type),
    Function(Function),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Real,           //ΠΡΑΓΜΑΤΙΚΟΣ
    Char,           //ΧΑΡΑΚΤΗΡΑΣ
    Ptr(Box<Type>), //ex. ΠΡΑΓΜΑΤΙΚΟΣ*
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Function {
    Print, //ΕΚΤΥΠΩΣΕ
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}
