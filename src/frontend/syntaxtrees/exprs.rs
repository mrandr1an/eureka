use std::fmt::{Debug, Display};

use crate::frontend::lexers::token::Operator as OpToken;

use super::{value::Value, variable::Variable};

pub struct Expr<'a> {
    variant: ExprVariant<'a>,
    next: Option<Box<Expr<'a>>>,
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = writeln!(f, "{}", self.variant);
        match &self.next {
            Some(expr) => writeln!(f, "{expr}"),
            None => Ok(()),
        }
    }
}

pub enum ExprVariant<'a> {
    Binary(Binary<'a>),
    UnaryLHS(UnaryLHS<'a>),
    UnaryRHS(UnaryRHS<'a>),
    Assign(Assign<'a>),
    Id(Value<'a>),
}

impl<'a> Display for ExprVariant<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(a) => write!(f, "({a})"),
            Self::Binary(b) => write!(f, "({b})"),
            Self::Id(i) => write!(f, "({i})"),
            Self::UnaryLHS(u) => write!(f, "({u})"),
            Self::UnaryRHS(u) => write!(f, "({})", u),
        }
    }
}

impl<'a> Debug for ExprVariant<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(a) => write!(f, "({a})"),
            Self::Binary(b) => write!(f, "({b})"),
            Self::Id(i) => write!(f, "({i})"),
            Self::UnaryLHS(u) => write!(f, "({u})"),
            Self::UnaryRHS(u) => write!(f, "({})", u),
        }
    }
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    pub fn bp(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

impl Debug for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "+"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

pub struct Binary<'a> {
    lhs: Box<ExprVariant<'a>>,
    op: BinOp,
    rhs: Box<ExprVariant<'a>>,
}

impl<'a> Display for Binary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} {} {}>", self.lhs, self.op, self.rhs)
    }
}

impl<'a> Debug for Binary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} {} {}>", self.lhs, self.op, self.rhs)
    }
}

pub enum PrefixOp {
    Goto,
    AdrOf,
}

impl PrefixOp {
    pub fn bp(&self) -> ((), u8) {
        match self {
            Self::Goto => ((), 1),
            Self::AdrOf => ((), 6),
        }
    }
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Goto => write!(f, "ΠΗΓΑΙΝΕ "),
            Self::AdrOf => write!(f, "ΔΙΕΥΘΥΝΣΗ "),
        }
    }
}

pub struct UnaryLHS<'a> {
    op: PrefixOp,
    rhs: Box<ExprVariant<'a>>,
}

impl<'a> Display for UnaryLHS<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} {}>", self.op, self.rhs)
    }
}

impl<'a> Debug for UnaryLHS<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} {}>", self.op, self.rhs)
    }
}

pub enum PostfixOp<'a> {
    Fact,
    Call(Vec<Value<'a>>),
}

impl<'a> PostfixOp<'a> {
    pub fn bp(&self) -> (u8, ()) {
        match self {
            Self::Call(_) => (16, ()),
            Self::Fact => (5, ()),
        }
    }
}

impl<'a> Display for PostfixOp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fact => write!(f, "!"),
            Self::Call(params) => {
                for val in params {
                    let _ = write!(f, "{val} ");
                }
                Ok(())
            }
        }
    }
}

impl<'a> Debug for PostfixOp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fact => write!(f, "!"),
            Self::Call(params) => {
                for val in params {
                    let _ = write!(f, "<Param {val}> ");
                }
                Ok(())
            }
        }
    }
}

pub struct UnaryRHS<'a> {
    lhs: Box<ExprVariant<'a>>,
    op: PostfixOp<'a>,
}

impl<'a> Display for UnaryRHS<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.lhs, self.op)
    }
}

impl<'a> Debug for UnaryRHS<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.lhs, self.op)
    }
}

pub struct Assign<'a> {
    var: Variable<'a>,
    expr: Box<ExprVariant<'a>>,
}

impl<'a> Display for Assign<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Assign {}<={}>", self.var, self.expr)
    }
}

impl<'a> Debug for Assign<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Assign {}<={}>", self.var, self.expr)
    }
}

pub enum Operator<'a> {
    Pre(PrefixOp),
    Post(PostfixOp<'a>),
    Mid(BinOp),
}

impl<'a> Operator<'a> {
    pub fn bp(&self) -> (Option<u8>, Option<u8>) {
        match self {
            Self::Pre(pre) => (None, Some(pre.bp().1)),
            Self::Post(post) => (Some(post.bp().0), None),
            Self::Mid(bin) => (Some(bin.bp().0), Some(bin.bp().1)),
        }
    }
}

impl<'a> Display for Operator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pre(pre) => write!(f, "{pre}"),
            Self::Post(post) => write!(f, "{post}"),
            Self::Mid(bin) => write!(f, "{bin}"),
        }
    }
}
