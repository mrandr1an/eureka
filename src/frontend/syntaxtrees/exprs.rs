use std::fmt::{Debug, Display};

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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "+"),
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
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Goto => write!(f, "ΠΗΓΑΙΝΕ "),
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
