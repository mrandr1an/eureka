use std::fmt::Display;

use crate::frontend::{
    lexers::{
        lexer::Lexer,
        token::{Infix, Operator, Prefix, Reserved, TokenKind},
    },
    parsers::lib::parser::{until, Flatten, Pair},
    syntaxtrees::{
        exprs::{BinOp, Operator as Op, PostfixOp, PrefixOp},
        value::Value,
    },
};

use super::{
    lib::{
        error::Perror,
        parser::{either, just, Combinator, Parser},
        result::{Either, ParserR},
    },
    value,
};

pub enum S<'a> {
    Atom(Value<'a>),
    Cons(Op<'a>, Vec<S<'a>>),
}

impl<'a> Display for S<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(value) => write!(f, "{}", value),
            Self::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

pub fn binop(input: Lexer) -> ParserR<BinOp, Perror> {
    either(just("+"), just("-"))
        .or(either(just("*"), just("/")))
        .flatten()
        .flatten()
        .map(|token| match token.kind {
            TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Add))) => BinOp::Add,
            TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Sub))) => BinOp::Sub,
            TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Mul))) => BinOp::Mul,
            TokenKind::Reserved(Reserved::Op(Operator::Infix(Infix::Div))) => BinOp::Div,
            e => panic!("will never happen {:#?}", e),
        })
        .parse(input)
}

pub fn postop(input: Lexer) -> ParserR<PostfixOp, Perror> {
    either(just("!"), just("(").then(until(")", value::value)).right())
        .map(|op| match op {
            Either::Left(r) => PostfixOp::Fact,
            Either::Right(call) => PostfixOp::Call(call),
        })
        .parse(input)
}

pub fn preop(input: Lexer) -> ParserR<PrefixOp, Perror> {
    just("&")
        .map(|token| match token.kind {
            TokenKind::Reserved(Reserved::Op(Operator::Prefix(Prefix::AdrOf))) => PrefixOp::AdrOf,
            e => panic!("will never happen {:#?}", e),
        })
        .parse(input)
}

pub fn op(input: Lexer) -> ParserR<Op, Perror> {
    todo!()
}

pub fn expr(input: Lexer) -> ParserR<S, Perror> {
    expr_bp(input, 0)
}

pub fn expr_bp(mut input: Lexer, min_bp: u8) -> ParserR<S, Perror> {
    match either(value::value, preop).parse(input.clone()) {
        ParserR::Ok((res, rest1)) => {
            let mut lhs = match res {
                Either::Left(value) => {
                    input = rest1;
                    S::Atom(value)
                }
                Either::Right(op) => {
                    let (_, r_bp) = op.bp();
                    let (rhs, rest) = match expr_bp(rest1, r_bp) {
                        ParserR::Ok((output, rest)) => (output, rest),
                        ParserR::Err(err) => return ParserR::Err(err),
                    };
                    input = rest;
                    S::Cons(Op::Pre(op), vec![rhs])
                }
            };
            loop {
                let (op, rest2) = match either(binop, postop).or(just(".")).parse(input.clone()) {
                    ParserR::Ok((Either::Left(output), rest3)) => match output {
                        Either::Left(binop) => (binop, rest3),
                        Either::Right(postop) => {
                            let (l_bp, _) = postop.bp();
                            if l_bp < min_bp {
                                break;
                            }
                            input = rest3;
                            lhs = S::Cons(Op::Post(postop), vec![lhs]);
                            continue;
                        }
                    },
                    ParserR::Ok((Either::Right(_), _)) => break,
                    ParserR::Err(err) => return ParserR::Err(err),
                };

                let (l_bp, r_bp) = op.bp();
                if l_bp < min_bp {
                    break;
                }

                input = rest2;
                let (rhs, rest3) = match expr_bp(input, r_bp) {
                    ParserR::Ok((output, rest4)) => (output, rest4),
                    ParserR::Err(err) => return ParserR::Err(err),
                };
                input = rest3;
                lhs = S::Cons(Op::Mid(op), vec![lhs, rhs]);
            }
            ParserR::Ok((lhs, input))
        }
        ParserR::Err(err) => ParserR::Err(err),
    }
}

#[cfg(test)]
mod test {
    use crate::frontend::{
        lexers::lexer::Lexer,
        parsers::lib::{error::ParserError, parser::Parser, result::ParserR},
        syntaxtrees::exprs::ExprVariant,
    };

    use super::{expr, postop};

    #[test]
    fn simple_pratt() -> miette::Result<()> {
        let lexer = Lexer::new("main", "45 + &2 + 2! * hello(1 1).");
        match expr.parse(lexer) {
            ParserR::Ok((output, _rest)) => {
                assert_eq!(
                    output.to_string(),
                    "(+ (+ <Value Real 45> (ΔΙΕΥΘΥΝΣΗ  <Value Real 2>)) (* (! <Value Real 2>) (<Value Real 1> <Value Real 1>  <Value hello>)))"
                )
            }
            ParserR::Err(err) => return Err(err.report()),
        }
        Ok(())
    }

    #[test]
    fn postop_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main", "('f' 32 34)");
        match postop.parse(lexer) {
            ParserR::Ok((output, _rest)) => {
                assert_eq!(
                    output.to_string(),
                    "(+ (+ <Value Real 45> (ΔΙΕΥΘΥΝΣΗ  <Value Real 2>)) (* <Value Real 2> <Value Real 4>))"
                )
            }
            ParserR::Err(err) => return Err(err.report()),
        }
        Ok(())
    }

    #[test]
    fn expr_parse() -> miette::Result<()> {
        let lexer = Lexer::new("main", "add(1 2) + 2.");
        match expr.parse(lexer) {
            ParserR::Ok((output, _rest)) => {
                assert_eq!(
                    output.to_string(),
                    "(+ (<Params <Value Real 1> <Value Real 2> > <Value add>) <Value Real 2>)"
                );
                println!("{}", ExprVariant::new(output));
            }
            ParserR::Err(err) => return Err(err.report()),
        }
        Ok(())
    }

    #[test]
    fn random_test() -> miette::Result<()> {
        let lexer = Lexer::new("main", "45 + 2! + &12 + 5(3) * 3.");
        match expr.parse(lexer) {
            ParserR::Ok((output, _rest)) => {
                assert_eq!(output.to_string(), "<Value Real 45>");
            }
            ParserR::Err(err) => return Err(err.report()),
        }
        Ok(())
    }
}
