use super::{ast::AST, perror::ParserError};


use std::rc::Rc;
use crate::components::{lexer::lex::Lex, line::{scope::Scope, line::Line}, token::tokentype::{TokenType, Operator, Separator}};
use crate::parser::values;

pub struct Parser
{

}

impl Parser
{
    fn parse(lex: &mut Lex) -> Result<AST, ParserError>
    {
        todo!()
    }

    fn children_checker(scope: Rc<Scope>)
    {
        match scope.edge.borrow().is_empty()
        {
            true => todo!(),
            false => todo!()
        }
    }

    fn parse_line(line: Line)
    {
      let line_iter = &mut line.contents.iter();
    }

    fn iter_line<I>(iter: &mut I) -> ()
    where
        I: Iterator<Item = TokenType>
    {
        // Next token
        match iter.next()
        {
            // If token exists
            Some(token) =>
            { 
                match token
                {
                    TokenType::Sep(separator) =>
                    {
                        match separator
                        {
                            //If separator is operator check what it operates
                            Ok(op) =>
                            {
                                match op
                                {
                                    Operator::Define => Self::define(iter),
                                    _ => todo!(), 
                                }
                            },
                            //If separator is operator check what it separates
                            Err(sep) => todo!(),
                        }
                    },
                    TokenType::Pos(possible) => todo!(),
                    TokenType::Done => todo!(),
                }
            },
            None => todo!(),
        }
    }

    fn define<I>(iter: &mut I) -> ()
    where
        I: Iterator<Item = TokenType>
    {
        // match iter.next()
        // {

        // }
    }
}
