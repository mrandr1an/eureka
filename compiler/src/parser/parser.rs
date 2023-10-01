use super::{ast::AST, perror::ParserError};


use std::rc::Rc;
use crate::components::{lexer::lex::Lex, line::{scope::Scope, line::Line}};

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
}
