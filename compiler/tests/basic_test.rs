use compiler::components::token::tokentype::*;
use compiler::components::error::eureka_error;
use compiler::components::lexer::lex::{Lex};
use compiler::parser::*;

/// Checks if all tokens can be converted to strings
/// and displayed properlyt]
#[test]
fn token_eq()
{
    let plus = "+";
    assert_eq!(TokenType::from(plus), TokenType::Sep(Ok(Operator::Plus)));
    let indent = "\n     ";
    assert_eq!(TokenType::from(indent), TokenType::Sep(Err(Separator::Indent(5))));
    let paren = "(";
    assert_eq!(TokenType::from(paren), TokenType::Sep(Ok(Operator::LParen)));
}

#[test]
fn line_iterate()
{
 
}

#[test]
fn lex_test()
{
    let input = "defining module main
  f(x)+1
  l(x)
 f(x)
 y(x)
 \"d(v)\"";
        let l = Lex::lex(input);
        if let Some(s) = l
        {
            for t in s.root.data.contents.iter()
            {
                println!("{}", t)
            }
        }
}

