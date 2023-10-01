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
    assert_eq!(TokenType::from(indent), TokenType::Sep(Err(Separator::Indent(5))))
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
 d(v)";
        let l = Lex::lex(input);
        if let Some(s) = l
        {
            let r = s.root.toproot();
            let c = r.edge.borrow();
            let mut e = c.iter();
            while let Some(v) = e.next()
            {
                let mut a = v.data.contents.iter();
                match a.next()
                {
                    Some(token) =>
                    {
                        println!("{}",token);
                    },
                    None =>
                    {
                        println!("Nothing here anymore")
                    }
                }
            }
        }
}

