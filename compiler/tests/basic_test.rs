use compiler::components::token::tokentype::*;
use compiler::components::error::eureka_error;
use compiler::components::lexer::lex::Lex;

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
fn lex_test()
{
    let input = "set main\ngoodbye +    hello\ni+wow \n    kafig";
    Lex::lex(input);
}

