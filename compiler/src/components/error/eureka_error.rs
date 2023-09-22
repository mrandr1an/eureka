use std::fmt;

const NOT_IMPLEMENTED_ERROR: &str = "Feature has not been implemented yet";
const LEXER_ERROR: &str = "Lexer error detected";
const PARSER_ERROR: &str = "Parser error detected";
const ERROR_ERROR: &str = "Internal compiler error, this should not have happened";

pub enum EurekaError<>
{
    LexerError(&'static str,String ),
    ParserError(&'static str,String),
    NotImplemented(&'static str,String), 
    ErrorError(&'static str,String),
}

