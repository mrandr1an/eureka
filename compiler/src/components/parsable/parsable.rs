type AST = String;
use crate::components::expr;

pub trait Parsable<T: AsRef<str>>
{
    type EurekaError;
    fn parse(&self, input: T) -> Result<AST,Self::EurekaError>;
    fn tokenize(&self, input: T) -> Result<AST, Self::EurekaError>;
    fn generate(&self, input: T) -> Result<AST, Self::EurekaError>;
}
