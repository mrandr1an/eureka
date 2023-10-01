use crate::components::{token::tokentype::{TokenType, Operator, Separator, Value}};

pub struct define
{
    id: Value,
    t: Operator, //type of define {Fn, Seq, (), module}
}

pub struct expression
{
    t: Operator, //Postfix, infix, prefix
    operands: Value
}
