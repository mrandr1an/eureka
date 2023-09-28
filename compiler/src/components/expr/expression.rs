use crate::components::token::tokentype::*;

pub enum Expression
{
    Information,
    Declaration,
    Location,
    Operation,
}

pub enum LocExpr
{
  //import {modulename} from {modulepath}
  Import(TokenType,TokenType, TokenType, TokenType),
  //define module {modulename}
  Init(TokenType,TokenType,TokenType),
}

pub enum OpExpr
{
 BinaryExpr(Box<OpExpr>,Operator,Box<OpExpr>),
 UnaryL(Box<OpExpr>, Operator),
 UnaryR(Operator, Box<OpExpr>),
}
