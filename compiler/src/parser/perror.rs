use core::fmt;

use crate::components::line::line::Line;

pub enum Severity
{
    Uncompilable,
    High,
    Unsafe,
    Warn,
    Low, 
}

pub struct ParserError<'a>
{
    msg: &'a str,
    severity: Severity,
    line: Line,
}

impl<'a> ParserError<'a>
{
    fn call(msg: &'a str, severity: Severity, line: Line) -> Self
    {
        Self
        {
            msg,
            severity,
            line,
        }
    }
}

impl<'a> std::fmt::Display for ParserError<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "Error at line {} {} {}\n", self.line.number, self.severity, self.msg);
        write!(f, "{}", self.line);
        write!(f, "^^^^^^^^^^^^^^")
    }
}

impl std::fmt::Display for Severity
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Severity::High =>  write!(f,"HIGH"),
            Severity::Low =>  write!(f,"LOW"),
            Severity::Warn =>  write!(f,"WARN"),
            Severity::Unsafe =>  write!(f,"UNSAFE"),
            Severity::Uncompilable =>  write!(f,"Uncompilable"),
        }
    }
}
