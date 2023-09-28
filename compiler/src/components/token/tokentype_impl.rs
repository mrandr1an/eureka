use super::tokentype::*;
use std::iter::FromIterator;
use std::cmp::Ordering;
use std::fmt;

impl TokenType
{
    /// A character value is recognized because a ' symbol
    /// has been detected
    fn tokenize_character(s: &str) -> Value
    {//Skip the first character since its a '
        //and add every character until another '
        // is detected
        let mut chars: Vec<char> = Vec::new();
        for c in s.chars().skip(1)
        {
            if c == '\''
            {
                break;
            }
            else
            {
                chars.push(c);
            }
        }
        Value::Character(chars)
    }

    ///An array value is recognized because a " symbol
    /// has been detected
    fn tokenize_array(s: &str) -> Value
    {
        //Skip the first character since its a "
        //and add every character until another "
        // is detected
        let mut elements: Vec<Value> = Vec::new();
        let mut element: String = String::new();
        for c in s.chars().skip(1)
        {
            if c == '\"'
            {
                break;
            }
            else
            {
                // If character is a space then
                // move to next element
                if c == ' '
                {
                  //push current element
                  elements.push(Value::from(&element));
                  //clear current element
                  element.clear() 
                }
                else
                {
                    //push character to current element
                    element.push(c);
                }
            }
        }
        Value::Array(elements)
    }

    //An number value is recognized because a number symbol
    // has been detected
    fn tokenize_number(s: &str) -> Value
    {
        let mut int_digits: Vec<char> = Vec::new();
        let mut float_digits: Vec<char> = Vec::new(); 
        let mut calculating_float = false;
        let _imaginary: Vec<char> = Vec::new();
        let _perc: usize = 0; 

        for digit in s.chars()
        {
            if digit.is_numeric()
            {
                if calculating_float
                {
                    float_digits.push(digit)
                }
                else
                {
                    int_digits.push(digit);
                }
            }
            else
            {
                if digit != '.'
                {
                    break;
                }
                else
                {
                    calculating_float = true;
                }
            }
        }
        if float_digits.is_empty()
        {
            return Value::Num(Number::Real(int_digits))
        }
        else
        {
            return Value::Num(Number::Float(int_digits,float_digits))
        }
    }

    //An identifier value is recognized because it is no other
    //value
    fn tokenize_identifier(s: &str) -> Value
    {
        Value::Id(String::from(s))
        //TODO check type
    }
}

impl<T> From<T> for TokenType
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self
    {
        //Check if its an operator
        match Operator::from(value.as_ref())
        {
            //Check if its a separator
            Operator::NotOp =>
            {
                match Separator::from(value.as_ref())
                {
                    //Check if its a value
                    Separator::NotSep =>
                    {
                        match Value::from(value.as_ref())
                        {
                           s => TokenType::Pos(Ok(s))
                        }
                    },
                    sep => TokenType::Sep(Err(sep))
                }
            }
                ,
            op => TokenType::Sep(Ok(op))
        }
    }
}


impl<T> From<T> for Operator
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self
    {
        match value.as_ref()
        {
            "define" => Operator::Define,
            "Meta" => Operator::Meta,
            "otherwise" => Operator::Otherwise,
            "when" => Operator::When,
            "halt" => Operator::Halt,
            "data" => Operator::Data, "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "mod" => Operator::Mod,
            "^" => Operator::Power,
            "|" => Operator::Abs,
            "!" => Operator::Fact,
            "=" => Operator::Equal,
            "." => Operator::Dot,
            "`" => Operator::Tik,
            "~" => Operator::Almost,
            "&" => Operator::Ref,
            "@" => Operator::Pointer,
            "deref" => Operator::Deref,
            "\'" => Operator::SingleQ,
            "\"" => Operator::DoubleQ,
            "(" => Operator::LParen,
            ")" => Operator::RParen,
            "[" => Operator::LSParen,
            "]" => Operator::RSParen,
            "{" => Operator::LCParen,
            "}" => Operator::RCParen,
            "=>" => Operator::MatchAgainst,
            "->" => Operator::PointTo,
            ";" => Operator::Comment,
            ".." => Operator::Range,
            "_" => Operator::PlaceHolder,
            "?" => Operator::Help,
            "," => Operator::Comma,
            "and" => Operator::And,
            "or" => Operator::Or,
            "not" => Operator::Not,
            "is" => Operator::Is,
            ">" => Operator::Gr,
            "<" => Operator::Le,
            ">=" => Operator::GrEq,
            "<=" => Operator::LeEq,
            _ => Operator::NotOp, 
        }
    }
}

impl<T> From<T> for Separator
where
    T: AsRef<str>,
{
    fn from(s: T) -> Self
    {
        match s.as_ref()
        {
            " " => Separator::Space,
            sep if s.as_ref().contains("\n") =>
            {
                if sep == "\n"
                {
                    return Separator::Endline
                }
                else
                {
                    let mut spaces = 0;
                    for c in sep.chars().skip(1)
                    {
                        if c.is_whitespace()
                        {
                            spaces = spaces + 1;
                        }
                        else
                        {
                            break;
                        }
                    }
                   return Separator::Indent(spaces) 
                }
            },
            _ =>  Separator::NotSep,
        }
    }
}


impl<T> From<T> for Value 
where
    T: AsRef<str>,
{
    fn from(s: T) -> Self
    {
        //If it starts with ' its a character
        if s.as_ref().starts_with('\'')
        {
          TokenType::tokenize_character(s.as_ref())
        }
        //If it start with " its an array
        else if s.as_ref().starts_with("\"")
        {
            TokenType::tokenize_array(s.as_ref())
        }
        //If it starts with a number its a number
        else if s.as_ref().starts_with(|c: char| c.is_digit(10))
        {
            TokenType::tokenize_number(s.as_ref())
        }
        //If its non of the above its an identifier
        else
        {
            TokenType::tokenize_identifier(s.as_ref())
        }
    }
}

impl fmt::Display for Operator
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "mod"),
            Operator::Power => write!(f, "^"),
            Operator::Abs => write!(f, "|"),
            Operator::Fact => write!(f, "!"),
            Operator::Equal => write!(f, "="),
            Operator::Dot => write!(f, "."),
            Operator::Tik => write!(f, "`"),
            Operator::Almost => write!(f, "~"),
            Operator::Ref => write!(f, "&"),
            Operator::Pointer => write!(f, "@"),
            Operator::Deref => write!(f, "deref"),
            Operator::SingleQ => write!(f, "\'"),
            Operator::DoubleQ => write!(f, "\""),
            Operator::LParen => write!(f, "("),
            Operator::RParen => write!(f, ")"),
            Operator::LSParen => write!(f, "["),
            Operator::RSParen=> write!(f, "]"),
            Operator::LCParen => write!(f, "{{"),
            Operator::RCParen => write!(f, "}}"),
            Operator::MatchAgainst => write!(f, "=>"),
            Operator::PointTo => write!(f, "->"),
            Operator::Comment => write!(f, ";"),
            Operator::Range => write!(f, ".."),
            Operator::PlaceHolder => write!(f, "_"),
            Operator::Help => write!(f, "?"),
            Operator::Comma => write!(f, ","),
            Operator::And => write!(f, "and"),
            Operator::Or => write!(f, "or"),
            Operator::Not => write!(f, "not"),
            Operator::Is => write!(f, "is"),
            Operator::Gr => write!(f, ">"),
            Operator::Le => write!(f, "<"),
            Operator::GrEq => write!(f, ">="),
            Operator::LeEq => write!(f, "<="),
            Operator::Meta => write!(f, "Meta"),
            Operator::Define => write!(f, "define"),
            Operator::Data => write!(f, "data"),
            Operator::When => write!(f, "when"),
            Operator::Otherwise => write!(f, "otherwise"),
            Operator::Halt => write!(f, "halt"),
            Operator::NotOp => write!(f, "Error NotOp"),
        }
    }
}

impl fmt::Display for Separator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self
        {
            Separator::Space => write!(f,"space"),
            Separator::Indent(level) => write!(f,"Indent{}",level),
            Separator::Endline => write!(f,"Endline"),
            Separator::NotSep => write!(f,"Error NotSep"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self
        {
            Value::Character(c) =>write!(f,"Character({})",c.into_iter().collect::<String>()),
            Value::Array(a) => write!(f,"Array({})", a.into_iter().collect::<String>()),
            Value::Num(n) => write!(f, "Number({})", n),

            Value::Id(name) => write!(f, "Id({})", name),
        }
    }
}

impl fmt::Display for Number
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Number::Float(integer,float)
                => write!(f, "{},{}", integer.into_iter().collect::<String>(), float.into_iter().collect::<String>()),
            Number::Real(integer)
                => write!(f,"{}", integer.into_iter().collect::<String>()),
            _ => todo!()
        }
    }
}

impl<'a> FromIterator<&'a Value> for String {
    fn from_iter<I: IntoIterator<Item = &'a Value>>(iter: I) -> Self {
        let mut result = String::new();
        
        for value in iter {
            result = result + " " + &value.to_string()
        }
        
        result
    }
}

impl fmt::Display for TokenType
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            TokenType::Pos(v) =>
            {
                match v
                {
                    Ok(v) => write!(f,"{v}"),
                    Err(_) => write!(f,"Error TokenType"),
                }
            },
            TokenType::Sep(s) =>
            {
                match s
                {
                 Ok(op) => write!(f,"{op}"),
                 Err(sep) => write!(f, "{sep}")
                }
            },
            TokenType::Done =>
            {
                write!(f,"EOF")
            }
        }
    }
}

impl PartialOrd for Separator
{

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let left = match *self
                {
                    Self::Indent(l) => l,
                    _ => 0,
                };
        let right = match *other
                {
                    Self::Indent(l) => l,
                    _ => 0,
                };
        Some(left.cmp(&right))
    } 
}

impl PartialOrd for TokenType
{

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let left = match *self
                {
                    Self::Sep(Err(Separator::Indent(l))) => l,
                    _ => 0,
                };
        let right = match *other
                {
                    Self::Sep(Err(Separator::Indent(l))) => l,
                    _ => 0,
                };
        Some(left.cmp(&right))
    } 
}
