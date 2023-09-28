use crate::components::token::tokentype::{TokenType, Separator, Operator};
use crate::components::line::line::Line;
use crate::components::line::scope::Scope;

use std::rc::Rc;
use std::collections::VecDeque;
use std::iter::Peekable;

pub struct Lex
{
}

impl Lex
{
    /// Lexes input into a data-structure that is easily parsable 
    pub fn lex<T>(input: T) -> ()
    where
        T: AsRef<str>
    {
        let input_iter = &mut input.as_ref().chars().peekable();
        let lines = Self::lex_lines(input_iter);
        let scopes = Self::transform(lines);
        for scope in scopes
        {
            println!("{}", scope)
        }
    }

    /// Gets a peekable iterator of some input and iterates over it (Items are just single characters)
    /// until a separator or a space is detected depending on the situation below.
    /// It by design should never fail or panic but returns TokenType::Done when there is nothing more to lex.
    /// # Separators
    ///  A separator can only stop being iterated after a space
    /// # Non Separators
    ///  A non separator (alphanumerical literals) stop being iterated after a seperator is detected
    /// # Return 
    /// When iteration stops then a value of TokenType is returned
    /// # Spaces and Endlines
    /// Spaces and endline tokens are skipped.
    /// # Example
    /// ```
    /// use compiler::components::lexer::lex::Lex;
    /// use compiler::components::token::tokentype::*;
    /// let input_iter = &mut "x + y".chars().peekable();
    /// let first_token = Lex::next_token(input_iter);
    /// let second_token = Lex::next_token(input_iter);
    /// let third_token = Lex::next_token(input_iter);
    /// let fourth_token = Lex::next_token(input_iter);
    /// let fifth_token = Lex::next_token(input_iter);
    /// assert_eq!(first_token, TokenType::Pos(Ok(Value::Id("x".to_string()))));
    /// assert_eq!(second_token, TokenType::Sep(Ok(Operator::Plus))); 
    /// assert_eq!(third_token, TokenType::Pos(Ok(Value::Id("y".to_string())))); 
    /// assert_eq!(fourth_token, TokenType::Done); 
    /// assert_eq!(fifth_token, TokenType::Done); 
    /// ```
    pub fn next_token<I>(input: &mut Peekable<I>) -> TokenType
    where I: Iterator<Item = char> 
    {
        //Check if next_token exists
        match input.next()
        {
            Some(ch) =>
            {
                /*
                If its alphanumerical its not a separator:
                if its alphabetical keep inserting
                if its numerical keep inserting accordingly (think about how eureka numbers work)
                If its a separator check:
                If its endline then check if its indent or not
                Ignore endline 
                If its a = or a =>
                If its a - or a ->
                If its a > or a >=
                If its a < or a <=
                If its a comment (;)
                Otherwise continue pushing chars until a space is detected
                Else push chars until separator is detected
                Ignore space
                 */
                match ch
                {
                    alphanum if ch.is_alphanumeric() => Self::lex_alphanumeric(input,String::from(alphanum)),
                    separator => Self::lex_separator(input, String::from(separator)),
                }
            },
            None =>
            {
                TokenType::Done
            }
        }
    }

    /// Decides based on the first character whether to lex a number value or a alpha-literal value
    /// # Usage
    /// Designed to be used inside Self::next_token, do not forget to String::from(first_character_of_str)
    /// since it immediately moves the iterator afterwards 
    /// # Example
    /// ```
    ///  //let input = "+".chars().peekable()
    ///  //let token_type = lex_alphanumeric(input, String::from(input)) 
    ///  // This will panic because its a separator, when used correctly it should never happen
    /// ```
    fn lex_alphanumeric<I>(input: &mut Peekable<I>, buffer: String) -> TokenType
    where I: Iterator<Item = char>
    {
        // This should always be Some(ch)
        let c = buffer.chars().nth(0).expect("This should never happen");
        match c
        {
            literal if c.is_alphabetic() => Self::lex_alphabetic(input,&mut String::from(literal)),
            digit if c.is_numeric() || c == '.' || c == '%'=> Self::lex_numeric(input,&mut String::from(digit)),  
            e => panic!("Expected to lex an alphanumerical lexeme but got separator {e} instead")
        }
    }

    /// Responsible for lexing alphabetic lexemes
    /// # Algorithm
    /// The function uses a while let loop to iterate over the input characters using the peek method.
    /// The loop continues as long as there are characters available to peek at.
    /// Inside the loop, it checks if the current character (ch) is alphabetic using the is_alphabetic method.
    /// If it is, the character is appended to the buffer using the push method,and then the next.
    /// If the current character is not alphabetic, the loop is exited.
    /// Finally, after the loop, the function returns a TokenType value 
    /// # Identifiers
    /// As a result of the algorithm identifiers should be able to have names that end in numbers
    /// and anything else as long as they start with an alphabetic character
    /// # Values
    /// Lexer is not responsible for directly creating values so for example chars
    /// are lexed separately with separators '
    fn lex_alphabetic<I>(input: &mut Peekable<I>, buffer: &mut String) -> TokenType
     where I: Iterator<Item = char>
    {
        while let Some(ch) = input.peek()
        {
            if ch.is_alphabetic()
            {
                buffer.push(*ch);
                input.next();
            }
            else
            {
                break;
            }
        }
        TokenType::from(buffer)
    }

    /// Need to think about how eureka numbers will work
    fn lex_numeric<I>(input: &mut Peekable<I>, buffer: &mut String) -> TokenType
     where I: Iterator<Item = char>
    {
        while let Some(ch) = input.peek()
        {
            if ch.is_numeric()
            {
                buffer.push(*ch);
                input.next();
            }
            else
            {
                break;
            }
        }
        TokenType::from(buffer)
    }

    fn lex_separator<I>(input: &mut Peekable<I>, buffer: String) -> TokenType
     where I: Iterator<Item = char>
    {
                /*  If its a separator check:
                    If its endline then check if its indent or not
                      Ignore endline 
                    If its a = or a =>
                    If its a - or a ->
                    If its a > or a >=
                    If its a < or a <=
                    If its a comment (;)
                Otherwise continue pushing chars until a space is detected
                Else push chars until separator is detected
                Ignore space
         */
        if let Some(ch) = buffer.chars().nth(0)
        {
            if ch == '='
            {
                if let Some(next_char) = input.peek()
                {
                    if *next_char == '>'
                    {
                        let _ = input.next();
                        TokenType::Sep(Ok(Operator::MatchAgainst))
                    }
                    else
                    {
                        TokenType::Sep(Ok(Operator::Equal))
                    }
                }
                else
                {
                    TokenType::Done
                }
            }
            else if ch == '-'
            {
              if let Some(next_char) = input.peek()
                {
                    if *next_char == '>'
                    {
                        let _ = input.next();
                        TokenType::Sep(Ok(Operator::PointTo))
                    }
                    else
                    {
                        TokenType::Sep(Ok(Operator::Minus))
                    }
                }
                else
                {
                    TokenType::Done
                }
            }
            else if ch == '>'
            {
              if let Some(next_char) = input.peek()
                {
                    if *next_char == '='
                    {
                        let _ = input.next();
                        TokenType::Sep(Ok(Operator::GrEq))
                    }
                    else
                    {
                        TokenType::Sep(Ok(Operator::Gr))
                    }
                }
                else
                {
                    TokenType::Done
                } 
            }
            else if ch == '<'
            {
             if let Some(next_char) = input.peek()
                {
                    if *next_char == '='
                    {
                        let _ = input.next();
                        TokenType::Sep(Ok(Operator::LeEq))
                    }
                    else
                    {
                        TokenType::Sep(Ok(Operator::Le))
                    }
                }
                else
                {
                    TokenType::Done
                } 
            } 
            else
            {
                if ch == ' '
                {
                    Self::next_token(input)
                }
                else if ch == '\n'
                {
                    let mut spaces = 0;
                    while let Some(next_ch) = input.peek()
                    {
                        if *next_ch == ' '
                        {
                            input.next();
                            spaces = spaces + 1; 
                        }
                        else
                        {
                            break;
                        }
                    }
                    TokenType::Sep(Err(Separator::Indent(spaces)))
                }
                else
                {
                  TokenType::from(buffer)
                }
            }
        }
        else
        {
            TokenType::Done
        }
    }

    fn lex_lines<I>(input: &mut Peekable<I>) -> VecDeque<Line>
    where
        I: Iterator<Item = char>
    {
        // Starting position is 1
        let mut pos = 1;
        // Starting indent is always going to be 0 aswell
        let mut indent = 0;
        let mut lines: VecDeque<Line> = VecDeque::new();
        while let (Some(line),indent_of_next) = Self::next_line(input,pos,indent)
        {
            lines.push_back(line);
            pos = pos + 1;
            match indent_of_next
            {
                Some(i) =>
                {
                    indent = i;
                }
                None =>
                {
                    break;
                }
            };
        }
        lines
    }

    fn next_line<I>(input: &mut Peekable<I>,pos: usize, indent: usize) -> (Option<Line>, Option<usize>)
    where
        I: Iterator<Item = char>
    {
        let mut token = Self::next_token(input);
        let mut indent_of_next = 0;
        let mut contents: VecDeque<TokenType> = VecDeque::new();
        while token != TokenType::Done
        {
            match token
            {
                TokenType::Sep(Err(Separator::Indent(level))) =>
                {
                    indent_of_next = level;
                    break;
                }
                t =>
                {
                   //Cloning for now cant figure out a better way
                    contents.push_back(t);
                    token = Self::next_token(input); 
                }
            }
        }
        if token == TokenType::Done
        {
            (Some(Line::new(pos,indent,contents)), None)
        }
        else
        {
        (Some(Line::new(pos,indent,contents)), Some(indent_of_next))
        }
    }

    fn transform(mut lines: VecDeque<Line>) -> Vec<Rc<Scope>>
    {
        let mut scopes: Vec<Rc<Scope>> = Vec::new();
        while let Some(current_line) = lines.pop_front()
        {
            let current_in = current_line.indent;
            let current_scope = Scope::new(current_line);
            for next_line in lines.iter()
            {
                if current_in <  next_line.indent
                {
                   current_scope.insert_child(next_line.clone())
                }
                else
                {
                    break;
                }
            }
            scopes.push(current_scope);
        }
        scopes
    }
}

