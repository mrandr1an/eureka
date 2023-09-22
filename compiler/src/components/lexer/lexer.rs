//! Lexer is responsible for separating the literal strings of input (source code)
//! from the separators to output parsable tokens.

use std::collections::VecDeque;

use crate::components::line::line::{Line};
use crate::components::token::tokentype::*;
/// Lexed is the result of tokenizing a parsable
/// # Lexed | Lexeme Heap
///  More specifically Lexed is the structure that holds a structured form of the modules and lines
/// of eureka source code.
///  To break it down further its in a tree-like format.
///  From top to bottom we will call it the y axis and from left to right the x axis. This is the way
///  Lexed or (Lexeme Heap) should be traversed respectively:
///  - Main Module (Root, the special module called main that is always on top of the Lexeme Heap)
///  - Relatives of Modules (A collection of modules in the same y), ///  - Line Relatives (A collection of lines in the same y)
///  - Line families (A collection of lines in the same y as the parent of the specific y(x) line)
pub struct Lexed;

#[derive(PartialEq)]
enum State
{
    Start,
    FilledLine,
    Done,
}

impl Lexed
{
    pub fn lex<T: AsRef<str>>(input: T) -> Self
    {
        let chars_literal = input.as_ref();
        Self::init(chars_literal);
        Self        
    }

    fn init(input: &str)
    {
        let contents: &mut VecDeque<TokenType> = &mut VecDeque::new();
        let mut lines:  VecDeque<Line> = VecDeque::new();
        let iter = &mut input.chars();
        let mut line_pos = 0;
        let mut current_state = State::Start;

        while current_state != State::Done
        {
            Self::check_state(&mut current_state,iter, contents,&mut line_pos, &mut lines);
        }

        for line in lines
        {
            println!("{}", line)
        }
    }

    /// Fills one line
    fn fill_one<I>(input: &mut I, contents: &mut VecDeque<TokenType>,
                   line_pos: &mut usize, state: &mut State,lines: &mut VecDeque<Line>) 
        where
        I: Iterator<Item = char>
    {
        match input.next() {
            Some(ch) =>
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
                if ch.is_alphanumeric()
                {
                    //If is alphabetic
                    if ch.is_alphabetic()
                    {
                       //Push first character into string
                       let s = String::from(ch);
                       //Push rest of alpha characters into string
                       let alpha_token = Self::fill_alpha(input, s);
                       contents.push_back(TokenType::from(alpha_token))
                    }
                    // Else if is numberic
                    // then tokenize it like it is a number
                    // (eureka specific need to think about this)
                    else 
                    {
                        todo!()
                    }
                }
                else
                {
                    if ch == '='
                    {
                        //Check if next character is >
                        if let Some(next_char) = input.next()
                        {
                            if next_char == '>'
                            {
                                contents.push_back(TokenType::Sep(Ok(Operator::MatchAgainst)))
                            }
                            else
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::Equal)))
                            }
                        }
                        else
                        {
                           contents.push_back( TokenType::Pos(Err(LexerError::Done)));
                        }
                    }
                    else if ch == '-'
                    {
                        //Check if next character is >
                        if let Some(next_char) = input.next()
                        {
                            if next_char == '>'
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::PointTo)))
                            }
                            else
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::Minus)))
                            }
                        }
                        else
                        {
                           contents.push_back( TokenType::Pos(Err(LexerError::Done)))
                        }
                    }
                    else if ch == '>'
                    {
                        //Check if next character is =
                        if let Some(next_char) = input.next()
                        {
                            if next_char == '='
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::GrEq)))
                            }
                            else
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::Gr)))
                            }
                        }
                        else
                        {
                           contents.push_back( TokenType::Pos(Err(LexerError::Done)))
                        }
                    }
                    else if ch == '<'
                    {
                        //Check if next character is =
                        if let Some(next_char) = input.next()
                        {
                            if next_char == '='
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::LeEq)))
                            }
                            else
                            {
                                contents.push_back( TokenType::Sep(Ok(Operator::Le)))
                            }
                        }
                        else
                        {
                           contents.push_back( TokenType::Pos(Err(LexerError::Done)))
                        }
                    }
                    else if ch == ';'
                    {
                        todo!()
                    }
                    else if ch == '\n'
                    {
                        /*
                        if the number of spaces after \n
                        is larger than 0 then its a new line
                        -If indent of the new line is greater
                        than current lines then its a child line
                        -If its smaller or equal its a relative line
                        */
                        let current_line = Self::fill_current_line(contents, line_pos);
                        if let Some(indent) = Self::lex_indent(input, contents)
                        {
                            contents.push_back(indent);
                        }

                        lines.push_back(current_line);
                        
                        *state = State::FilledLine; 
                    }
                    else if ch == ' ' 
                    {
                        //Ignore and fill the next
                        // Self::fill_one(input, contents,line_pos,state, lines);
                    }
                    else
                    {
                        todo!()
                    }
                }
            },
            None =>
            {
                /* Lexing stage is done */
                // println!("Lexing stage is done");
                *state = State::Done;
            },
        }
    }

    fn fill_alpha<I>(input: &mut I, mut buffer: String) -> String
    where
        I: Iterator<Item = char>
    {
        while let Some(ch) = input.next()
        {
            if ch.is_alphabetic()
            {
                buffer.push(ch)
            }
            else
            {
                break
            }
        }
        buffer
    }

    fn fill_current_line<'a>(contents: &mut VecDeque<TokenType>, line_pos: &mut usize) -> Line
    {
        /*
          First TokenType of line is the Indentation of Line 
           If there is none then line is empty for now set indentation to zero
           If token is anything but Indentation then indentation is equal to zero
        */
        let line_indentation =
            match contents.get(0)
                {
                    Some(token) =>
                    {
                        if let TokenType::Sep(Err(Separator::Indent(level))) = token
                        {
                            *level
                        }
                        else
                        {
                            0
                        }
                    },
                    None =>
                    {
                        0
                    }
                };

        *line_pos = *line_pos + 1;
        let l = Line::new(line_indentation,*line_pos,contents.clone());
        //Clear contents
        contents.clear();
        l
    }

    fn lex_indent<I>(input: &mut I, contents: &mut VecDeque<TokenType>) -> Option<TokenType>
    where
        I: Iterator<Item = char>
    {
        let mut spaces = 0;
        while let Some(ch) = input.next()
        {
            if ch == ' '
            {
               spaces = spaces + 1; 
            }
            else
            {
                break;
            } 
        }

        if spaces > 0
        {
            Some(TokenType::Sep(Err(Separator::Indent(spaces))))
        }
        else
        {
            None
        }
    }
 
    fn check_state<I>(current_state: &mut State,input: &mut I, contents: &mut VecDeque<TokenType>,line_pos: &mut usize, lines: &mut VecDeque<Line>)
    where
        I: Iterator<Item = char>
    {
        match current_state
        {
            State::Start => Self::fill_one(input, contents, line_pos, current_state, lines),
            State::FilledLine =>
            {
                //Set state to start
                *current_state = State::Start;
                //recursively call Self::check_state
                Self::check_state(current_state,input,contents,line_pos,lines);
            },
            State::Done =>
            {
                ()
            },
        }
    }

   /// deprecated for now
   fn ignore<I>(input: &mut I) 
   where
     I: Iterator<Item = char>
    {
       input.next();
    }
}
