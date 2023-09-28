use std::fmt;

use super::line::Line;
use crate::components::token::tokentype::TokenType;

impl<'a> fmt::Display for Line
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {

        for (index,token) in self.contents.clone().into_iter().enumerate()
        { 
            if index == (self.contents.len() - 1)
            {
                let _ = write!(f, "{}\t", token);
                let _ = write!(f,"Indent:{} Number:{}\n",self.indent,self.number);
            }
            else
            {
                let _ = write!(f, "{}", token);
            }
        }
        Ok(())
    }
}

impl Iterator for Line {
    type Item = TokenType;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.contents.pop_front() {
            Some(item)
        } else {
            None
        }
    }
}
