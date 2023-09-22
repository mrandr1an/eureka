use std::fmt;

use super::line::Line;

impl<'a> fmt::Display for Line
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {

        for (index,token) in self.contents.clone().into_iter().enumerate()
        { 
            if index == (self.contents.len() - 1)
            {
                let _ = write!(f, "{}\n", token);
            }
            else
            {
                let _ = write!(f, "{}", token);
            }
        }
        let _ = write!(f,"Line|Indetation|Linenumber|\n");
        write!(f,"    |    {}    |    {}    |",self.indent, self.number)
    }
}
