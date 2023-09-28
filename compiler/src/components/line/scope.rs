use std::rc::Rc;
use std::fmt;
use std::cell::RefCell;

use crate::components::line::line::Line;

pub struct Scope
{
    pub data: Line,
    pub edge: Rc<RefCell<Vec<Rc<Scope>>>>
}

impl Scope
{
    pub fn new(data:Line) -> Rc<Self>
    {
        Rc::new(Self
        {
            data,
            edge: Rc::new(RefCell::new(Vec::new()))
        })
    }

    pub fn insert_child(&self, line: Line)
    {
       self.edge.borrow_mut().push(Scope::new(line));
    }
}

impl std::fmt::Display for Scope
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f,"Line number {} the following children",self.data.number);
        for child in self.edge.borrow().iter()
        {
            write!(f,"Line number {}", child.data.number);
        }
        writeln!(f,"end")
    }  
}
