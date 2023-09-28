use std::rc::Rc;
use std::fmt;
use std::cell::RefCell;

use crate::components::line::line::Line;

pub struct Scope
{
    pub data: Line,
    pub parent: Option<Rc<Scope>>,
    pub edge: Rc<RefCell<Vec<Rc<Scope>>>>
}

impl Scope
{
    pub fn new(data:Line, parent: Option<Rc<Scope>>) -> Rc<Self>
    {
        Rc::new(Self
        {
            parent,
            data,
            edge: Rc::new(RefCell::new(Vec::new()))
        })
    }

    pub fn insert_child(&self, child_scope: Rc<Scope>)
    {
       self.edge.borrow_mut().push(child_scope);
    }

    pub fn toproot(&self) -> &Self
    {
        match &self.parent
        {
            Some(p) => p.toproot(),
            None => self,
        }
    }

}

impl std::fmt::Display for Scope
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f,"Line number {} the following children",self.data.number);
        for child in self.edge.borrow().iter()
        {
            write!(f,"Line number {}\n", child.data.number);
        }
        writeln!(f,"end")
    }  
}
