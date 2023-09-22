use super::line::Line;

pub struct Familly<'a>
{
    parent: Line,
    child: Option<&'a Familly<'a>>  
}

impl<'a> Familly<'a>
{
    fn new(parent: Line, child: &'a Familly<'a>) -> Self
    {
        Self
        {
            parent,
            child: Some(child),
        }
    }
}
