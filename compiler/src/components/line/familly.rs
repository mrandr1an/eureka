use super::line::Line;

pub struct Familly
{
    parent: Line,
    child: Option<Vec<Familly>>  
}

impl Familly
{
    fn new(parent: Line) -> Self
    {
        Self
        {
            parent,
            child: Some(Vec::new()),
        }
    }
}
