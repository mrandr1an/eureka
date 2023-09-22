use crate::components::line::familly::Familly;
use crate::components::line::line::Line;

/*

*/
pub struct Module<'a>
{
  root: Line,
  relatives: Familly<'a>,
}

