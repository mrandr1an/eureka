use std::fmt::{Debug, Display};

pub trait AST: Display + Debug + Eq + PartialEq {
    type Eval;
    /* Functions to turn an AST into something rust-native */
    fn into_eval(self) -> Self::Eval;
    fn eval(&self) -> &Self::Eval;
    fn eval_mut(&mut self) -> &mut Self::Eval;
}
