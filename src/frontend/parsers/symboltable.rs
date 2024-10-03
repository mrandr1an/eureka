use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    rc::{Rc, Weak},
};

use crate::frontend::syntaxtrees::{
    function::Function, identifier::Identifier, method::Method, procedure::Procedure, types::Type,
    variable::Variable,
};

pub enum Symbol {
    Procedure {
        params: Vec<Symbol>,
    },
    Function {
        params: Vec<Symbol>,
        ret: Type,
    },
    Method {
        params: Vec<Symbol>,
        ret: Option<Type>,
    },
    Param {
        _type: Type,
    },
    Variable {
        _type: Type,
    },
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param { _type } => write!(f, "{}", _type),
            _ => panic!("wowowo"),
        }
    }
}

pub struct SymbolTable<'a> {
    table: HashMap<Identifier<'a>, Symbol>,
    parent: Option<Weak<Table<'a>>>,
    children: Option<Rc<Table<'a>>>,
}

pub struct Table<'a>(RefCell<SymbolTable<'a>>);

impl<'a> Table<'a> {
    pub fn global() -> Rc<Self> {
        Rc::new(Self(RefCell::new(SymbolTable {
            table: HashMap::new(),
            parent: None,
            children: None,
        })))
    }

    pub fn new(parent: &Rc<Table<'a>>) -> Rc<Self> {
        Rc::new(Self(RefCell::new(SymbolTable {
            table: HashMap::new(),
            parent: Some(Rc::downgrade(parent)),
            children: None,
        })))
    }

    fn add_function(self: Rc<Self>, f: &Function<'a>) -> Option<Symbol> {
        let mut params = Vec::<Symbol>::new();
        for param in &f.params {
            params.push(Symbol::Param {
                _type: param._type.clone(),
            })
        }

        self.0.borrow_mut().table.insert(
            f.id.clone(),
            Symbol::Function {
                params,
                ret: f._type.clone(),
            },
        )
    }

    fn add_method(self: Rc<Self>, f: &Method<'a>) -> Option<Symbol> {
        let mut params = Vec::<Symbol>::new();
        for param in &f.params {
            params.push(Symbol::Param {
                _type: param._type.clone(),
            })
        }

        self.0.borrow_mut().table.insert(
            f.id.clone(),
            Symbol::Method {
                params,
                ret: f._type.clone(),
            },
        )
    }

    fn add_procedure(self: Rc<Self>, f: &Procedure<'a>) -> Option<Symbol> {
        let mut params = Vec::<Symbol>::new();
        for param in &f.params {
            params.push(Symbol::Param {
                _type: param._type.clone(),
            })
        }

        self.0
            .borrow_mut()
            .table
            .insert(f.id.clone(), Symbol::Procedure { params })
    }

    fn add_variable(self: Rc<Self>, f: &Variable<'a>) -> Option<Symbol> {
        self.0.borrow_mut().table.insert(
            f.name.clone(),
            Symbol::Variable {
                _type: f._type.clone(),
            },
        )
    }
}

impl<'a> Display for SymbolTable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = writeln!(f, "{: <15} | {: <15} | {: <15}", "Id", "Symbol", "Type");
        for (key, val) in self.table.iter() {
            let _ = match val {
                Symbol::Variable { _type } => {
                    writeln!(f, "{: <15} | {: <15} | {: <15}", key, "Variable", _type,)
                }
                Symbol::Function { params, ret } => {
                    let _ = writeln!(f, "{: <15} | {: <15} | ", key, "Function");
                    for param in params {
                        let _ = writeln!(f, "{: <15} ", param);
                    }
                    writeln!(f, "->{: <15} ", ret)
                }
                Symbol::Method { params, ret } => {
                    let _ = writeln!(f, "{: <15} | {: <15} | ", key, "Method");
                    for param in params {
                        let _ = writeln!(f, "{: <15} ", param);
                    }

                    if let Some(ret) = ret {
                        return writeln!(f, "->{: <15} ", ret);
                    } else {
                        Ok(())
                    }
                }
                Symbol::Procedure { params } => {
                    let _ = writeln!(f, "{: <15} | {: <15} | ", key, "Procedure");
                    for param in params {
                        let _ = writeln!(f, "{: <15} ", param);
                    }
                    Ok(())
                }
                _ => todo!(),
            };
        }
        todo!()
    }
}

impl<'a> Display for Table<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}

#[cfg(test)]
mod test {

}
