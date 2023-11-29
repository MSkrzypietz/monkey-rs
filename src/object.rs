use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::ast::{Ident, Program};
use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Array(Vec<Object>),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<Ident>, Program, Rc<RefCell<Environment>>),
    Builtin(fn(&[Object]) -> Object),
    Error(String),
    Null,
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::String(_) => "STRING".to_string(),
            Object::Array(_) => "ARRAY".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Function(_, _, _) => "FUNCTION".to_string(),
            Object::Builtin(_) => "BUILTIN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::String(str) => write!(f, "{}", str),
            Object::Array(_) => unimplemented!(),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(obj) => write!(f, "{}", obj),
            Object::Function(_, _, _) => unimplemented!(),
            Object::Builtin(_) => unimplemented!(),
            Object::Error(err) => write!(f, "{}", err),
            Object::Null => write!(f, "null"),
        }
    }
}
