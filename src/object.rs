use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(obj) => write!(f, "{}", obj),
            Object::Error(err) => write!(f, "{}", err),
            Object::Null => write!(f, "null"),
        }
    }
}
