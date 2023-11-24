use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self { store: HashMap::new(), outer: None }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Self { store: HashMap::new(), outer: Some(outer) }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name).cloned() {
            Some(obj) => Some(obj),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None
            }
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}