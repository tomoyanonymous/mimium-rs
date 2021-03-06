use std::collections::{HashMap, LinkedList};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment<T>(pub LinkedList<HashMap<String, T>>);

impl<T> Environment<T> {
    pub fn new() -> Self {
        Self(LinkedList::<HashMap<String, T>>::new())
    }
}

pub struct Error(String);

impl<T: Clone> Environment<T> {
    pub fn extend(&mut self) {
        self.0.push_front(HashMap::new());
    }
    pub fn add_bind(&mut self, name: String, e: T) {
        assert!(self.0.len() > 0);
        self.0.front_mut().unwrap().insert(name, e);
    }

    pub fn get_bound_value(&self, name: String) -> Option<&T> {
        let mut res: Option<&T> = None;
        for hashmap in self.0.iter() {
            let r = hashmap.get(&name);
            if let Some(_v) = r {
                res = r;
                break;
            }
        }
        res
    }
}
