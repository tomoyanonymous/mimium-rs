use std::collections::LinkedList;
type EnvInner<T> = LinkedList<Vec<(String, T)>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment<T>(pub EnvInner<T>);

impl<T> Environment<T> {
    pub fn new() -> Self {
        Self(EnvInner::new())
    }
}

pub struct Error(String);

impl<T: Clone> Environment<T> {
    pub fn extend(&mut self) {
        self.0.push_front(Vec::new());
    }
    pub fn to_outer(&mut self) {
        let _ = self.0.pop_front();
    }
    pub fn add_bind(&mut self, binds: &mut Vec<(String, T)>) {
        assert!(self.0.len() > 0);
        self.0.front_mut().unwrap().append(binds);
    }

    pub fn lookup(&self, name: &String) -> Option<&T> {
        self.0
            .iter()
            .find(|vec| vec.iter().find(|(n, _)| n == name).is_some())
            .map(|vec| vec.iter().find(|(n, _)| n == name).map(|(_, v)| v))
            .flatten()
    }
}
