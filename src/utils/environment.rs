use std::collections::LinkedList;
type EnvInner<T> = LinkedList<Vec<(String, T)>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment<T>(pub EnvInner<T>);

pub struct Error(String);

#[derive(Clone, Debug, PartialEq)]
pub enum LookupRes<T: Clone> {
    Local(T),
    UpValue(T),
    Global(T),
    None,
}
impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        let mut res = Self(EnvInner::new());
        res.extend();
        res
    }
    pub fn is_global(&self) -> bool {
        self.0.len() <= 1
    }
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
    pub fn lookup_cls(&self, name: &String) -> LookupRes<&T> {
        match self
            .0
            .iter()
            .enumerate()
            .find(|(level, vec)| vec.iter().find(|(n, _)| n == name).is_some())
            .map(|(level, vec)| vec.iter().find(|(n, _)| n == name).map(|(_, v)| (level, v)))
            .flatten()
        {
            None => LookupRes::None,
            Some((0, e)) => LookupRes::Local(e),
            Some((level, e)) if level == self.0.len() - 1 => LookupRes::Global(e),
            Some((_level, e)) => LookupRes::UpValue(e),
        }
    }
}
