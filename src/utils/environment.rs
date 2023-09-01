use std::collections::{HashMap, LinkedList};

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

/// Environment as a temporary object that holds mutable reference to the vector of key-value-pair.
/// The environment is initialized by adding a vector of key value pair and remove them automatically when it is destroyed.
struct EnvironmentT<'a, T: Clone>(&'a mut Vec<(String, T)>, usize);

impl<'a, T: Clone> EnvironmentT<'a, T> {
    pub fn new(vec: &'a mut Vec<(String, T)>, mut names: Vec<(String, T)>) -> Self {
        let len = vec.len();
        vec.append(&mut names);
        Self(vec, len)
    }
    pub fn lookup(&self, name: &String) -> Option<T> {
        let res = self
            .0
            .iter()
            .rev()
            .filter(|(n, _v)| name == n)
            .collect::<Vec<_>>();
        res.get(0).map(|(_, v)| v.clone())
    }
}

impl<'a, T: Clone> Drop for EnvironmentT<'a, T> {
    fn drop(&mut self) {
        println!("drop called");
        self.0.truncate(self.1);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn eval_environment() {
        let mut envdata = Vec::new();
        let src1 = vec!["hoge", "fuga", "fuge"]
            .iter()
            .map(|s| (s.to_string(), format!("{}_value", s)))
            .collect::<Vec<_>>();
        {
            let _env = EnvironmentT::<'_, String>::new(&mut envdata, src1);
            assert_eq!(_env.0.len(), 3);
        }
        assert_eq!(envdata.len(), 0);
        let src2 = vec!["poge", "puga"]
            .iter()
            .map(|s| (s.to_string(), format!("{}_value", s)))
            .collect::<Vec<_>>();
        {
            let _env2 = EnvironmentT::<'_, String>::new(&mut envdata, src2);
            assert_eq!(_env2.0.len(), 2);
        }
        assert_eq!(envdata.len(), 0);
    }
}
