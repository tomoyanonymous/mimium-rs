use std::collections::LinkedList;

pub struct Environment<T> {
    value: LinkedList<(String, T)>,
}

impl<T> Environment<T> {
    pub fn addBind(&mut self, name: String, e: T) {
        self.value.push_front((name, e));
    }
    pub fn getBoundValue(&self,name:String){
        
    }
}
