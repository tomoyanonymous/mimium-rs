//todo! need to replace with interned string.
use crate::types::Type; 
use crate::utils::miniprint::MiniPrint;
pub type Id = String;


#[derive(Debug, Clone,PartialEq)]
pub enum Pattern {
    Single(Id),
    Tuple(Vec<Self>),
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Single(id) => write!(f, "{id}"),
            Pattern::Tuple(vec) => {
                let s = vec
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .concat();
                write!(f, "({s})")
            }
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypedId {
    pub id: Id,
    pub ty: Option<Type>,
}

impl MiniPrint for TypedId {
    fn simple_print(&self) -> String {
        match &self.ty {
            Some(t) => format!("(tid {} {})", self.id, t), //todo:type
            None => self.id.clone(),
        }
    }
}

#[derive(Clone, Debug,PartialEq)]
pub struct TypedPattern {
    pub pat: Pattern,
    pub ty: Option<Type>,
}
impl MiniPrint for TypedPattern {
    fn simple_print(&self) -> String {
        match &self.ty {
            Some(t) => format!("(tpat {} {})", self.pat, t), //todo:type
            None =>self.pat.to_string(),
        }
    }
}