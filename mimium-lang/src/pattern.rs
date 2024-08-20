use crate::ast::Symbol;
//todo! need to replace with interned string.
use crate::types::Type;
use crate::utils::miniprint::MiniPrint;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Single(Symbol),
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
    pub id: Symbol,
    pub ty: Option<Type>,
}
impl std::fmt::Display for TypedId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(t) => write!(f, "{} :{t}", self.id),
            None => write!(f, "{}", self.id),
        }
    }
}

impl MiniPrint for TypedId {
    fn simple_print(&self) -> String {
        match &self.ty {
            Some(t) => format!("(tid {} {})", self.id, t), //todo:type
            None => self.id.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedPattern {
    pub pat: Pattern,
    pub ty: Option<Type>,
}
#[derive(Debug)]
pub struct ConversionError;
impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to convert pattern. The pattern did not matched to single identifier."
        )
    }
}
impl std::error::Error for ConversionError {}

impl From<TypedId> for TypedPattern {
    fn from(value: TypedId) -> Self {
        TypedPattern {
            pat: Pattern::Single(value.id),
            ty: value.ty,
        }
    }
}
impl TryFrom<TypedPattern> for TypedId {
    type Error = ConversionError;

    fn try_from(value: TypedPattern) -> Result<Self, Self::Error> {
        match value.pat {
            Pattern::Single(id) => Ok(TypedId { id, ty: value.ty }),
            Pattern::Tuple(_) => Err(ConversionError),
        }
    }
}

impl MiniPrint for TypedPattern {
    fn simple_print(&self) -> String {
        match &self.ty {
            Some(t) => format!("(tpat {} {})", self.pat, t), //todo:type
            None => self.pat.to_string(),
        }
    }
}
impl std::fmt::Display for TypedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(t) => write!(f, "{} :{t}", self.pat),
            None => write!(f, "{}", self.pat),
        }
    }
}
