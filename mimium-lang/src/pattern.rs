use crate::ast::Symbol;
use crate::interner::{with_session_globals, SessionGlobals, TypeNodeId};
//todo! need to replace with interned string.
use crate::types::Type;
use crate::utils::metadata::Span;
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
    pub ty: Option<TypeNodeId>,
}

impl TypedId {
    pub fn to_span(&self) -> Option<&Span> {
        with_session_globals(|session_globals| match &self.ty {
            Some(tid) => unsafe {
                let span = std::mem::transmute::<&Span, &Span>(session_globals.get_span(*tid));
                Some(span)
            },
            None => None,
        })
    }
}

impl std::fmt::Display for TypedId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(t) => write!(f, "{} :{}", self.id, t.to_type()),
            None => write!(f, "{}", self.id),
        }
    }
}

impl MiniPrint for TypedId {
    fn simple_print(&self) -> String {
        match &self.ty {
            Some(t) => format!("(tid {} {})", self.id, t.to_type()), //todo:type
            None => self.id.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedPattern {
    pub pat: Pattern,
    pub ty: Option<TypeNodeId>,
}

impl TypedPattern {
    pub fn to_span(&self) -> Option<&Span> {
        with_session_globals(|session_globals| match &self.ty {
            Some(tid) => unsafe {
                let span = std::mem::transmute::<&Span, &Span>(session_globals.get_span(*tid));
                Some(span)
            },
            None => None,
        })
    }
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
            Some(t) => format!("(tpat {} {})", self.pat, t.to_type()), //todo:type
            None => self.pat.to_string(),
        }
    }
}
impl std::fmt::Display for TypedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(t) => write!(f, "{} :{}", self.pat, t.to_type()),
            None => write!(f, "{}", self.pat),
        }
    }
}
