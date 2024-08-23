use std::{cell::RefCell, collections::BTreeMap, hash::Hash};

use slotmap::{Key, KeyData, SlotMap};
use string_interner::{backend::StringBackend, StringInterner};

use crate::{ast::Expr, dummy_span, types::Type, utils::metadata::Span};

pub struct SessionGlobals {
    pub symbol_interner: StringInterner<StringBackend<usize>>,
    pub expr_storage: SlotMap<ExprNodeId, Expr>,
    pub type_storage: SlotMap<TypeNodeId, Type>,
    pub span_storage: BTreeMap<NodeId, Span>,
}

impl SessionGlobals {
    fn store_expr(&mut self, expr: Expr) -> ExprNodeId {
        self.expr_storage.insert(expr)
    }

    fn store_span<T: ToNodeId>(&mut self, node_id: T, span: Span) {
        self.span_storage.insert(node_id.to_node_id(), span);
    }

    pub fn store_type(&mut self, ty: Type) -> TypeNodeId {
        self.type_storage.insert(ty)
    }

    pub fn store_expr_with_span(&mut self, expr: Expr, span: Span) -> ExprNodeId {
        let expr_id = self.store_expr(expr);
        self.store_span(expr_id, span);
        expr_id
    }

    pub fn store_type_with_span(&mut self, ty: Type, span: Span) -> TypeNodeId {
        let type_id = self.store_type(ty);
        self.store_span(type_id, span);
        type_id
    }

    pub fn get_expr(&self, expr_id: ExprNodeId) -> &Expr {
        unsafe { self.expr_storage.get_unchecked(expr_id) }
    }

    pub fn get_type(&self, type_id: TypeNodeId) -> &Type {
        unsafe { self.type_storage.get_unchecked(type_id) }
    }

    pub fn get_span<T: ToNodeId>(&self, node_id: T) -> Option<&Span> {
        self.span_storage.get(&node_id.to_node_id())
    }
}

thread_local!(static SESSION_GLOBALS: RefCell<SessionGlobals> =  RefCell::new(
    SessionGlobals {
        symbol_interner: StringInterner::new(),
        expr_storage: SlotMap::with_key(),
        type_storage: SlotMap::with_key(),
        span_storage: BTreeMap::new()
    }
));

pub fn with_session_globals<R, F>(f: F) -> R
where
    F: FnOnce(&mut SessionGlobals) -> R,
{
    SESSION_GLOBALS.with_borrow_mut(f)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeId {
    ExprArena(KeyData),
    TypeArena(KeyData),
}

// Note: I wanted to use slotmap::DefaultKey, but it already implements
// PartialEq, which we want to implement differently.

#[derive(Debug, Clone, Copy, Default, Eq, PartialOrd, Ord)]
pub struct ExprNodeId(KeyData);

#[derive(Debug, Clone, Copy, Default, Eq, PartialOrd, Ord)]
pub struct TypeNodeId(KeyData);

// traits required for Key trait

impl PartialEq for ExprNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_expr() == other.to_expr() && self.to_span() == self.to_span()
    }
}

// Note: this implementation is the same as the default implementation. But,
// clippy would deny it if I use #[derive(Hash)].
//
// cf. https://rust-lang.github.io/rust-clippy/master/index.html#/derived_hash_with_manual_eq
impl Hash for ExprNodeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq for TypeNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_type() == other.to_type()
    }
}

impl Hash for TypeNodeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

// Key trait

unsafe impl Key for ExprNodeId {
    fn data(&self) -> KeyData {
        self.0
    }
}

impl From<KeyData> for ExprNodeId {
    fn from(value: KeyData) -> Self {
        Self(value)
    }
}

unsafe impl Key for TypeNodeId {
    fn data(&self) -> KeyData {
        self.0
    }
}

impl From<KeyData> for TypeNodeId {
    fn from(value: KeyData) -> Self {
        Self(value)
    }
}

impl ExprNodeId {
    pub fn to_expr(&self) -> &Expr {
        with_session_globals(|session_globals| unsafe {
            // This transmute is needed to convince the borrow checker. Since
            // the session_global should exist until the end of the session,
            // this &Expr should live sufficiently long.
            std::mem::transmute::<&Expr, &Expr>(session_globals.get_expr(*self))
        })
    }

    pub fn to_span(&self) -> &Span {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&Span, &Span>(
                session_globals.get_span(*self).expect("Unknown ID"),
            )
        })
    }
}

impl TypeNodeId {
    pub fn to_type(&self) -> &Type {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&Type, &Type>(session_globals.get_type(*self))
        })
    }

    pub fn to_span(&self) -> &Span {
        with_session_globals(|session_globals| match session_globals.get_span(*self) {
            Some(span) => unsafe { std::mem::transmute::<&Span, &Span>(span) },
            None => &dummy_span!(),
        })
    }
}

pub trait ToNodeId {
    fn to_node_id(&self) -> NodeId;
}

impl ToNodeId for ExprNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::ExprArena(self.data())
    }
}

impl ToNodeId for TypeNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::TypeArena(self.data())
    }
}
