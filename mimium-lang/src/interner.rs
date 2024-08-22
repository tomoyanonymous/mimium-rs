use std::{cell::RefCell, collections::BTreeMap};

use id_arena::{Arena, Id};
use string_interner::{backend::StringBackend, StringInterner};

use crate::{ast::Expr, types::Type, utils::metadata::Span};

pub struct SessionGlobals {
    pub symbol_interner: StringInterner<StringBackend<usize>>,
    pub expr_storage: Arena<Expr>,
    pub type_storage: Arena<Type>,
    pub span_storage: BTreeMap<NodeId, Span>,
}

impl SessionGlobals {
    fn store_expr(&mut self, expr: Expr) -> ExprNodeId {
        ExprNodeId(self.expr_storage.alloc(expr))
    }

    fn store_type(&mut self, ty: Type) -> TypeNodeId {
        TypeNodeId(self.type_storage.alloc(ty))
    }

    fn store_span<T: ToNodeId>(&mut self, node_id: T, span: Span) {
        self.span_storage.insert(node_id.to_node_id(), span);
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
        self.expr_storage
            .get(expr_id.0)
            .expect("Unknown ExprNodeId")
    }

    pub fn get_type(&self, type_id: TypeNodeId) -> &Type {
        self.type_storage
            .get(type_id.0)
            .expect("Unknown TypeNodeId")
    }

    pub fn get_span<T: ToNodeId>(&self, node_id: T) -> &Span {
        self.span_storage
            .get(&node_id.to_node_id())
            .expect("Unknown NodeID")
    }
}

thread_local!(static SESSION_GLOBALS: RefCell<SessionGlobals> =  RefCell::new(
    SessionGlobals {
        symbol_interner: StringInterner::new(),
        expr_storage: Arena::new(),
        type_storage: Arena::new(),
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
    ExprArena(usize),
    TypeArena(usize),
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord)]
pub struct ExprNodeId(pub Id<Expr>);

#[derive(Debug, Clone, Copy, PartialOrd, Ord)]
pub struct TypeNodeId(pub Id<Type>);

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
            std::mem::transmute::<&Span, &Span>(session_globals.get_span(*self))
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
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&Span, &Span>(session_globals.get_span(*self))
        })
    }
}

impl PartialEq for TypeNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_type() == other.to_type() && self.to_span() == self.to_span()
    }
}

impl Eq for TypeNodeId {}

impl ToNodeId for TypeNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::TypeArena(self.0.index())
    }
}

pub trait ToNodeId {
    fn to_node_id(&self) -> NodeId;
}

impl ToNodeId for ExprNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::ExprArena(self.0.index())
    }
}

// ExprNodeId needs to implement Ord in order to be a key of BTreeMap. To implement
// Ord, Rust requires PartialEq, Eq, and PartialOrd. PartialEq needs to be
// implemented manually so that the actual expressions and spans are compared.

impl PartialEq for ExprNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_expr() == other.to_expr() && self.to_span() == self.to_span()
    }
}

impl Eq for ExprNodeId {}
