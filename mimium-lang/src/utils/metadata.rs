use crate::interner::{Symbol, ToSymbol};

#[derive(Clone,Debug)]
pub struct Location {
    span: std::ops::Range<usize>,
    path: String,
}
impl ariadne::Span for Location {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.path
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct WithMeta<T>{
//     pub location: Span,
//     pub value : T
// }
pub(crate) const GLOBAL_LABEL: &str = "_mimium_global";

// #[derive(Clone, Debug, PartialEq)]
// pub struct WithMeta<T: Clone + PartialEq>(pub T, pub Span);

// impl<T: Clone + PartialEq> WithMeta<T> {
//     fn map<F, O>(self, f: F) -> WithMeta<O>
//     where
//         F: FnOnce(T) -> O,
//         O: Clone + PartialEq,
//     {
//         WithMeta(f(self.0), self.1)
//     }
// }
