pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq)]
pub struct WithMeta<T>{
    pub location: Span,
    pub value : T
}