
use super::{RawVal};
use slotmap::{new_key_type, DefaultKey, SlotMap};
new_key_type! {pub struct RawObjectKey;}
new_key_type! {pub struct ClosureObjectKey;}

pub(super) struct ReferenceCounted<T: Clone> {
    count: usize,
    data: T,
}

pub(super) trait Object {
    fn as_raw_values(&self) -> &[RawVal];
}

//values escaped in the closure.
pub(super) struct RawValObject(ReferenceCounted<Vec<RawVal>>);
pub(super) struct RawObjectStorage(SlotMap<RawObjectKey, RawValObject>);

#[derive(Debug, Default, PartialEq)]
//closure object dynamically allocated
pub(crate) struct Closure {
    pub fn_proto_pos: usize, //position of function prototype in global_ftable
    pub base_ptr: u64,       //base pointer to current closure, to calculate open upvalue
    pub is_closed: bool,
    pub(self) upvalues: Vec<SharedUpValue>,
    state_storage: StateStorage,
}

pub(super) struct ClosureObject(ReferenceCounted<Closure>);
