//use this file replaces all the Vec<T> definitions.
//Note that you can't use vec![] macro for initialization.

use smallvec::SmallVec;
pub(super) type Vec<T> = SmallVec::<[T;10]>;