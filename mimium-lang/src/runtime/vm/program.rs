use super::{Instruction, RawVal};
use crate::mir;
use crate::types::Type;
pub use mir::OpenUpValue;
use super::smallvec as smallvec_internal;
use smallvec_internal::*;
#[derive(Debug, Default, Clone, PartialEq)]
pub struct FuncProto {
    pub nparam: usize,
    pub nret: usize,
    pub upindexes: Vec<OpenUpValue>,
    pub bytecodes: Vec<Instruction>,
    pub constants: Vec<RawVal>,
    // feedvalues are mapped in this vector
    pub state_size: u64,
    pub delay_sizes:Vec<u64>,
}
pub(crate) type InstVec = Vec<Instruction>;
impl FuncProto {
    pub fn new(nparam: usize, nret: usize) -> Self {
        Self {
            nparam,
            nret,
            upindexes: Default::default(),
            bytecodes: Default::default(),
            constants: Default::default(),
            state_size: 0,
            delay_sizes: Default::default(),
        }
    }
    pub fn add_new_constant(&mut self, cval: RawVal) -> usize {
        self.constants.binary_search(&cval).unwrap_or_else(|_err| {
            self.constants.push(cval);
            self.constants.len() - 1
        })
    }
}

impl From<&mir::Function> for FuncProto {
    fn from(value: &mir::Function) -> Self {
        Self {
            nparam: value.args.len().into(),
            nret: 1,
            upindexes: Default::default(),
            bytecodes: Default::default(),
            constants: Default::default(),
            state_size: value.state_size,
            delay_sizes: Default::default(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Program {
    pub global_fn_table: Vec<(String, FuncProto)>,
    pub ext_fun_table: Vec<(String, Type)>,
    pub ext_cls_table: Vec<(String, Type)>,
    pub global_vals: Vec<RawVal>,
}
impl Program {
    pub fn get_fun_index(&self, name: &str) -> Option<usize> {
        self.global_fn_table
            .iter()
            .position(|(label, _f)| label.as_str() == name)
    }
    pub fn get_dsp_fn(&self) -> Option<&FuncProto> {
        self.get_fun_index("dsp")
            .and_then(|idx| self.global_fn_table.get(idx).map(|(_, f)| f))
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fns in self.global_fn_table.iter() {
            let _ = write!(f, "{}\n", fns.0);
            let _ = write!(f, "nparams:{} nret: {}\n", fns.1.nparam, fns.1.nret);
            let _ = write!(f, "upindexes: {:?}  ", fns.1.upindexes);
            let _ = write!(f, "state_size: {}  \n", fns.1.state_size);
            let _ = write!(f, "constants:  {:?}\n", fns.1.constants);
            let _ = write!(f, "instructions:\n");
            for inst in fns.1.bytecodes.iter() {
                let _ = write!(f, "  {}\n", inst);
            }
        }
        let _ = write!(
            f,
            "ext_fun:\n{:?}\n",
            self.ext_fun_table
                .iter()
                .fold("".to_string(), |s, (f, _)| if s.is_empty() {
                    format!("{f}")
                } else {
                    format!("{s}, {f}")
                })
        );
        let _ = write!(f, "ext_cls:\n{:?}\n", self.ext_cls_table);
        write!(f, "globals:\n{:?}", self.global_vals)
    }
}
