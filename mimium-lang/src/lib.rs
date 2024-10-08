#![feature(iterator_try_collect)]
#![feature(iter_collect_into)]
#![feature(if_let_guard)]

pub mod ast;
pub mod interner;
pub mod mir;
pub(crate) mod pattern;
pub mod types;
pub mod utils;

pub mod compiler;
pub mod runtime;
use std::cell::LazyCell;

pub struct ExecContext {
    pub compiler: compiler::Context,
    // pub vm: Option<runtime::vm::Machine>,
    pub extfuns: Vec<ExtFnInfo>,
    pub extcls: Vec<ExtClsInfo>,
    //pub plugins: Vec<Plugin>
}

use interner::{Symbol, ToSymbol};
use runtime::{
    scheduler::{Scheduler, SyncScheduler},
    vm::{self, ExtClsInfo, ExtFnInfo},
};
impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    pub fn new(
        additional_extfuns: &[ExtFnInfo],
        extcls: &[ExtClsInfo],
        file_path: Option<Symbol>,
    ) -> Self {
        let mut extfuns = runtime::vm::builtin::get_builtin_fns().to_vec();
        extfuns.append(&mut additional_extfuns.to_vec());
        let mut extfuntypes = extfuns
            .iter()
            .map(|(name, _, ty)| (name.to_symbol(), *ty))
            .collect::<Vec<_>>();
        let mut extclstypes = extcls
            .iter()
            .map(|(name, _, ty)| (name.to_symbol(), *ty))
            .collect::<Vec<_>>();
        extfuntypes.append(&mut extclstypes);
        let compiler = compiler::Context::new(&extfuntypes, file_path);

        Self {
            compiler,
            extfuns,
            extcls: extcls.to_vec(),
        }
    }
    pub fn prepare_machine(&mut self, src: &str) -> vm::Machine {
        let prog = self.compiler.emit_bytecode(src).unwrap();
        vm::Machine::new(
            Some(Box::new(SyncScheduler::new())),
            prog,
            &self.extfuns,
            &self.extcls,
        )
    }
}

//todo: remove
pub mod ast_interpreter;
pub mod repl;
