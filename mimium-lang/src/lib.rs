//! Main module of compiler and runtime for **mimium**, an infrastructural programming language for sound and music.

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

pub mod plugin;

/// A set of compilr and external functions (plugins).
/// From this information, user can generate VM with [`Self::prepare_machine`].

pub struct ExecContext {
    pub compiler: compiler::Context,
    // pub vm: Option<runtime::vm::Machine>,
    pub plugins: Vec<Arc<dyn Plugin>>,
}

use std::sync::Arc;

use interner::{Symbol, ToSymbol};
use plugin::Plugin;
use runtime::{
    scheduler::{Scheduler, SyncScheduler},
    vm::{self, ExtClsInfo, ExtFnInfo},
};
impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    pub fn new(plugins: &[Arc<dyn Plugin>], file_path: Option<Symbol>) -> Self {
        let extfuntypes = plugin::get_extfun_types(plugins)
            .into_iter()
            .collect::<Vec<_>>();
        let compiler = compiler::Context::new(&extfuntypes, file_path);
        let plugins = Vec::from(plugins);
        Self { compiler, plugins }
    }
    pub fn prepare_machine(&mut self, src: &str) -> vm::Machine {
        let prog = self.compiler.emit_bytecode(src).unwrap();
        vm::Machine::new(
            Some(Box::new(SyncScheduler::new())),
            prog,
            plugin::get_extfuninfos(&self.plugins),
            plugin::get_extclsinfos(&self.plugins),
        )
    }
}

//todo: remove
pub mod ast_interpreter;
pub mod repl;
