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
use std::cell::RefCell;
use std::rc::Rc;

use interner::Symbol;
use plugin::{to_ext_cls_info, Plugin, SysPluginDyn, SystemPlugin};
use runtime::vm::{self, ExtClsInfo, Program};
// use slotmap::SlotMap;

// slotmap::new_key_type! {}struct PluginInstanceId;

/// A set of compiler and external functions (plugins).
/// From this information, user can generate VM with [`Self::prepare_machine`].
pub struct ExecContext {
    pub compiler: compiler::Context,
    pub vm: Option<runtime::vm::Machine>,
    pub plugins: Vec<Box<dyn Plugin>>,
    pub sys_plugins: Vec<SysPluginDyn>,
    extclsinfos_reserve: Vec<ExtClsInfo>,
}
impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    pub fn new(plugins: impl Iterator<Item = Box<dyn Plugin>>, file_path: Option<Symbol>) -> Self {
        let plugins = plugins.collect::<Vec<_>>();
        let extfuntypes = plugin::get_extfun_types(&plugins)
            .into_iter()
            .collect::<Vec<_>>();
        let compiler = compiler::Context::new(&extfuntypes, file_path);
        let sys_plugins = vec![];
        Self {
            compiler,
            vm: None,
            plugins,
            sys_plugins,
            extclsinfos_reserve: vec![],
        }
    }
    pub fn add_system_plugin<T: SystemPlugin + 'static>(&mut self, plug: T) {
        let plug = Rc::new(RefCell::new(plug));
        self.extclsinfos_reserve
            .extend(to_ext_cls_info(plug.clone()));
        self.sys_plugins.push(SysPluginDyn(plug))
    }
    pub fn prepare_machine(&mut self, src: &str) {
        let prog = self.compiler.emit_bytecode(src).unwrap();

        self.prepare_machine_with_bytecode(prog);
    }
    pub fn prepare_machine_with_bytecode(&mut self, prog: Program) {
        self.extclsinfos_reserve
            .extend(plugin::get_extclsinfos(&self.plugins));
        self.vm = Some(vm::Machine::new(
            prog,
            plugin::get_extfuninfos(&self.plugins),
            self.extclsinfos_reserve.clone().into_iter(),
        ));
    }
}

//todo: remove
pub mod ast_interpreter;
pub mod repl;
