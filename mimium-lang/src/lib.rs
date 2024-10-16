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

use compiler::ExtFunTypeInfo;
use interner::Symbol;
use plugin::{to_ext_cls_info, DynSystemPlugin, Plugin, SystemPlugin};
use runtime::vm::{self, ExtClsInfo, Program};
// use slotmap::SlotMap;

// slotmap::new_key_type! {}struct PluginInstanceId;

/// A set of compiler and external functions (plugins).
/// From this information, user can generate VM with [`Self::prepare_machine`].
pub struct ExecContext {
    pub compiler: Option<compiler::Context>,
    pub vm: Option<runtime::vm::Machine>,
    pub plugins: Vec<Box<dyn Plugin>>,
    pub sys_plugins: Vec<DynSystemPlugin>,
    path: Option<Symbol>,
    extclsinfos_reserve: Vec<ExtClsInfo>,
    extfuntypes: Vec<ExtFunTypeInfo>,
}
impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    pub fn new(plugins: impl Iterator<Item = Box<dyn Plugin>>, path: Option<Symbol>) -> Self {
        let plugins = plugins.collect::<Vec<_>>();
        let extfuntypes = plugin::get_extfun_types(&plugins).collect();
        let sys_plugins = vec![];
        Self {
            compiler: None,
            vm: None,
            plugins,
            sys_plugins,
            path,
            extclsinfos_reserve: vec![],
            extfuntypes,
        }
    }
    pub fn add_plugin<T: Plugin + 'static>(&mut self, plug: T) {
        self.plugins.push(Box::new(plug))
    }
    //todo: make it to builder pattern
    pub fn add_system_plugin<T: SystemPlugin + 'static>(&mut self, plug: T) {
        let (plugin_dyn, sysplug_info) = to_ext_cls_info(plug);
        let sysplug_typeinfo = sysplug_info
            .iter()
            .cloned()
            .map(|(name, _, ty)| ExtFunTypeInfo { name, ty });
        self.extfuntypes.extend(sysplug_typeinfo);
        self.extclsinfos_reserve.extend(sysplug_info);
        self.sys_plugins.push(plugin_dyn)
    }
    pub fn prepare_compiler(&mut self) {
        self.compiler = Some(compiler::Context::new(self.extfuntypes.clone(), self.path));
    }
    pub fn prepare_machine(&mut self, src: &str) {
        if self.compiler.is_none() {
            self.prepare_compiler();
        }

        let prog = self.compiler.as_ref().unwrap().emit_bytecode(src).unwrap();

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
