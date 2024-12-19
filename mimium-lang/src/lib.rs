//! Main module of compiler and runtime for **mimium**, an infrastructural programming language for sound and music.

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
pub use log;
use plugin::{to_ext_cls_info, DynSystemPlugin, Plugin, SystemPlugin};
use runtime::vm::{
    self,
    builtin::{get_builtin_fn_types, get_builtin_fns},
    ExtClsInfo, Program, ReturnCode,
};
use utils::error::ReportableError;

#[cfg(feature = "native")]
use mimalloc::MiMalloc;
#[cfg(feature = "native")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// A set of compiler and external functions (plugins).
/// From this information, user can generate VM with [`Self::prepare_machine`].
pub struct ExecContext {
    compiler: Option<compiler::Context>,
    vm: Option<runtime::vm::Machine>,
    plugins: Vec<Box<dyn Plugin>>,
    sys_plugins: Vec<DynSystemPlugin>,
    path: Option<Symbol>,
    extclsinfos_reserve: Vec<ExtClsInfo>,
    extfuntypes: Vec<ExtFunTypeInfo>,
}

impl ExecContext {
    //The Argument will be changed to the plugins, when the plugin system is introduced
    pub fn new(plugins: impl Iterator<Item = Box<dyn Plugin>>, path: Option<Symbol>) -> Self {
        let plugins = plugins.collect::<Vec<_>>();
        let extfuntypes = plugin::get_extfun_types(&plugins)
            .chain(get_builtin_fn_types())
            .collect::<Vec<_>>();

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
    pub fn get_system_plugins(&self) -> impl Iterator<Item = &DynSystemPlugin> {
        self.sys_plugins.iter()
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
    pub fn get_compiler(&self) -> Option<&compiler::Context> {
        self.compiler.as_ref()
    }
    pub fn take_vm(&mut self) -> Option<runtime::vm::Machine> {
        self.vm.take()
    }
    pub fn get_vm(&self) -> Option<&runtime::vm::Machine> {
        self.vm.as_ref()
    }
    pub fn get_compiler_mut(&mut self) -> Option<&mut compiler::Context> {
        self.compiler.as_mut()
    }
    pub fn get_vm_mut(&mut self) -> Option<&mut runtime::vm::Machine> {
        self.vm.as_mut()
    }
    pub fn prepare_compiler(&mut self) {
        self.compiler = Some(compiler::Context::new(self.extfuntypes.clone(), self.path));
    }
    pub fn prepare_machine(&mut self, src: &str) -> Result<(), Vec<Box<dyn ReportableError>>> {
        if self.compiler.is_none() {
            self.prepare_compiler();
        }

        let prog = self.compiler.as_ref().unwrap().emit_bytecode(src)?;
        self.prepare_machine_with_bytecode(prog);
        Ok(())
    }
    pub fn prepare_machine_with_bytecode(&mut self, prog: Program) {
        self.extclsinfos_reserve
            .extend(plugin::get_extclsinfos(&self.plugins));
        let extfninfos = plugin::get_extfuninfos(&self.plugins).chain(get_builtin_fns());
        let vm = vm::Machine::new(
            prog,
            extfninfos,
            self.extclsinfos_reserve.clone().into_iter(),
        );
        self.vm = Some(vm);
    }
    pub fn try_get_main_loop(&mut self) -> Option<Box<dyn FnOnce()>> {
        let mut mainloops = self.sys_plugins.iter_mut().filter_map(|p| {
            let p = unsafe { p.0.get().as_mut().unwrap_unchecked() };
            p.try_get_main_loop()
        });
        let res = mainloops.next();
        if mainloops.next().is_some() {
            log::warn!("more than 2 main loops in system plugins found")
        }
        res
    }
    pub fn run_main(&mut self) -> ReturnCode {
        if let Some(vm) = self.vm.as_mut() {
            self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
                //todo: encapsulate unsafety within SystemPlugin functionality
                let p = unsafe { plug.0.get().as_mut().unwrap_unchecked() };
                let _ = p.on_init(vm);
            });
            let res = vm.execute_main();
            self.sys_plugins.iter().for_each(|plug: &DynSystemPlugin| {
                //todo: encapsulate unsafety within SystemPlugin functionality
                let p = unsafe { plug.0.get().as_mut().unwrap_unchecked() };
                let _ = p.after_main(vm);
            });
            res
        } else {
            0
        }
    }
}
//todo: remove
pub mod ast_interpreter;
pub mod repl;
