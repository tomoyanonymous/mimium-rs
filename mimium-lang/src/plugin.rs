//! # Plugin System for mimium
//! In order to extend mimium's capability to communicate between host system, mimium has its own FFI system.
//! The FFI is done through some traits in this plugin module in order to decouple dependencies(modules may depends on external crates).
//! There are 3 types of interfaces you need to define depending on what you need.
//!
//! 1. **IO Plugins** Sets of instance-free external functions such as `print` and `println`. They are mostly for glue functions for host system's IO. `mimium-core` is an example of this type of module.
//! 2. **External Unit Generator(UGen) Plugin.** If you need to define native Unit Generator, use `UGenPlugin` interface. In mimium code, you need to call higher-order function that returns instance of the UGen. You need to write small wrapper for this which simply calls object's constructor (In the future, this wrapper will be automatically implemented through proc-macro). Multiple instances may exist at the same time. `mimium-symphonia` is a example of this type of module.
//! 3. **System Plugin**. If your plugin needs to mutate states of system-wide instance (1 plugin instance per 1 vm), you need to implement `SystemPlugin` traits. System plugin can have callbacks invoked at the important timings of the system like `on_init`, `before_on_sample` & so on. Internal synchronous event scheduler is implemented through this plugins system. `mimium-rand` is also an example of this type of module.

mod system_plugin;
pub use system_plugin::{to_ext_cls_info, SysPluginDyn, SysPluginSignature, SystemPlugin};

use crate::{
    compiler::ExtFunTypeInfo,
    interner::{Symbol, TypeNodeId},
    runtime::vm::{ExtClsInfo, ExtFnInfo},
};

pub trait Plugin {
    fn get_ext_functions(&self) -> Vec<ExtFnInfo>;
    fn get_ext_closures(&self) -> Vec<ExtClsInfo>;
}

pub struct InstantPlugin {
    pub extfns: Vec<ExtFnInfo>,
    pub extcls: Vec<ExtClsInfo>,
}
impl Plugin for InstantPlugin {
    fn get_ext_functions(&self) -> Vec<ExtFnInfo> {
        self.extfns.clone()
    }

    fn get_ext_closures(&self) -> Vec<ExtClsInfo> {
        self.extcls.clone()
    }
}

pub trait IOPlugin {
    fn get_ext_functions(&self) -> Vec<ExtFnInfo>;
}

impl<T> Plugin for T
where
    T: IOPlugin,
{
    fn get_ext_functions(&self) -> Vec<ExtFnInfo> {
        <T as IOPlugin>::get_ext_functions(&self)
    }
    fn get_ext_closures(&self) -> Vec<ExtClsInfo> {
        vec![]
    }
}

/// Todo: Make wrapper macro for auto impl `Plugin`
pub trait UGenPlugin {
    type InitParam;
    type Args;
    type Ret;
    fn new(param: Self::InitParam) -> Self;
    fn on_sample(&mut self, arg: Self::Args) -> Self::Ret;
}
// type DynUgenPlugin{}
// pub type UGenPluginCollection(Vec<DynUGenPlugin>);
// impl Plugin for UGenPluginCollection{}

pub fn get_extfun_types(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtFunTypeInfo> + '_ {
    plugins.iter().flat_map(|plugin| {
        plugin
            .get_ext_functions()
            .into_iter()
            .map(|(name, _, ty)| ExtFunTypeInfo { name, ty })
            .chain(
                plugin
                    .get_ext_closures()
                    .into_iter()
                    .map(|(name, _, ty)| ExtFunTypeInfo { name, ty }),
            )
            .collect::<Vec<_>>()
    })
}

pub fn get_extfuninfos(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtFnInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_functions().into_iter())
}
pub fn get_extclsinfos(plugins: &[Box<dyn Plugin>]) -> impl Iterator<Item = ExtClsInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_closures().into_iter())
}
