use std::sync::Arc;

use crate::{
    interner::{Symbol, ToSymbol, TypeNodeId},
    runtime::vm::{ExtClsInfo, ExtFnInfo, Machine, ReturnCode},
};

pub trait Plugin {
    fn get_ext_functions(&self) -> &[ExtFnInfo];
    fn get_ext_closures(&self) -> &[ExtClsInfo];
    fn on_init(&mut self, vm: &mut Machine) -> ReturnCode {
        0
    }
    fn on_sample(&mut self, vm: &mut Machine) -> ReturnCode {
        0
    }
}
pub fn get_extfun_types(
    plugins: &[Arc<dyn Plugin>],
) -> impl IntoIterator<Item = (Symbol, TypeNodeId)> + '_ {
    plugins.iter().flat_map(|plugin| {
        plugin
            .get_ext_functions()
            .iter()
            .map(|(name, _, ty)| (*name, *ty))
            .chain(
                plugin
                    .get_ext_closures()
                    .iter()
                    .map(|(name, _, ty)| (*name, *ty)),
            )
    })
}

pub fn get_extfuninfos(plugins: &[Arc<dyn Plugin>]) -> impl Iterator<Item = ExtFnInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_functions().iter())
        .cloned()
}
pub fn get_extclsinfos(plugins: &[Arc<dyn Plugin>]) -> impl Iterator<Item = ExtClsInfo> + '_ {
    plugins
        .iter()
        .flat_map(|plugin| plugin.get_ext_closures().iter())
        .cloned()
}
