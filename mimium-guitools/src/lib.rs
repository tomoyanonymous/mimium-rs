use mimium_lang::{
    function,
    interner::ToSymbol,
    numeric,
    plugin::{SysPluginSignature, SystemPlugin},
    runtime::vm,
    string_t, tuple,
    types::{PType, Type},
    unit,
};

pub struct GuiToolPlugin{

}

impl SystemPlugin for GuiToolPlugin{
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        todo!()
    }
}