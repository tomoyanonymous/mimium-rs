use std::{cell::RefCell, rc::Rc};

use mimium_lang::{
    function,
    interner::ToSymbol,
    numeric,
    plugin::{SysPluginSignature, SystemPlugin, SystemPluginFnType},
    runtime::vm::{ExtClsInfo, Machine, ReturnCode},
    string_t,
    types::{PType, Type},
    unit,
};
use ringbuf::{
    traits::{Producer, Split},
    HeapRb,
};
pub(crate) mod plot_ui;
pub mod plot_window;
pub struct GuiToolPlugin {
    window: plot_window::PlotApp,
}

impl GuiToolPlugin {
    /// This method is exposed as "make_probe(label:String)->(float)->float".
    pub fn make_probe(&mut self, vm: &mut Machine) -> ReturnCode {
        let idx = vm.get_stack(0);
        let probename = vm.prog.strings[idx as usize].as_str();

        let fnty = function!(vec![numeric!()], numeric!());
        let (mut prod, cons) = HeapRb::<f64>::new(512).split();
        self.window.add_plot(probename, cons);
        let cb = move |vm: &mut Machine| -> ReturnCode {
            let v = Machine::get_as::<f64>(vm.get_stack(0));
            let _ = prod.try_push(v);
            //do not modify any stack values
            1
        };
        let info: ExtClsInfo = ("probegetter".to_symbol(), Rc::new(RefCell::new(cb)), fnty);
        let cls = vm.wrap_extern_cls(info);
        vm.set_stack(0, Machine::to_value(cls));
        1
    }
}

impl SystemPlugin for GuiToolPlugin {
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let ty = function!(vec![string_t!()], unit!());
        let fptr: SystemPluginFnType<Self> = Self::make_probe;
        let make_probe = SysPluginSignature::new("make_probe", fptr, ty);
        vec![make_probe]
    }
}
