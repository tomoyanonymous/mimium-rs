use std::{cell::RefCell, rc::Rc};

use mimium_lang::{
    function,
    interner::ToSymbol,
    log, numeric,
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
    window: Option<plot_window::PlotApp>,
}

impl GuiToolPlugin {
    /// This method is exposed as "make_probe(label:String)->(float)->float".
    pub fn make_probe(&mut self, vm: &mut Machine) -> ReturnCode {
        if let Some(app) = &mut self.window {
            log::warn!("make_probe called other than global context.");
            let idx = vm.get_stack(0);
            let probename = vm.prog.strings[idx as usize].as_str();

            let fnty = function!(vec![numeric!()], numeric!());
            let (mut prod, cons) = HeapRb::<f64>::new(512).split();
            app.add_plot(probename, cons);
            let cb = move |vm: &mut Machine| -> ReturnCode {
                let v = Machine::get_as::<f64>(vm.get_stack(0));
                let _ = prod.try_push(v);
                //do not modify any stack values
                1
            };
            let info: ExtClsInfo = ("probegetter".to_symbol(), Rc::new(RefCell::new(cb)), fnty);
            let cls = vm.wrap_extern_cls(info);
            vm.set_stack(0, Machine::to_value(cls));
        }
        1
    }
}

impl SystemPlugin for GuiToolPlugin {
    fn after_main(&mut self, _machine: &mut Machine) -> ReturnCode {
        let native_options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default()
                .with_inner_size([400.0, 300.0])
                .with_min_inner_size([300.0, 220.0]), // .with_icon(
                                                      //     // NOTE: Adding an icon is optional
                                                      //     eframe::icon_data::from_png_bytes(&include_bytes!("../assets/icon-256.png")[..])
                                                      //         .expect("Failed to load icon"),)
            ..Default::default()
        };
        let _ = eframe::run_native(
            "mimium guitools",
            native_options,
            Box::new(|_cc| Ok(Box::new(self.window.take().unwrap()))),
        );
        0
    }
    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let ty = function!(vec![string_t!()], unit!());
        let fptr: SystemPluginFnType<Self> = Self::make_probe;
        let make_probe = SysPluginSignature::new("make_probe", fptr, ty);
        vec![make_probe]
    }
}
