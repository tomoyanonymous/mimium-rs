use crate::{
    interner::{ToSymbol, TypeNodeId},
    runtime::{
        vm::{ExtClsInfo, Machine, ReturnCode},
        Time,
    },
};
use std::{any::Any, cell::RefCell, rc::Rc};

pub struct SysPluginSignature {
    name: &'static str,
    /// The function internally implements Fn(&mut T:SystemPlugin,&mut Machine)->ReturnCode
    /// but the type is erased for dynamic dispatching. later the function is downcasted into their own type.
    fun: Rc<dyn Any>,
    ty: TypeNodeId,
}
impl SysPluginSignature {
    pub fn new<F, T>(name: &'static str, fun: F, ty: TypeNodeId) -> Self
    where
        F: Fn(&mut T, &mut Machine) -> ReturnCode + 'static,
        T: SystemPlugin,
    {
        Self {
            name,
            fun: Rc::new(fun),
            ty,
        }
    }
}

pub trait SystemPlugin {
    fn on_init(&mut self, machine: &mut Machine) -> ReturnCode;
    fn on_sample(&mut self, time: Time, machine: &mut Machine) -> ReturnCode;
    fn gen_interfaces(&self) -> Vec<SysPluginSignature>;
    // fn get_plugin_instance_dyn(&self, id: usize, machine: &mut Machine) -> Rc<RefCell<Self>> {
    //     todo!()
    // }
    // fn get_extclsinfos(&mut self, id: usize, machine: &mut Machine) -> Vec<ExtClsInfo> {
    //     let plug = machine.get_plugin_instance_mut(id).clone();
    //     self.gen_interfaces()
    //         .iter()
    //         .map(|SysPluginSignature { name, fun, ty }| {
    //             let fun: ExtClsType = Rc::new(RefCell::new(|machine: &mut Machine| {
    //                 fun(&mut plug.borrow_mut(), machine)
    //             }));
    //             (name.to_symbol(), fun, *ty)
    //         })
    //         .collect::<Vec<_>>()
    // }
}

pub fn to_ext_cls_info<T: SystemPlugin + 'static>(dyn_plugin: Rc<RefCell<T>>) -> Vec<ExtClsInfo> {
    let ifs = dyn_plugin.borrow().gen_interfaces();
    ifs.into_iter()
        .map(|SysPluginSignature { name, fun, ty }| -> ExtClsInfo {
            let plug = dyn_plugin.clone();
            println!("fntypeid: {:?}", fun.type_id());
            let fun = fun
                .clone()
                .downcast::<fn(&mut T, &mut Machine) -> ReturnCode>()
                .expect("invalid conversion applied in the system plugin resolution.");
            let fun = Rc::new(RefCell::new(move |machine: &mut Machine| -> ReturnCode {
                fun(&mut plug.borrow_mut(), machine)
            }));
            let res: ExtClsInfo = (name.to_symbol(), fun, ty);
            res
        })
        .collect::<Vec<_>>()
}

pub struct SysPluginDyn(pub Rc<RefCell<dyn SystemPlugin>>);




// impl<T: Sized> SysPluginSignature<T> {}
