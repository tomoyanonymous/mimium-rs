use mimium_lang::{plugin::{SysPluginSignature, SystemPlugin}, runtime::{vm, Time}};
use midir::MidiInput;
pub struct MidiPlugin {

}

impl SystemPlugin for MidiPlugin{
    fn on_init(&mut self, machine: &mut vm::Machine) ->vm::ReturnCode {
        todo!()
    }

    fn on_sample(&mut self, time:Time, machine: &mut vm::Machine) ->vm::ReturnCode {
        todo!()
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        todo!()
    }
}