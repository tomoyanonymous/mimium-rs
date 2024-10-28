use std::cell::OnceCell;

use midir::{MidiInput, MidiInputPort};
use mimium_lang::{
    plugin::{SysPluginSignature, SystemPlugin},
    runtime::{vm, Time},
};
pub struct MidiPlugin {
    input: MidiInput,
    port: OnceCell<MidiInputPort>,
    port_name: Option<String>,
}

impl MidiPlugin {
    pub fn try_new() -> Option<Self> {
        let midiin = MidiInput::new("mimium midi plugin").ok();
        midiin.map(|input| Self {
            input,
            port: OnceCell::new(),
            port_name: None,
        })
    }
}

impl SystemPlugin for MidiPlugin {
    fn after_main(&mut self, machine: &mut vm::Machine) -> vm::ReturnCode {
        let mut ports = self.input.ports();
        let port_opt = if let Some(pname) = &self.port_name {
            let mut matchedports = ports.iter_mut().filter(|port| {
                let name = self.input.port_name(port).unwrap_or_default();
                &name == pname
            });
            matchedports.next()
        } else {
            ports.iter_mut().next()
        };
        port_opt.map(|p| {
            let name = self.input.port_name(p).unwrap_or_default();
            log::debug!("Midi Input: Connected to {name}");
            // self.input.connect(
            //     p,
            //     &name,
            //     |stamp, message, machine: &mut vm::Machine| {
            //         todo!();
            //     },
            //     machine,
            // );
            self.port.set(p.clone());
        });

        0
    }

    fn on_sample(&mut self, time: Time, machine: &mut vm::Machine) -> vm::ReturnCode {
        0
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        todo!()
    }
}
