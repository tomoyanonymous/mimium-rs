use atomic_float::AtomicF64;
use midir::{MidiInput, MidiInputPort};
use mimium_lang::{
    function,
    interner::ToSymbol,
    numeric,
    plugin::{SysPluginSignature, SystemPlugin},
    runtime::{vm, Time},
};
use std::{
    cell::{OnceCell, RefCell},
    rc::Rc,
    sync::Arc,
};

type NoteCallBack = Box<dyn Fn(f64, f64) -> ()>;

#[derive(Default)]
struct NoteCallBacks(pub [Vec<NoteCallBack>; 16]);

pub struct MidiPlugin {
    input: MidiInput,
    port: OnceCell<MidiInputPort>,
    port_name: Option<String>,
    note_callbacks: NoteCallBacks,
}

impl MidiPlugin {
    pub fn try_new() -> Option<Self> {
        let midiin = MidiInput::new("mimium midi plugin").ok();
        midiin.map(|input| Self {
            input,
            port: OnceCell::new(),
            port_name: None,
            note_callbacks: Default::default(),
        })
    }
    fn add_note_callback(&mut self, chan: u8, cb: NoteCallBack) {
        if chan < 15 {
            self.note_callbacks.0[chan as usize].push(cb);
        }
    }
    fn invoke_note_callback(&self, chan: u8, note: u8, vel: u8) {
        if chan < 15 {
            self.note_callbacks.0[chan as usize]
                .iter()
                .for_each(|cb| cb(note as f64, vel as f64));
        }
    }
    pub fn bind_midi_note_mono(&mut self, vm: &mut vm::Machine) -> vm::ReturnCode {
        let ch = vm::Machine::get_as::<f64>(vm.get_stack(0));
        let cell = Arc::new((AtomicF64::new(), AtomicF64::new()));
        self.add_note_callback(ch, |note, vel| {
            let (note_c, vel_c) = cell.clone();
            note_c.write(note);
            vel_c.write(vel);
        });
        let cls = |vm: &mut vm::Machine| -> vm::ReturnCode {
            let (note_c, vel_c) = cell.clone();
            vm.set_stack(0, vm::Machine::to_value(note_c));
            vm.set_stack(1, vm::Machine::to_value(vel_c));
            2
        };
        let ty = function!(vec![], numeric!());
        let rcls = vm.wrap_extern_cls(("get_midi_val".to_symbol(), Rc::new(RefCell::new(cls)), ty));
        vm.set_stack(0, vm::Machine::to_value(rcls));
        1
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
        let ty = function!(vec![numeric!()], function!(vec![], numeric!()));

        let bindnote =
            SysPluginSignature::new("bind_midi_note_mono", Self::bind_midi_note_mono, ty);
        vec![bindnote]
    }
}
