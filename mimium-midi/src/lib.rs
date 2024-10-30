use atomic_float::AtomicF64;
use midir::{MidiInput, MidiInputPort};
use mimium_lang::{
    function,
    interner::ToSymbol,
    numeric,
    plugin::{SysPluginSignature, SystemPlugin},
    runtime::{vm, Time},
    types::{PType, Type},
};
use std::{
    cell::{OnceCell, RefCell},
    rc::Rc,
    sync::{atomic::Ordering, Arc},
};
use wmidi::MidiMessage;

type NoteCallBack = Arc<dyn Fn(f64, f64) + Send + Sync>;

#[derive(Default)]
struct NoteCallBacks(pub [Vec<NoteCallBack>; 16]);
impl NoteCallBacks {
    pub fn invoke_note_callback(&self, chan: u8, note: u8, vel: u8) {
        if chan < 15 {
            self.0[chan as usize]
                .iter()
                .for_each(|cb| cb(note as f64, vel as f64));
        };
    }
}
pub struct MidiPlugin {
    input: Option<MidiInput>,
    port: OnceCell<MidiInputPort>,
    port_name: Option<String>,
    note_callbacks: Option<NoteCallBacks>,
}

impl MidiPlugin {
    pub fn try_new() -> Option<Self> {
        let midiin = MidiInput::new("mimium midi plugin").ok();
        midiin.map(|input| Self {
            input: Some(input),
            port: OnceCell::new(),
            port_name: None,
            note_callbacks: Some(Default::default()),
        })
    }
    fn add_note_callback(&mut self, chan: u8, cb: NoteCallBack) {
        if chan < 15 {
            let _ = self.note_callbacks.as_mut().map(|v| {
                v.0[chan as usize].push(cb);
            });
        }
    }

    pub fn bind_midi_note_mono(&mut self, vm: &mut vm::Machine) -> vm::ReturnCode {
        let ch = vm::Machine::get_as::<f64>(vm.get_stack(0));
        let cell = Arc::new((AtomicF64::new(0.0), AtomicF64::new(0.0)));
        let cell_c = cell.clone();
        self.add_note_callback(
            ch as u8,
            Arc::new(move |note, vel| {
                cell_c.0.store(note, Ordering::Relaxed);
                cell_c.1.store(vel, Ordering::Relaxed);
            }),
        );
        let cls = move |vm: &mut vm::Machine| -> vm::ReturnCode {
            let note = cell.0.load(Ordering::Relaxed);
            let vel = cell.1.load(Ordering::Relaxed);
            vm.set_stack(0, vm::Machine::to_value(note));
            vm.set_stack(1, vm::Machine::to_value(vel));
            2
        };
        let ty = function!(vec![], numeric!());
        let rcls = vm.wrap_extern_cls(("get_midi_val".to_symbol(), Rc::new(RefCell::new(cls)), ty));
        vm.set_stack(0, vm::Machine::to_value(rcls));
        1
    }
}

impl SystemPlugin for MidiPlugin {
    fn after_main(&mut self, _machine: &mut vm::Machine) -> vm::ReturnCode {
        let mut ports = self.input.as_ref().unwrap().ports();
        let port_opt = if let Some(pname) = &self.port_name {
            let mut matchedports = ports.iter_mut().filter(|port| {
                let name = self
                    .input
                    .as_ref()
                    .unwrap()
                    .port_name(port)
                    .unwrap_or_default();
                &name == pname
            });
            matchedports.next()
        } else {
            ports.iter_mut().next()
        };
        let _ = port_opt.map(|p| {
            let name = self
                .input
                .as_ref()
                .unwrap()
                .port_name(p)
                .unwrap_or_default();
            log::debug!("Midi Input: Connected to {name}");
            let res = self.input.take().unwrap().connect(
                p,
                &name,
                |_stamp, message, cbs: &mut NoteCallBacks| {
                    let msg = MidiMessage::from_bytes(message);
                    if let Ok(m) = msg {
                        match m {
                            MidiMessage::NoteOff(channel, note, _vel) => {
                                cbs.invoke_note_callback(channel.index(), u8::from(note), 0);
                            }
                            MidiMessage::NoteOn(channel, note, vel) => {
                                cbs.invoke_note_callback(
                                    channel.index(),
                                    u8::from(note),
                                    vel.into(),
                                );
                            }
                            _ => {}
                        }
                    }
                },
                self.note_callbacks.take().unwrap(),
            );
            match res {
                Ok(_c) => {
                    //todo:close handling
                }
                Err(e) => {
                    println!("{}", e)
                }
            }
            let _ = self.port.set(p.clone());
        });

        0
    }

    fn on_sample(&mut self, _time: Time, _machine: &mut vm::Machine) -> vm::ReturnCode {
        0
    }

    fn gen_interfaces(&self) -> Vec<SysPluginSignature> {
        let ty = function!(vec![numeric!()], function!(vec![], numeric!()));

        let bindnote =
            SysPluginSignature::new("bind_midi_note_mono", Self::bind_midi_note_mono, ty);
        vec![bindnote]
    }
}
