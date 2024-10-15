use std::borrow::BorrowMut;

use mimium_lang::{
    interner::{Symbol, ToSymbol},
    plugin::{SysPluginDyn, SystemPlugin},
    runtime::{
        vm::{self, ExtClsType, ExtFunType, FuncProto, ReturnCode},
        Time,
    },
    utils::error::ReportableError,
    ExecContext,
};
use num_traits::Float;

#[derive(Clone)]
pub struct PlaybackInfo {
    pub sample_rate: u32,
    pub current_time: usize,
    pub frame_per_buffer: u64,
    pub channels: u64,
}

impl PlaybackInfo {
    pub fn get_current_realtime(&self) -> f32 {
        self.current_time as f32 / self.sample_rate as f32
    }
    pub fn rewind(&mut self) {
        self.current_time = 0;
    }
}

pub trait Component {
    type Sample: Float;
    fn get_input_channels(&self) -> u64;
    fn get_output_channels(&self) -> u64;
    fn prepare_play(&mut self, info: &PlaybackInfo);
    fn render(&mut self, input: &[Self::Sample], output: &mut [Self::Sample], info: &PlaybackInfo);
}

#[derive(Clone, Copy)]
pub struct SampleRate(pub u32);
#[derive(Debug)]
pub enum Error {
    Unknown,
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Unknown => write!(f, "unknown runtime error"),
        }
    }
}
impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        0..0 //todo!
    }
}

// Note: `Driver` trait doesn't have `new()` so that the struct can have its own
// `new()` with any parameters specific to the type. With this in mind, `init()`
// can accept only common parameters.
pub trait Driver {
    type Sample: Float;
    fn init(&mut self, ctx: ExecContext, sample_rate: Option<SampleRate>) -> bool;
    fn play(&mut self) -> bool;
    fn pause(&mut self) -> bool;
    fn get_samplerate(&self) -> SampleRate;
    fn get_current_sample(&self) -> Time;
    fn is_playing(&self) -> bool;
}

pub struct RuntimeData {
    pub vm: vm::Machine,
    pub sys_plugins: Vec<SysPluginDyn>,
    pub dsp_i: usize,
}
impl RuntimeData {
    /// VM in RuntimeData additionally uses audio-driver specific external closure like `_mimium_getnow` so this constructor accepts it.
    /// This initilization process maybe changed in the future due to more runtime-specific value will be introduced.
    pub fn new(
        mut vm: vm::Machine,
        sys_plugins: Vec<SysPluginDyn>,
        ext_clss: &[(Symbol, ExtClsType)],
    ) -> Self {
        ext_clss.iter().for_each(|(name, f)| {
            vm.install_extern_cls(*name, f.clone());
        });
        //todo:error handling
        let dsp_i = vm.prog.get_fun_index(&"dsp".to_symbol()).unwrap_or(0);
        Self {
            vm,
            sys_plugins,
            dsp_i,
        }
    }
    pub fn run_main(&mut self) -> ReturnCode {
        self.sys_plugins.iter().for_each(|plug: &SysPluginDyn| {
            let mut p = (&*plug.0).borrow_mut();
            let _ = p.on_init(&mut self.vm);
        });
        self.vm.execute_main()
    }
    pub fn get_dsp_fn(&self) -> &FuncProto {
        &self.vm.prog.global_fn_table[self.dsp_i].1
    }
    pub fn run_dsp(&mut self, time: Time) -> ReturnCode {
        self.sys_plugins.iter().for_each(|plug: &SysPluginDyn| {
            let mut p = (&*plug.0).borrow_mut();
            let _ = p.on_sample(time, &mut self.vm);
        });
        self.vm.execute_idx(self.dsp_i)
    }
}




pub fn load_default_runtime() -> Box<dyn Driver<Sample = f64>> {
    crate::backends::cpal::native_driver(4096)
}
