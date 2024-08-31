use mimium_lang::runtime::vm;
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

#[derive(Clone, Copy)]
pub struct Time(pub u64);

pub trait Driver {
    type Sample: Float;
    fn init(
        &mut self,
        program: vm::Program,
        sample_rate: Option<SampleRate>,
        buffer_size: usize,
    ) -> bool;
    fn play(&mut self) -> bool;
    fn pause(&mut self) -> bool;
    fn get_samplerate(&self) -> SampleRate;
    fn get_current_sample(&self) -> Time;
    fn is_playing(&self) -> bool;
}

pub struct RuntimeData {
    pub program: vm::Program,
    pub vm: vm::Machine,
}
impl RuntimeData {
    pub fn new(program: vm::Program) -> Self {
        let mut vm = vm::Machine::new();
        vm.link_functions(&program);
        Self { program, vm }
    }
}

pub fn load_default_runtime() -> Box<dyn Driver<Sample = f64>> {
    Box::new(crate::backends::cpal::NativeDriver::default())
}
