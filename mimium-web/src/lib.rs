use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::Driver;
use mimium_lang::log;
use mimium_lang::ExecContext;
use wasm_bindgen::prelude::*;
#[wasm_bindgen]
#[derive(Default)]
pub struct Config {
    pub sample_rate: f64,
    pub input_channels: u32,
    pub output_channels: u32,
    pub buffer_size: u32,
}
#[wasm_bindgen]
impl Config {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self::default()
    }
}

type Output<'a> = &'a mut [f32];
type Input<'a> = &'a [f32];

type Processer = Box<dyn FnMut(Input, Output) -> u64>;
#[wasm_bindgen]
#[derive(Default)]
pub struct Context {
    processor: Option<Processer>,
    config: Config,
}

fn get_default_context() -> ExecContext {
    let mut ctx = ExecContext::new([].into_iter(), None);
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    if let Some(midi_plug) = mimium_midi::MidiPlugin::try_new() {
        ctx.add_system_plugin(midi_plug);
    } else {
        log::warn!("Midi is not supported on this platform.")
    }
    ctx
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen(constructor)]
    pub fn new(config: Config) -> Self {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        Context {
            config,
            ..Default::default()
        }
    }
    #[wasm_bindgen]
    pub fn compile(&mut self, src: String) {
        let mut ctx = get_default_context();

        if let Err(e) = ctx.prepare_machine(src.as_str()) {
            e.iter().for_each(|e| {
                eprintln!("{}", e);
            });
        }
        let mut driver = LocalBufferDriver::new(self.config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());
        ctx.run_main();
        driver.init(
            ctx,
            Some(mimium_audiodriver::driver::SampleRate::from(
                self.config.sample_rate as u32,
            )),
        );
        let out_ch = self.config.output_channels;
        let mut out_buf = vec![0.0; (out_ch * self.config.buffer_size) as usize];
        self.processor = Some(Box::new(move |_input, output: Output| -> u64 {
            driver.play();
            driver
                .get_generated_samples()
                .iter()
                .map(|f| *f as f32)
                .enumerate()
                .for_each(|(i, f)| {
                    out_buf[i] = f;
                });
            output.copy_from_slice(&out_buf);
            0
        }));
    }
    /// .
    ///
    /// # Safety
    /// Array size of input and output must be equal to `input_channels * buffer_size` and `output_channels * buffer_size` respectively.
    /// .
    #[wasm_bindgen]
    pub fn process(&mut self, input: &[f32], output: &mut [f32]) -> u64 {
        unsafe { self.processor.as_mut().unwrap_unchecked()(input, output) }
    }
}
