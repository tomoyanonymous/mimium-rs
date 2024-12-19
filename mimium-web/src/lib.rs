use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::Driver;
use mimium_lang::log;
use mimium_lang::runtime::Time;
use mimium_lang::ExecContext;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Config {
    sample_rate: f64,
    buffer_size: u32,
}

type Processer = Box<dyn FnMut(&[f64], &mut [f64]) -> u64>;
#[wasm_bindgen]
#[derive(Default)]
pub struct Context {
    processor: Option<Processer>,
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
    pub fn new() -> Self {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));

        Context {
            ..Default::default()
        }
    }
    #[wasm_bindgen]
    pub fn compile(&mut self, src: String, config: Config) {
        let mut ctx = get_default_context();
        if let Err(e) = ctx.prepare_machine(src.as_str()) {
            e.iter().for_each(|e| {
                eprintln!("{}", e);
            });
        }
        let mut driver = LocalBufferDriver::new(config.buffer_size as usize);
        ctx.add_plugin(driver.get_as_plugin());
        ctx.run_main();
        driver.init(
            ctx,
            Some(mimium_audiodriver::driver::SampleRate::from(
                config.sample_rate as u32,
            )),
        );
        let mut now = 0;
        self.processor = Some(Box::new(move |_input, output| {
            driver.vmdata.as_mut().unwrap().run_dsp(Time(now));
            now += 1;
            0
        }));
    }
    #[wasm_bindgen]
    pub fn process(&mut self,input : &[f64], output: &mut [f64]) {
        if let Some(ref mut processor) = self.processor {
            processor(input, output);
        }
    }
}
