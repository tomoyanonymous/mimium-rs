use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

use mimium_lang::{interner::ToSymbol, runtime::scheduler, runtime::vm::Machine};

use crate::driver::{Driver, RuntimeData, SampleRate, Time};
pub struct MockDriver {
    pub vmdata: Option<RuntimeData>,
    count: Arc<AtomicU64>,
    samplerate: SampleRate,
    localbuffer: Vec<f64>,
    _ichannels: u64,
    ochannels: u64,
}

impl Default for MockDriver {
    fn default() -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate(48000),
            localbuffer: vec![],
            _ichannels: 0,
            ochannels: 0,
        }
    }
}

impl MockDriver {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn play_times(&mut self, times: usize) -> &[<MockDriver as Driver>::Sample] {
        let _ = self
            .vmdata
            .as_mut()
            .expect("Not initialized yet?")
            .run_main();
        self.localbuffer.clear();
        for _ in 0..times {
            let now = self.count.load(Ordering::Relaxed);

            let _ = self
                .vmdata
                .as_mut()
                .expect("Not initialized yet?")
                .run_dsp(Time(now));
            let res = Machine::get_as_array::<<MockDriver as Driver>::Sample>(
                self.vmdata
                    .as_mut()
                    .expect("Not initialized yet?")
                    .vm
                    .get_top_n(self.ochannels as _),
            );
            self.localbuffer.extend_from_slice(res);
            //update current time.
            self.count.store(now + 1, Ordering::Relaxed);
        }
        &self.localbuffer
    }

    pub fn get_ochannels(&self) -> usize {
        self.ochannels as _
    }
}

impl Driver for MockDriver {
    type Sample = f64;

    fn init(
        &mut self,
        program: mimium_lang::runtime::vm::Program,
        sample_rate: Option<crate::driver::SampleRate>,
    ) -> bool {
        let dsp_i = program
            .get_fun_index(&"dsp".to_symbol())
            .expect("no dsp function found");
        let (_, dsp_func) = &program.global_fn_table[dsp_i];
        self.ochannels = dsp_func.nret as u64;
        self.samplerate = sample_rate.unwrap_or(SampleRate(48000));

        //todo: split as trait interface method
        let schedule_fn = scheduler::gen_schedule_at();
        let getnow_fn = crate::runtime_fn::gen_getnowfn(self.count.clone());

        self.vmdata = Some(RuntimeData::new(program, &[schedule_fn], &[getnow_fn]));

        true
    }

    fn play(&mut self) -> bool {
        debug_assert!(
            false,
            "this is driver for test purpose. to run program, use play_times method"
        );
        false
    }

    fn pause(&mut self) -> bool {
        false
    }

    fn get_samplerate(&self) -> SampleRate {
        self.samplerate
    }

    fn get_current_sample(&self) -> Time {
        Time(self.count.load(Ordering::Relaxed))
    }

    fn is_playing(&self) -> bool {
        false
    }
}

pub fn mock_driver() -> Box<dyn Driver<Sample = f64>> {
    Box::new(MockDriver::new())
}
