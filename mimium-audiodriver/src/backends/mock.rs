use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

use mimium_lang::{interner::ToSymbol, runtime::scheduler, runtime::vm::Machine};

use crate::driver::{Driver, RuntimeData, SampleRate, Time};
pub struct MockDriver {
    pub vmdata: RuntimeData,
    count: Arc<AtomicU64>,
    samplerate: SampleRate,
    localbuffer: Vec<f64>,
    _ichannels: u64,
    ochannels: u64,
}

impl MockDriver {
    pub fn new(
        program: mimium_lang::runtime::vm::Program,
        sample_rate: Option<SampleRate>,
    ) -> Self {
        let dsp_i = program
            .get_fun_index(&"dsp".to_symbol())
            .expect("no dsp function found");
        let (_, dsp_func) = &program.global_fn_table[dsp_i];
        let ochannels = dsp_func.nret as u64;
        let count = Arc::new(AtomicU64::new(0));
        //todo: split as trait interface method
        let schedule_fn = scheduler::gen_schedule_at();
        let getnow_fn = crate::runtime_fn::gen_getnowfn(count.clone());

        let vmdata = RuntimeData::new(program, &[schedule_fn], &[getnow_fn]);
        let localbuffer: Vec<f64> = vec![];
        let samplerate = sample_rate.unwrap_or(SampleRate(48000));
        Self {
            vmdata,
            count,
            samplerate,
            localbuffer,
            _ichannels: 0,
            ochannels,
        }
    }
    pub fn play_times(&mut self, times: usize) -> &[<MockDriver as Driver>::Sample] {
        let _ = self.vmdata.run_main();
        self.localbuffer.clear();
        for _ in 0..times {
            let now = self.count.load(Ordering::Relaxed);

            let _ = self.vmdata.run_dsp(Time(now));
            let res = Machine::get_as_array::<<MockDriver as Driver>::Sample>(
                self.vmdata.vm.get_top_n(self.ochannels as _),
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
        buffer_size: usize,
    ) -> bool {
        let _ = self.vmdata.run_main();
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
