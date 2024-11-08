use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

use mimium_lang::{
    interner::ToSymbol,
    runtime::{vm, Time},
    ExecContext,
};

use crate::driver::{Driver, RuntimeData, SampleRate};

/// Execute the program n times and write the result values to `localbuffer`.
pub struct LocalBufferDriver {
    pub vmdata: Option<RuntimeData>,
    pub count: Arc<AtomicU64>,
    samplerate: SampleRate,
    localbuffer: Vec<f64>,
    times: usize,
    _ichannels: u64,
    ochannels: u64,
}

impl Default for LocalBufferDriver {
    fn default() -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate(48000),
            localbuffer: vec![],
            times: 0,
            _ichannels: 0,
            ochannels: 0,
        }
    }
}

impl LocalBufferDriver {
    pub fn new(times: usize) -> Self {
        let count = Arc::new(AtomicU64::new(0));

        Self {
            vmdata: None,
            count,
            samplerate: SampleRate(48000),
            localbuffer: vec![],
            times,
            _ichannels: 0,
            ochannels: 0,
        }
    }

    pub fn get_ochannels(&self) -> usize {
        self.ochannels as _
    }

    pub fn get_generated_samples(&self) -> &[<LocalBufferDriver as Driver>::Sample] {
        &self.localbuffer
    }
}

impl Driver for LocalBufferDriver {
    type Sample = f64;

    fn get_runtimefn_infos(&self) -> Vec<vm::ExtClsInfo> {
        let getnow = crate::runtime_fn::gen_getnowfn(self.count.clone());
        vec![getnow]
    }

    fn init(&mut self, ctx: ExecContext, sample_rate: Option<crate::driver::SampleRate>) -> bool {
        let vm = ctx.vm.expect("vm is not prepared yet");
        let dsp_i = vm
            .prog
            .get_fun_index(&"dsp".to_symbol())
            .expect("no dsp function found");
        let (_, dsp_func) = &vm.prog.global_fn_table[dsp_i];
        self.ochannels = dsp_func.nret as u64;
        self.localbuffer = Vec::with_capacity(dsp_func.nret * self.times);
        self.samplerate = sample_rate.unwrap_or(SampleRate(48000));

        self.vmdata = Some(RuntimeData::new(vm, ctx.sys_plugins));

        true
    }

    fn play(&mut self) -> bool {
        let vmdata = self.vmdata.as_mut().expect("Not initialized yet?");
        self.localbuffer.clear();
        for _ in 0..self.times {
            let now = self.count.load(Ordering::Relaxed);

            let _ = vmdata.run_dsp(Time(now));
            let res = vm::Machine::get_as_array::<<LocalBufferDriver as Driver>::Sample>(
                vmdata.vm.get_top_n(self.ochannels as _),
            );
            self.localbuffer.extend_from_slice(res);
            //update current time.
            self.count.store(now + 1, Ordering::Relaxed);
        }
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

pub fn local_buffer_driver(times: usize) -> Box<dyn Driver<Sample = f64>> {
    Box::new(LocalBufferDriver::new(times))
}
