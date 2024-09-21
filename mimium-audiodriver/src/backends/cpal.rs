use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use crate::driver::{Driver, RuntimeData, SampleRate, Time};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{self, BufferSize, StreamConfig};
use mimium_lang::runtime::scheduler;
use mimium_lang::runtime::vm;
use ringbuf::traits::{Consumer, Producer, Split};
use ringbuf::{HeapCons, HeapProd, HeapRb};
const BUFFER_RATIO: usize = 2;
pub struct NativeDriver {
    sr: SampleRate,
    hardware_ichannels: usize,
    hardware_ochannels: usize,
    is_playing: bool,
    istream: Option<cpal::Stream>,
    ostream: Option<cpal::Stream>,
    count: Arc<AtomicU64>,
}
impl Default for NativeDriver {
    fn default() -> Self {
        Self {
            sr: SampleRate(48000),
            hardware_ichannels: 1,
            hardware_ochannels: 1,
            is_playing: false,
            istream: None,
            ostream: None,
            count: Default::default(),
        }
    }
}

//Runtime data, which will be created and immidiately send to audio thread.
struct NativeAudioData {
    pub vmdata: RuntimeData,
    dsp_ochannels: usize,
    buffer: HeapCons<f64>,
    localbuffer: Vec<f64>,
    count: Arc<AtomicU64>,
}
unsafe impl Send for NativeAudioData {}

impl NativeAudioData {
    pub fn new(program: vm::Program, buffer: HeapCons<f64>, count: Arc<AtomicU64>) -> Self {
        //todo: split as trait interface method
        let schedule_fn = scheduler::gen_schedule_at();
        let getnow_fn = crate::runtime_fn::gen_getnowfn(count.clone());
        let vmdata = RuntimeData::new(program, &[schedule_fn], &[getnow_fn]);
        let localbuffer: Vec<f64> = vec![0.0f64; 4096];
        let (_, dsp_func) = &vmdata.program.global_fn_table[vmdata.dsp_i];
        let dsp_ochannels = dsp_func.nret;
        Self {
            vmdata,
            dsp_ochannels,
            buffer,
            localbuffer,
            count,
        }
    }
    pub fn process(&mut self, dst: &mut [f32], h_ochannels: usize) {
        // let sample_size = dst.len() / h_ochannels;
        let local = &mut self.localbuffer.as_mut_slice()[..dst.len()];
        self.buffer.pop_slice(local);
        for (o, _s) in dst
            .chunks_mut(h_ochannels)
            .zip(local.chunks(self.dsp_ochannels))
        {
            let _rc = self
                .vmdata
                .run_dsp(Time(self.count.load(Ordering::Relaxed)));
            let res =
                vm::Machine::get_as_array::<f64>(self.vmdata.vm.get_top_n(self.dsp_ochannels));
            self.count.fetch_add(1, Ordering::Relaxed);
            match (h_ochannels, self.dsp_ochannels) {
                (i1, i2) if i1 == i2 => {
                    for i in 0..i1 {
                        o[i] = res[i] as f32;
                    }
                }
                (2, 1) => {
                    o[0] = res[0] as f32;
                    o[1] = res[0] as f32;
                }
                (1, 2) => {
                    o[0] = res[0] as f32;
                }
                (_, _) => {
                    todo!()
                }
            }
        }
    }
}
struct NativeAudioReceiver {
    dsp_ichannels: usize,
    localbuffer: Vec<f64>,
    buffer: HeapProd<f64>,
    count: u64,
}
unsafe impl Send for NativeAudioReceiver {}
impl NativeAudioReceiver {
    pub fn new(dsp_ichannels: usize, buffer: HeapProd<f64>) -> Self {
        Self {
            dsp_ichannels,
            localbuffer: vec![0f64; 4096],
            buffer,
            count: 0,
        }
    }
    pub fn receive_data(&mut self, data: &[f32], h_ichannels: usize) {
        for (ic, buf) in data
            .chunks(h_ichannels)
            .zip(self.localbuffer.chunks_mut(self.dsp_ichannels))
        {
            match (h_ichannels, self.dsp_ichannels) {
                (i1, i2) if i1 == i2 => {
                    for i in 0..i1 {
                        buf[i] = ic[i] as f64;
                    }
                }
                (2, 1) => {
                    buf[0] = ic[0] as f64; //copy lch
                }
                (1, 2) => {
                    buf[0] = ic[0] as f64;
                    buf[1] = ic[0] as f64;
                }
                (_, _) => {
                    todo!()
                }
            }
        }
        let local = &self.localbuffer.as_slice()[..data.len()];

        self.buffer.push_slice(local);
        self.count += (data.len() / h_ichannels) as u64;
    }
}
impl NativeDriver {
    fn init_iconfig(device: &cpal::Device, sample_rate: Option<SampleRate>) -> StreamConfig {
        let config_builder = device
            .supported_input_configs()
            .unwrap()
            .next()
            .expect("no supported config");
        if let Some(SampleRate(sr)) = sample_rate {
            config_builder
                .with_sample_rate(cpal::SampleRate(sr))
                .config()
        } else {
            device
                .default_input_config()
                .expect("no default input configs")
                .config()
        }
    }
    fn init_oconfig(device: &cpal::Device, sample_rate: Option<SampleRate>) -> StreamConfig {
        let config_builder = device
            .supported_output_configs()
            .unwrap()
            .next()
            .expect("no supported config");
        if let Some(SampleRate(sr)) = sample_rate {
            config_builder
                .with_sample_rate(cpal::SampleRate(sr))
                .config()
        } else {
            device
                .default_output_config()
                .expect("no default output configs")
                .config()
        }
    }
    fn set_streams(
        &mut self,
        istream: Option<cpal::Stream>,
        ostream: Option<cpal::Stream>,
        // iconfig: Option<cpal::StreamConfig>,
        // oconfig: Option<cpal::StreamConfig>,
    ) {
        self.istream = istream;
        self.ostream = ostream;
        // self.iconfig = iconfig;
        // self.oconfig = oconfig;
    }
}
impl Driver for NativeDriver {
    type Sample = f64;

    fn init(
        &mut self,
        program: vm::Program,
        sample_rate: Option<SampleRate>,
        buffer_size: usize,
    ) -> bool {
        let host = cpal::default_host();
        let (prod, cons) = HeapRb::<Self::Sample>::new(buffer_size).split();

        let idevice = host.default_input_device();
        let in_stream = if let Some(idevice) = idevice {
            let mut iconfig = Self::init_iconfig(&idevice, sample_rate);
            iconfig.buffer_size = BufferSize::Fixed((buffer_size / BUFFER_RATIO) as u32);
            log::info!(
                "input device: {} buffer size:{:?}",
                idevice.name().unwrap_or_default(),
                iconfig.buffer_size
            );
            let dsp_ichannels = 1; //todo
            let mut receiver = NativeAudioReceiver::new(dsp_ichannels, prod);
            self.hardware_ichannels = iconfig.channels as usize;
            let h_ichannels = self.hardware_ichannels;
            let in_stream = idevice.build_input_stream(
                &iconfig,
                move |data: &[f32], _s: &cpal::InputCallbackInfo| {
                    receiver.receive_data(data, h_ichannels)
                },
                |e| {
                    log::error!("{e}");
                },
                None,
            );
            in_stream.map_err(|e| log::error!("{e}")).ok()
        } else {
            None
        };
        let _ = in_stream.as_ref().map(|i| i.pause());
        let odevice = host.default_output_device();
        let out_stream = if let Some(odevice) = odevice {
            let mut processor = NativeAudioData::new(program, cons, self.count.clone());
            processor.vmdata.vm.execute_main(&processor.vmdata.program);
            let mut oconfig = Self::init_oconfig(&odevice, sample_rate);
            oconfig.buffer_size = cpal::BufferSize::Fixed((buffer_size / BUFFER_RATIO) as u32);
            log::info!(
                "output device {}buffer size:{:?}",
                odevice.name().unwrap_or_default(),
                oconfig.buffer_size
            );
            self.hardware_ochannels = oconfig.channels as usize;
            let h_ochannels = self.hardware_ochannels;
            let out_stream = odevice.build_output_stream(
                &oconfig,
                move |data: &mut [f32], _s: &cpal::OutputCallbackInfo| {
                    processor.process(data, h_ochannels)
                },
                |e| {
                    log::error!("{e}");
                },
                None,
            );
            out_stream.map_err(|e| log::error!("{e}")).ok()
        } else {
            None
        };
        log::info!(
            "in:{:?} out:{:?}",
            in_stream.is_some(),
            out_stream.is_some()
        );
        self.set_streams(in_stream, out_stream);
        true
    }

    fn play(&mut self) -> bool {
        let ires: bool = self
            .istream
            .as_mut()
            .and_then(|is| match is.play() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        let ores = self
            .ostream
            .as_mut()
            .and_then(|os| match os.play() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        self.is_playing = ires || ores;
        self.is_playing
    }

    fn pause(&mut self) -> bool {
        let _ires: bool = self
            .istream
            .as_mut()
            .and_then(|is| match is.pause() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        let _ores = self
            .ostream
            .as_mut()
            .and_then(|os| match os.pause() {
                Ok(_) => Some(()),
                Err(e) => {
                    log::error!("{e}");
                    None
                }
            })
            .is_some();
        self.is_playing = false;
        false
    }

    fn is_playing(&self) -> bool {
        self.is_playing
    }

    fn get_samplerate(&self) -> crate::driver::SampleRate {
        self.sr
    }

    fn get_current_sample(&self) -> crate::driver::Time {
        Time(self.count.load(Ordering::Relaxed))
    }
}
