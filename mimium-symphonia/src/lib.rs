use core::num;
use std::fs::File;
use std::sync::Arc;

use mimium_lang::interner::{Symbol, ToSymbol, TypeNodeId};
use mimium_lang::runtime::vm::{self, ExtClsType, ExtFnInfo, ExtFunType, Machine, ReturnCode};
use mimium_lang::types::{PType, Type};
use mimium_lang::{function, numeric, string_t};
use symphonia::core::audio::{AudioBuffer, Layout, SampleBuffer, SignalSpec};
use symphonia::core::codecs::{CodecParameters, Decoder, DecoderOptions, CODEC_TYPE_NULL};
use symphonia::core::errors::Error;
use symphonia::core::formats::{FormatOptions, FormatReader, SeekMode, SeekTo};
use symphonia::core::io::{MediaSource, MediaSourceStream, MediaSourceStreamOptions};
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::{Hint, ProbeResult};
use symphonia::core::units::{Duration, Time};
type DecoderSet = (Box<dyn Decoder>, ProbeResult, u32);
mod filemanager;
use filemanager::FileManager;

fn get_default_decoder(path: &str) -> Result<DecoderSet, Box<dyn std::error::Error>> {
    let flmgr = filemanager::get_global_file_manager();
    let src = flmgr.open_file_stream(path).expect("failed to open file");
    let ms: Box<dyn MediaSource> = Box::new(src);
    let mss_opts = MediaSourceStreamOptions::default();
    let mss = MediaSourceStream::new(ms, mss_opts);
    let hint = Hint::new();
    //  hint.with_extension("mp3");
    // Use the default options for metadata and format readers.
    let meta_opts: MetadataOptions = Default::default();
    let fmt_opts: FormatOptions = Default::default();

    // Probe the media source.
    let probed = symphonia::default::get_probe().format(&hint, mss, &fmt_opts, &meta_opts)?;

    let format = probed.format.as_ref();
    let track = format
        .tracks()
        .iter()
        .find(|t| t.codec_params.codec != CODEC_TYPE_NULL)
        .ok_or(Error::Unsupported("no supported audio tracks"))?;
    let id = track.id;
    // Use the default options for the decoder.
    let dec_opts: DecoderOptions = Default::default();
    // Create a decoder for the track.
    let decoder = symphonia::default::get_codecs().make(&track.codec_params, &dec_opts)?;

    Ok((decoder, probed, id))
}

fn load_wavfile_to_vec(path: &str) -> Vec<f64> {
    let (mut decoder, mut probed, id) = get_default_decoder(path).expect("failed to find file");
    let max_frames = decoder.codec_params().max_frames_per_packet.unwrap();
    let _ = probed.format.seek(
        SeekMode::Accurate,
        SeekTo::Time {
            time: Time::from_ss(0, 0).unwrap(),
            track_id: Some(id),
        },
    );
    let CodecParameters {
        channels,
        sample_rate,
        channel_layout,
        ..
    } = probed.format.default_track().unwrap().codec_params;
    let mut res = Vec::<f64>::new();
    let mut buf = SampleBuffer::<f64>::new(
        max_frames,
        SignalSpec::new_with_layout(
            sample_rate.unwrap_or(48000),
            channel_layout.unwrap_or(Layout::Mono),
        ),
    );
    match probed.format.next_packet() {
        Ok(packet) => {
            // Decode the packet into audio samples.
            let _ = decoder.decode(&packet).map(|decoded| {
                buf.copy_interleaved_ref(decoded.clone());
                // log::debug!(
                //     "frames:{}, timestamp:{}, n_samples: {}",
                //     decoded.frames(),
                //     packet.ts(),
                //     _nsamples
                // );
                res.extend_from_slice(buf.samples());
            });
        }
        Err(e) => {
            //
        }
    }
    res
}

fn load_wavfile(machine: &mut Machine) -> ReturnCode {
    //return closure

    let relpath = machine.prog.strings[vm::Machine::get_as::<usize>(machine.get_stack(0))];
    let relpath2 = std::path::Path::new(relpath.as_str());
    let filepath = machine
        .prog
        .file_path
        .map_or_else(|| "".to_string(), |s| s.to_string());
    let mmm_dirpath = std::path::Path::new(filepath.as_str()).parent().unwrap();
    let abspath = mmm_dirpath.join(relpath2);
    log::debug!("file path: {}", abspath.to_string_lossy());
    let vec = load_wavfile_to_vec(&abspath.to_string_lossy()); //the generated vector is moved into the closure
    let res = move |machine: &mut Machine| -> ReturnCode {
        let pos = vm::Machine::get_as::<f64>(machine.get_stack(0)) as usize;
        // this sampler read with boundary checks.
        let val = Machine::to_value(vec.get(pos).unwrap_or(&0.0));
        machine.set_stack(0, val);
        1
    };
    let ty = function!(vec![numeric!()], numeric!());
    let idx = machine.wrap_extern_cls(("loadwavfile_impl", Arc::new(res), ty));
    machine.set_stack(0, Machine::to_value(idx));
    1
}

pub fn get_signature() -> (&'static str, ExtFunType, TypeNodeId) {
    let t = function!(vec![string_t!()], function!(vec![numeric!()], numeric!()));
    ("loadwav", load_wavfile, t)
}
