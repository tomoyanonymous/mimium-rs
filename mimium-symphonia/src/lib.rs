use std::fs::File;
use std::sync::Arc;

use mimium_lang::interner::ToSymbol;
use mimium_lang::runtime::vm::{self, Machine, ExtClsType, ExtFnInfo, ReturnCode};
use symphonia::core::audio::{Layout, SampleBuffer, SignalSpec};
use symphonia::core::codecs::{Decoder, DecoderOptions, CODEC_TYPE_NULL};
use symphonia::core::errors::Error;
use symphonia::core::formats::{FormatOptions, FormatReader, SeekMode, SeekTo};
use symphonia::core::io::{MediaSource, MediaSourceStream, MediaSourceStreamOptions};
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::{Hint, ProbeResult};
use symphonia::core::units::Time;
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

pub struct FileSampler {
    data: Vec<f64>,
}

fn load_wavfile_to_vec(path:&str)->Vec<f64> {
    let (decoder, mut probed, id) = get_default_decoder(path).expect("failed to find file");
    let channels = decoder.codec_params().channels.unwrap().count() as u64;
    let max_frames = decoder.codec_params().max_frames_per_packet.unwrap();
    let audiobuffer = SampleBuffer::<f32>::new(
        max_frames,
        SignalSpec::new_with_layout(48000, Layout::Stereo),
    );
    probed.format.seek(
        SeekMode::Accurate,
        SeekTo::Time {
            time: Time::from_ss(0, 0).unwrap(),
            track_id: Some(id),
        },
    );
    match probed.format.next_packet() {
        Ok(packet) => todo!(),
        Err(_) => todo!(),
    }
}
   
fn load_wavfile(machine: &mut Machine)->ReturnCode{
    //return closure
    let path = vm::Machine::get_as_array::<&str>(machine.get_stack_range(0, 2).1)[0];
    let vec = load_wavfile_to_vec(path);
    let res =   move |machine: &mut Machine|->ReturnCode{
        let pos = vm::Machine::get_as::<f64>(machine.get_stack(0)) as usize;
        let val = Machine::to_value(vec[pos]);
        machine.set_stack(0, val);
        todo!()
    };
    let idx = machine.install_extern_cls("loadwavfile_impl".to_symbol(), Arc::new(res));
    machine.set_stack(0, idx as _);
    1
}

