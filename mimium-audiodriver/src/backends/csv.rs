use std::{
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use mimium_lang::runtime::vm;

use crate::driver::Driver;

use super::local_buffer::LocalBufferDriver;

pub struct CsvDriver {
    driver: LocalBufferDriver,
    csv_file: Box<dyn Write>,
}

impl CsvDriver {
    pub fn new(times: usize, csv_file: Box<dyn Write>) -> Self {
        Self {
            driver: LocalBufferDriver::new(times),
            csv_file,
        }
    }
}

impl Driver for CsvDriver {
    type Sample = <LocalBufferDriver as Driver>::Sample;

    fn init(&mut self, vm: vm::Machine, sample_rate: Option<crate::driver::SampleRate>) -> bool {
        let res = self.driver.init(vm, sample_rate);

        let chunk_size = self.driver.get_ochannels();
        let mut header = String::new();
        for i in 0..chunk_size {
            header.push_str(&format!("ch{i}"));
            if i != chunk_size - 1 {
                header.push(',');
            } else {
                header.push('\n');
            }
        }

        // TODO: these erros should be handled. The Driver interface will
        // probably return Result to propagate runtime errors (e.g. dsp() not
        // found). Let's revisit here after it happens.
        self.csv_file
            .write_all(header.as_bytes())
            .expect("failed to write");
        self.csv_file.flush().expect("failed to flush");

        res
    }

    fn play(&mut self) -> bool {
        let res = self.driver.play();

        let chunk_size = self.driver.get_ochannels();
        let mut line = String::new();
        for sample in self.driver.get_generated_samples().chunks(chunk_size) {
            for (i, v) in sample.iter().enumerate() {
                // :? is to display "0" as "0.0"
                line.push_str(&format!("{v:?}"));
                if i != sample.len() - 1 {
                    line.push(',');
                } else {
                    line.push('\n');
                }
            }
        }

        self.csv_file
            .write_all(line.as_bytes())
            .expect("failed to write");
        self.csv_file.flush().expect("failed to flush");

        res
    }

    fn pause(&mut self) -> bool {
        self.driver.pause()
    }

    fn get_samplerate(&self) -> crate::driver::SampleRate {
        self.driver.get_samplerate()
    }

    fn get_current_sample(&self) -> mimium_lang::runtime::Time {
        self.driver.get_current_sample()
    }

    fn is_playing(&self) -> bool {
        self.driver.is_playing()
    }
}

pub fn csv_driver<P: AsRef<Path>>(times: usize, path: P) -> Box<dyn Driver<Sample = f64>> {
    let csv_file_inner = File::create(path.as_ref()).unwrap();
    let csv_file = Box::new(BufWriter::new(csv_file_inner));
    Box::new(CsvDriver::new(times, csv_file))
}

pub fn csv_driver_stdout(times: usize) -> Box<dyn Driver<Sample = f64>> {
    let csv_file = Box::new(BufWriter::new(std::io::stdout()));
    Box::new(CsvDriver::new(times, csv_file))
}
