// pub mod wcalculus;
use clap::Parser;

use mimium_rs::ast_interpreter;
use mimium_rs::utils::{error::report, fileloader};
use mimium_rs::{
    compiler::{emit_mir, eval_top},
    repl,
};

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,
    #[arg(long, default_value_t = false)]
    pub emit_mir: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let mut global_ctx = ast_interpreter::Context::new();
    match args.file {
        Some(file) => {
            let (content, fullpath) = fileloader::load(file.clone())?;
            if args.emit_mir {
                println!("Filename: {}", fullpath.display());
                match emit_mir(&content.clone()) {
                    Ok(mir) => println!("{mir}"),
                    Err(e) => {
                        report(&content, fullpath, &e);
                    }
                }
            } else {
                match eval_top(content.clone(), &mut global_ctx) {
                    Ok(v) => {
                        println!("Filename: {}", fullpath.display());
                        println!("Value:\n{:?}", v);
                    }
                    Err(e) => {
                        report(&content, fullpath, &e);
                    }
                }
            }
        }
        None => {
            repl::run_repl();
        }
    };
    Ok(())
}
