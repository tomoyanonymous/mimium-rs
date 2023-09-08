// pub mod wcalculus;
use clap::Parser as _;

use mimium_rs::ast_interpreter;
use mimium_rs::utils::{environment::Environment, error::report, fileloader};
use mimium_rs::{compiler::eval_top, repl};

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut global_ctx = ast_interpreter::Context::new();
    match args.file {
        Some(file) => {
            let (content, fullpath) = fileloader::load(file.clone())?;
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
        None => {
            repl::run_repl();
        }
    };
    Ok(())
}
