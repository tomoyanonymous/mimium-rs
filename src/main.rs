// pub mod wcalculus;
use clap::Parser as _;

use mimium_rs::compiler::parser;
use mimium_rs::utils::{
    error::{report, ReportableError},
    fileloader,
};
use mimium_rs::{ast_interpreter, eval_top, repl};

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,
}
#[derive(Clone, Debug)]
struct Context {
    src: String, //for debug
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut global_env = Vec::<(String, ast_interpreter::Value)>::new();
    match args.file {
        Some(file) => {
            let (content, fullpath) = fileloader::load(file.clone())?;
            match eval_top(content.clone(), &mut global_env) {
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
