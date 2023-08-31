use super::hir_interpreter::*;

use clap::Parser as _;
use crate::compiler::utils::{error, fileloader};

fn main() -> anyhow::Result<()> {
    let args = mimium_hir_interpreter::Args::parse();
    for _ in 0..args.count {
        let (content, fullpath) = fileloader::load(args.file.clone())?;
        match eval_top(content.clone()) {
            Ok(v) => {
                println!("Filename: {}", fullpath.display());

                println!("Value:\n{:?}", v)
            }
            Err(e) => error::report(&content, fullpath, e),
        }
    }
    Ok(())
}
