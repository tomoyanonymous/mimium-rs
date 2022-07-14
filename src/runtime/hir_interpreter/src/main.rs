use mimium_hir_interpreter::*;

use clap::Parser as _;
use utils::{fileloader, metadata::WithMeta};

fn main() -> anyhow::Result<()> {
    let args = mimium_hir_interpreter::Args::parse();
    for _ in 0..args.count {
        let (content, fullpath) = fileloader::load(args.file.clone())?;
        match eval_top(content) {
            Ok(v) => {
                println!("Filename: {}", fullpath.display());

                println!("Value:\n{:?}", v)
            }
            Err(e) => panic!("Error here: \n{:?}", e),
        }
    }
    Ok(())
}


