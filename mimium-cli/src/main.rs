use std::io::stdin;

// pub mod wcalculus;
use clap::Parser;
use colog;
use log;
use mimium_audiodriver::driver::load_default_runtime;
use mimium_lang::compiler::{emit_ast, emit_bytecode};
use mimium_lang::utils::error::ReportableError;
use mimium_lang::utils::miniprint::MiniPrint;
use mimium_lang::utils::{error::report, fileloader};
use mimium_lang::{compiler::emit_mir, compiler::mirgen::selfconvert, repl};
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// File name
    #[clap(value_parser)]
    pub file: Option<String>,
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,
    #[arg(long, default_value_t = false)]
    pub emit_mir: bool,
    #[arg(long, default_value_t = false)]
    pub emit_bytecode: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if cfg!(debug_assertions) | cfg!(test) {
        colog::default_builder()
            .filter_level(log::LevelFilter::Debug)
            .init();
    } else {
        colog::default_builder().init();
    }
    let args = Args::parse();
    match args.file {
        Some(file) => {
            let (content, fullpath) = fileloader::load(file.clone())?;
            if args.emit_ast {
                println!("Filename: {}", fullpath.display());
                let ast = emit_ast(&content.clone()).unwrap();
                let expr2 = selfconvert::convert_self_top(ast)
                    .map_err(|e| {
                        let eb: Box<dyn ReportableError> = Box::new(e);
                        eb
                    })
                    .unwrap();
                match emit_ast(&content.clone()) {
                    Ok(ast) => println!("{}", expr2.pretty_print()),
                    Err(e) => {
                        report(&content, fullpath, &e);
                    }
                }
            } else if args.emit_mir {
                println!("Filename: {}", fullpath.display());
                match emit_mir(&content.clone()) {
                    Ok(mir) => println!("{mir}"),
                    Err(e) => {
                        report(&content, fullpath, &e);
                    }
                }
            } else if args.emit_bytecode {
                println!("Filename: {}", fullpath.display());
                match emit_bytecode(&content.clone()) {
                    Ok(prog) => println!("{prog}"),
                    Err(e) => {
                        report(&content, fullpath, &e);
                    }
                }
            } else {
                println!("Filename: {}", fullpath.display());
                match emit_bytecode(&content.clone()) {
                    Ok(prog) => {
                        let mut driver = load_default_runtime();
                        driver.init(prog, None, 4096);
                        let mut dummy = String::new();
                        driver.play();
                        //wait until input something
                        let _size = stdin().read_line(&mut dummy).expect("stdin read error.");
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
