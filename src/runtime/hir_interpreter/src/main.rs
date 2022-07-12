use anyhow::Result;
use clap::Parser as _;
use hirgen::*;
use lexer::*;
use mirgen::*;
use parser::*;
use utils::{fileloader, metadata::WithMeta};
/// Simple program to greet a person
#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// File name
    #[clap(value_parser)]
    file: String,

    /// Number of times to greet
    #[clap(short, long, value_parser, default_value_t = 1)]
    count: u8,
}

fn eval(content: String) -> anyhow::Result<WithMeta<ast::expr::Expr>, parser::Errors> {
    parser::parse(content)
}
fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    for _ in 0..args.count {
        let (content, fullpath) = fileloader::load(args.file.clone())?;
        match eval(content) {
            Ok(ast) => {
                println!("Filename: {}", fullpath.display());

                println!("AST:\n{:?}", ast)
            }
            Err(e) => panic!("Error here: \n{:?}", e),
        }
    }
    Ok(())
}
