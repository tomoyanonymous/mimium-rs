use utils::fileloader;
use clap::Parser;
use hirgen::*;
use lexer::*;
use mirgen::*;
use parser::*;
/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// File name
    #[clap(value_parser)]
    file: String,

    /// Number of times to greet
    #[clap(short, long, value_parser, default_value_t = 1)]
    count: u8,
}



fn main() {
    let args = Args::parse();
    for _ in 0..args.count {
        let res = fileloader::load(args.file.clone());
        match res {
            Ok((content, fullpath)) => {
                println!("Filename: {}", fullpath.display());
                println!("{}", content)
            }
            Err(e) => panic!("Error here: \n{:?}", e),
        }
    }
}
