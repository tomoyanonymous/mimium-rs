// pub mod wcalculus;
use clap::Parser as _;

use mimium_rs::ast_interpreter;
use mimium_rs::compiler::parser;
use mimium_rs::utils::{
    error::{report, ReportableError},
    fileloader,
};

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// File name
    #[clap(value_parser)]
    pub file: String,

    /// Number of times to greet
    #[clap(short, long, value_parser, default_value_t = 1)]
    pub count: u8,
}
#[derive(Clone, Debug)]
struct Context {
    src: String, //for debug
}

pub fn eval_top(
    content: String,
    global_env: &mut Vec<(String, ast_interpreter::Value)>,
) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
    let ast = parser::parse(content)?;
    ast_interpreter::eval_ast(ast.into(), global_env).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        vec![eb]
    })
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut global_env = Vec::<(String, ast_interpreter::Value)>::new();
    for _ in 0..args.count {
        let (content, fullpath) = fileloader::load(args.file.clone())?;
        match eval_top(content.clone(), &mut global_env) {
            Ok(v) => {
                println!("Filename: {}", fullpath.display());

                println!("Value:\n{:?}", v)
            }
            Err(e) => report(&content, fullpath, e),
        }
    }
    Ok(())
}
