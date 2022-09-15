#![feature(box_patterns)]
pub mod evaluator;
use utils::error::ReportableError;

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

pub fn eval_top(content: String) -> Result<evaluator::Value,Vec<Box<dyn ReportableError>>> {
    let ast = parser::parse(content)?;
    let hir = hirgen::generate_hir(ast)?;
    evaluator::eval(hir).map_err(|e| vec![e])
}

#[cfg(test)]
mod hireval_test {
    use super::evaluator;
    use super::*;
    use utils::fileloader;
    #[test]
    fn test() -> anyhow::Result<()> {

        let (content, _fullpath) = fileloader::load("test/hello.mmm".to_string())?;
        match eval_top(content) {
            Ok(evaluator::Value::Numeric(v)) => {
                assert_eq!(v, 2.0_f64.sqrt().sin())
            }
            Err(e) => panic!("Error here: \n{:?}", e),
            _ => panic!("Error"),
        }

        Ok(())
    }
}
