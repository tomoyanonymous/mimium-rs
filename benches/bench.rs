#![feature(test)]
extern crate test;

fn main() {
    // 省略
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use mimium_rs::compiler::emit_ast;
    use mimium_rs::ast_interpreter::{eval_ast, Context};

    #[bench]
    fn bench_eval_ast(b: &mut Bencher) {
        let src = "fn counter(){self+1.0}
counter()".to_string();
        let ast = Box::new(emit_ast(&src).expect("ok"));
        let mut ctx = Context::new();
        b.iter(move || eval_ast(&ast, &mut ctx));
    }
    
    // ...
}