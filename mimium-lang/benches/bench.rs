#![feature(test)]
extern crate test;

fn main() {
    // 省略
}

#[cfg(test)]
mod tests {
    use super::*;
    use mimium_lang::compiler::emit_bytecode;
    use mimium_lang::runtime::vm::Machine;
    use test::Bencher;

    fn make_multiosc_src(n: usize) -> String {
        format!(
            "let pi = 3.14159265359
let sr = 44100.0
fn phasor(freq){{
  (self + freq/sr)%1.0
}}
fn osc(freq){{
  sin(phasor(freq)*pi*2.0)
}}
fn amosc(freq,rate){{
  osc(freq + osc(rate)*100.0)
}}

fn replicate(n:float,gen:()->(float,float)->float){{
    if (n>0.0){{
        let c = replicate(n - 1.0,gen)
        let g = gen()
        |x,rate| {{g(x,rate) + c(x+100.0,rate+0.5)}}
    }}else{{
        |x,rate| {{ 0.0 }}
    }}
}}
let mycounter = replicate({n}.0,| |amosc);
fn dsp(){{
    mycounter(500.0,0.5)*0.1
}}"
        )
    }

    fn bench_multiosc(b: &mut Bencher,n:usize) {
        let content = make_multiosc_src(n);
        let program = Box::new(emit_bytecode(&content).expect("ok"));
        let mut machine = Machine::new();
        machine.link_functions(&program);
        machine.execute_main(&program);
        let idx = program.get_fun_index("dsp").expect("ok");
        b.iter(move || machine.execute_idx(&program, idx));
    }
    #[bench]
    fn bench_multiosc5(b:&mut Bencher){
        bench_multiosc(b, 5);
    }
    #[bench]
    fn bench_multiosc7(b:&mut Bencher){
        bench_multiosc(b, 7);
    }
    #[bench]
    fn bench_multiosc10(b:&mut Bencher){
        bench_multiosc(b, 10);
    }
    #[bench]
    fn bench_multiosc15(b:&mut Bencher){
        bench_multiosc(b, 15);
    }
    // ...
}
