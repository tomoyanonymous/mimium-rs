// Note: This implementation is inspired from the code in Rust compiler.
//
// https://github.com/rust-lang/rust/blob/master/compiler/rustc_macros/src/symbols.rs

mod input;
mod output;

pub(crate) use input::Input;

extern crate proc_macro;

#[proc_macro]
pub fn mimium_symbols(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = match syn::parse::<Input>(input.clone()) {
        Ok(expr) => expr,
        Err(e) => return e.to_compile_error().into(),
    };

    match output::generate(input) {
        Ok(out) => out.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
