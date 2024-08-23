use crate::input::Input;

use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Result, parse_quote, LitStr, Stmt};

pub(crate) fn generate(input: Input) -> Result<proc_macro2::TokenStream> {
    let mut index: usize = 0;
    let call_site_span = Span::call_site();

    let mut entry_point_symbols: Vec<Stmt> = Vec::new();
    let mut builtin_fn_symbols: Vec<Stmt> = Vec::new();
    let mut special_fn_symbols: Vec<Stmt> = Vec::new();

    let mut prefill_symbols: Vec<LitStr> = Vec::new();

    for e in input.entry_points.iter() {
        let name = &e.name;
        let const_name = format_ident!("{}", name.to_string().to_uppercase());

        entry_point_symbols.push(parse_quote!(
            pub const #const_name: Symbol = Symbol(#index);
        ));

        let label = match &e.label {
            Some(label) => label.clone(),
            None => LitStr::new(&name.to_string(), call_site_span),
        };
        prefill_symbols.push(label);

        index += 1;
    }

    for e in input.builtin_fns.iter() {
        let name = &e.name;
        let const_name = format_ident!("{}", name.to_string().to_uppercase());

        builtin_fn_symbols.push(parse_quote!(
            pub const #const_name: Symbol = Symbol(#index);
        ));

        prefill_symbols.push(LitStr::new(&name.to_string(), call_site_span));

        index += 1;
    }

    for e in input.special_fns.iter() {
        let name = &e.name;
        let const_name = format_ident!("{}", name.to_string().to_uppercase());

        special_fn_symbols.push(parse_quote!(
            pub const #const_name: Symbol = Symbol(#index);
        ));

        prefill_symbols.push(LitStr::new(&name.to_string(), call_site_span));

        index += 1;
    }

    let out = quote!(
        pub mod symbols {
            pub mod entry_point {
                use crate::interner::Symbol;
                #(#entry_point_symbols)*
            }

            pub mod builtin_fn {
                use crate::interner::Symbol;
                #(#builtin_fn_symbols)*
            }

            pub mod special_fn {
                use crate::interner::Symbol;
                #(#special_fn_symbols)*
            }
        }

        impl crate::interner::SessionGlobals {
            pub(crate) fn prefill(&mut self) {
                for s in [#(#prefill_symbols),*] {
                    self.symbol_interner.get_or_intern(s);
                }
            }
        }
    );

    Ok(out.into_token_stream())
}

#[cfg(test)]
mod tests {
    use prettyplease::unparse;
    use proc_macro2::Span;
    use syn::{parse_quote, punctuated::Punctuated, File, Ident, LitStr, Token};

    use crate::{
        input::{BuiltinFn, EntryPoint, FunctionSignature, Input, SpecialFn},
        output::generate,
    };

    #[test]
    fn test_generated_output() {
        let mut entry_points: Punctuated<EntryPoint, Token![,]> = Punctuated::new();
        for (name, label) in [("dsp", None), ("global", Some("_mimium_global"))] {
            entry_points.push(EntryPoint {
                name: Ident::new(name, Span::call_site()),
                label: label.map(|x| LitStr::new(x, Span::call_site())),
            });
        }

        let mut builtin_fns: Punctuated<BuiltinFn, Token![,]> = Punctuated::new();
        for name in ["neg", "sin", "cos"] {
            builtin_fns.push(BuiltinFn {
                name: Ident::new(name, Span::call_site()),
                ty: FunctionSignature::Unsupported,
            });
        }

        let mut special_fns: Punctuated<SpecialFn, Token![,]> = Punctuated::new();
        for name in ["delay"] {
            special_fns.push(SpecialFn {
                name: Ident::new(name, Span::call_site()),
                ty: FunctionSignature::Unsupported,
            });
        }

        let input = Input {
            entry_points,
            builtin_fns,
            special_fns,
        };

        let out = generate(input).unwrap();
        let file: File = parse_quote! {
            #out
        };
        assert_eq!(
            &unparse(&file),
            r#"pub mod symbols {
    pub mod entry_point {
        use crate::interner::Symbol;
        pub const DSP: Symbol = Symbol(0usize);
        pub const GLOBAL: Symbol = Symbol(1usize);
    }
    pub mod builtin_fn {
        use crate::interner::Symbol;
        pub const NEG: Symbol = Symbol(2usize);
        pub const SIN: Symbol = Symbol(3usize);
        pub const COS: Symbol = Symbol(4usize);
    }
    pub mod special_fn {
        use crate::interner::Symbol;
        pub const DELAY: Symbol = Symbol(5usize);
    }
}
impl crate::interner::SessionGlobals {
    pub(crate) fn prefill(&mut self) {
        for s in ["dsp", "_mimium_global", "neg", "sin", "cos", "delay"] {
            self.symbol_interner.get_or_intern(s);
        }
    }
}
"#
        )
    }
}
