// Note: This implementation is inspired from the code in Rust compiler.
//
// https://github.com/rust-lang/rust/blob/master/compiler/rustc_macros/src/symbols.rs

extern crate proc_macro;
use syn::{
    braced,
    parse::{Parse, ParseStream, Result},
    punctuated::Punctuated,
    Error, Ident, LitStr, Token,
};

// custom_keyword allows us to use a specified identifier as if it were a
// keyword, which means we can do more than the Rust syntax.
mod kw {
    // e.g. "dsp"
    syn::custom_keyword!(EntryPoint);
    // e.g. "neg"
    syn::custom_keyword!(Intrinsic);
    // e.g. "print"
    syn::custom_keyword!(BuiltinFn);
    // e.g. "delay"
    syn::custom_keyword!(SpecialFn);
}

pub(crate) struct Input {
    pub(crate) entry_points: Punctuated<EntryPoint, Token![,]>,
    pub(crate) intrinsics: Punctuated<Intrinsic, Token![,]>,
    pub(crate) builtin_fns: Punctuated<BuiltinFn, Token![,]>,
    pub(crate) special_fns: Punctuated<SpecialFn, Token![,]>,
}

pub(crate) struct EntryPoint {
    pub(crate) name: Ident,
    pub(crate) label: Option<LitStr>,
}

pub(crate) struct Intrinsic {
    pub(crate) name: Ident,
    pub(crate) ty: IntrinsicType, // TODO
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IntrinsicType {
    UnaryOp,
    BinaryOp,
    Fn,
}

pub(crate) struct BuiltinFn {
    pub(crate) name: Ident,
    pub(crate) ty: FunctionSignature, // TODO
}

pub(crate) struct SpecialFn {
    pub(crate) name: Ident,
    pub(crate) ty: FunctionSignature, // TODO
}

pub(crate) enum FunctionSignature {
    Unsupported,
}

impl Parse for Input {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        input.parse::<kw::EntryPoint>()?;
        let content;
        braced!(content in input);
        let entry_points = Punctuated::parse_terminated(&content)?;

        input.parse::<kw::Intrinsic>()?;
        let content;
        braced!(content in input);
        let intrinsics = Punctuated::parse_terminated(&content)?;

        input.parse::<kw::BuiltinFn>()?;
        let content;
        braced!(content in input);
        let builtin_fns = Punctuated::parse_terminated(&content)?;

        input.parse::<kw::SpecialFn>()?;
        let content;
        braced!(content in input);
        let special_fns = Punctuated::parse_terminated(&content)?;

        Ok(Input {
            entry_points,
            intrinsics,
            builtin_fns,
            special_fns,
        })
    }
}

impl Parse for EntryPoint {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let alias = if colon_token.is_some() {
            input.parse()?
        } else {
            None
        };
        Ok(EntryPoint { name, label: alias })
    }
}

impl Parse for Intrinsic {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let ty = if colon_token.is_some() {
            input.parse()?
        } else {
            IntrinsicType::Fn
        };

        Ok(Intrinsic { name, ty })
    }
}

impl Parse for IntrinsicType {
    fn parse(input: ParseStream) -> Result<Self> {
        let ty: Ident = input.parse()?;
        match ty.to_string().as_str() {
            "unary" => Ok(IntrinsicType::UnaryOp),
            "binary" => Ok(IntrinsicType::BinaryOp),
            "fn" => Ok(IntrinsicType::Fn),
            _ => Err(Error::new_spanned(ty, "Unknown intrinsic type")),
        }
    }
}

impl Parse for BuiltinFn {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let ty = if colon_token.is_some() {
            input.parse()?
        } else {
            FunctionSignature::Unsupported
        };

        Ok(BuiltinFn { name, ty })
    }
}

impl Parse for SpecialFn {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let ty = if colon_token.is_some() {
            input.parse()?
        } else {
            FunctionSignature::Unsupported
        };

        Ok(SpecialFn { name, ty })
    }
}

impl Parse for FunctionSignature {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(FunctionSignature::Unsupported)
    }
}

#[cfg(test)]
mod tests {
    use syn::{parse_quote, Macro};

    use crate::{input::IntrinsicType, Input};

    #[test]
    fn test_parse_input() {
        let input: Macro = parse_quote!(mimium_symbols! {
            EntryPoint {
                dsp,
                global: "_mimium_global",
            }

            Intrinsic {
                neg: unary,
                add: binary,
                sin,
                cos,
            }

            BuiltinFn {
                print,
                println,
            }

            SpecialFn {
                delay,
                mem,
            }
        });

        let input = match syn::parse2::<Input>(input.tokens) {
            Ok(expr) => expr,
            Err(e) => panic!("{}", e.to_compile_error()),
        };

        assert_eq!(
            &input
                .entry_points
                .iter()
                .map(|x| {
                    let alias = match &x.label {
                        Some(alias) => alias.value(),
                        None => "".to_string(),
                    };
                    (x.name.to_string(), alias)
                })
                .collect::<Vec<_>>(),
            &[
                ("dsp".to_string(), "".to_string()),
                ("global".to_string(), "_mimium_global".to_string())
            ]
        );

        assert_eq!(
            &input
                .intrinsics
                .iter()
                .map(|x| { (x.name.to_string(), x.ty.clone()) })
                .collect::<Vec<_>>(),
            &[
                ("neg".to_string(), IntrinsicType::UnaryOp),
                ("add".to_string(), IntrinsicType::BinaryOp),
                ("sin".to_string(), IntrinsicType::Fn),
                ("cos".to_string(), IntrinsicType::Fn),
            ]
        );

        assert_eq!(
            &input
                .builtin_fns
                .iter()
                .map(|x| x.name.to_string())
                .collect::<Vec<_>>(),
            &["print", "println"]
        );
        assert_eq!(
            &input
                .special_fns
                .iter()
                .map(|x| x.name.to_string())
                .collect::<Vec<_>>(),
            &["delay", "mem"]
        );
    }
}
