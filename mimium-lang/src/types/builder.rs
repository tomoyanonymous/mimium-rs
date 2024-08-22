use super::PType;
use super::Type;

#[macro_export]
macro_rules! unit {
    () => {
        Type::Primitive(PType::Unit)
    };
}

#[macro_export]
macro_rules! integer {
    () => {
        Type::Primitive(PType::Int)
    };
}
#[macro_export]
macro_rules! numeric {
    () => {
        Type::Primitive(PType::Numeric)
    };
}
#[macro_export]
macro_rules! string_t {
    () => {
        Type::Primitive(PType::String)
    };
}
#[macro_export]
macro_rules! function {
    ($params:expr, $return:expr) => {
        Type::Function($params, $return.into_id_without_span(), None)
    };
}

#[macro_export]
macro_rules! refer {
    ($t:expr) => {
        Type::Ref($t.into_id_without_span())
    };
}

#[macro_export]
macro_rules! tuple {

    ($($t:expr),*) => {
        Type::Tuple(vec![$($t,)*])
    };
}

#[cfg(test)]
mod typemacro_test {
    use super::*;
    #[test]
    fn buildertest() {
        let t = tuple!(
            refer!(function!(vec![integer!(), integer!()], numeric!())).into_id_without_span(),
            string_t!().into_id_without_span()
        );
        let answer = Type::Tuple(vec![
            Type::Ref(
                Type::Function(
                    vec![Type::Primitive(PType::Int), Type::Primitive(PType::Int)],
                    Type::Primitive(PType::Numeric).into_id_without_span(),
                    None,
                )
                .into_id_without_span(),
            )
            .into_id_without_span(),
            Type::Primitive(PType::String).into_id_without_span(),
        ]);
        assert_eq!(t, answer);
    }
}
