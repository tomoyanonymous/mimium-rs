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
        Type::Function($params, Box::new($return), None)
    };
}

#[macro_export]
macro_rules! refer {
    ($t:expr) => {
        Type::Ref(Box::new($t))
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
            refer!(function!(vec![integer!(), integer!()], numeric!())),
            string_t!()
        );
        let answer = Type::Tuple(vec![
            Type::Ref(Box::new(Type::Function(
                vec![Type::Primitive(PType::Int), Type::Primitive(PType::Int)],
                Box::new(Type::Primitive(PType::Numeric)),
                None,
            ))),
            Type::Primitive(PType::String),
        ]);
        assert_eq!(t, answer);
    }
}
