use super::Type;

#[macro_export]
macro_rules! integer {
    () => {
        Type::Int
    };
}
#[macro_export]
macro_rules! numeric {
    () => {
        Type::Numeric
    };
}
#[macro_export]
macro_rules! string_t {
    () => {
        Type::String
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
            refer!(function!(vec![integer!(),integer!()],numeric!())),
            string_t!()
        );
        let answer = Type::Tuple(vec![
            Type::Ref(Box::new(Type::Function(
                vec![Type::Int, Type::Int],
                Box::new(Type::Numeric),
                None,
            ))),
            Type::String,
        ]);
        assert_eq!(t, answer);
    }
}
