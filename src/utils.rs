pub mod error;
pub mod metadata;
pub mod environment;
pub mod fileloader;
pub mod miniprint;

#[macro_export]
macro_rules! format_vec {
    ($vec:expr) => {
        $vec.iter().fold("".to_string(), |a, b| {
            if a.is_empty(){
                format!("{}",b)
            }else{
                format!("{},{}",a,b)
            }
        })
    };
}