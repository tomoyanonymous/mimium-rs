use crate::interner::Symbol;
use crate::utils::error::ReportableError;
use crate::utils::metadata::Location;
use chumsky;
use std::fmt;
use std::hash::Hash;
// pub struct LexError(chumsky::error::Simple<char>);
#[derive(Debug)]
pub struct ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    pub content: chumsky::error::Simple<T>,
    pub file: Symbol,
}

impl<T> Into<chumsky::error::Simple<T>> for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn into(self) -> chumsky::error::Simple<T> {
        self.content
    }
}
impl<T> fmt::Display for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.content)
    }
}

impl<T> std::error::Error for ParseError<T> where T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display {}

impl<T> ReportableError for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn get_message(&self) -> String {
        match self.content.reason() {
            chumsky::error::SimpleReason::Unexpected
            | chumsky::error::SimpleReason::Unclosed { .. } => {
                format!(
                    "{}{}, expected {}",
                    if self.content.found().is_some() {
                        "unexpected token"
                    } else {
                        "unexpected end of input"
                    },
                    if let Some(label) = self.content.label() {
                        format!(" while parsing {label}")
                    } else {
                        " something else".to_string()
                    },
                    if self.content.expected().count() == 0 {
                        "somemething else".to_string()
                    } else {
                        self.content
                            .expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                )
            }
            chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
        }
    }

    fn get_labels(&self) -> Vec<(Location, String)> {
        vec![(
            Location {
                span: self.content.span(),
                path: self.file,
            },
            self.get_message(),
        )]
    }
}
