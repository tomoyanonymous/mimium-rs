use crate::utils::{error::ReportableError, metadata::Span};
use ariadne::{Color, Fmt};
use chumsky;
use std::fmt;
use std::hash::Hash;
// pub struct LexError(chumsky::error::Simple<char>);
#[derive(Debug)]
pub struct ParseError<T>(pub chumsky::error::Simple<T>)
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display;

impl<T> Into<chumsky::error::Simple<T>> for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn into(self) -> chumsky::error::Simple<T> {
        self.0
    }
}
impl<T> fmt::Display for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> std::error::Error for ParseError<T> where T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display {}

impl<T> ReportableError for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn get_span(&self) -> Span {
        self.0.span()
    }
    fn get_message(&self) -> String {
        match self.0.reason() {
            chumsky::error::SimpleReason::Unexpected
            | chumsky::error::SimpleReason::Unclosed { .. } => {
                format!(
                    "{}{}, expected {}",
                    if self.0.found().is_some() {
                        "unexpected token"
                    } else {
                        "unexpected end of input"
                    },
                    if let Some(label) = self.0.label() {
                        format!(" while parsing {}", label.fg(Color::Green))
                    } else {
                        " something else".to_string()
                    },
                    if self.0.expected().count() == 0 {
                        "somemething else".to_string()
                    } else {
                        self.0
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
    fn get_label(&self) -> String {
        match self.0.reason() {
            chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
            _ => format!(
                "Unexpected {}",
                self.0
                    .found()
                    .map(|c| format!("token {}", c.fg(Color::Red)))
                    .unwrap_or_else(|| "end of input".to_string())
            ),
        }
    }
}
