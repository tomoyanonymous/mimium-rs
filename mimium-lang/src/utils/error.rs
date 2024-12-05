use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::interner::Symbol;

use super::metadata::Location;

pub trait ReportableError: std::error::Error {
    /// message is used for reporting verbose message for ariadne.
    fn get_message(&self) -> String {
        self.to_string()
    }
    /// label is used for indicating error with the specific position for ariadne.
    fn get_labels(&self) -> Vec<(Location, String)>;
}

#[derive(Debug, Clone)]
pub struct SimpleError {
    pub message: String,
    pub span: Location,
}
impl std::fmt::Display for SimpleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for SimpleError {}
impl ReportableError for SimpleError {
    fn get_labels(&self) -> Vec<(Location, String)> {
        vec![(self.span.clone(), self.message.clone())]
    }
}

struct FileCache {
    path: Symbol,
    src: ariadne::Source<String>,
}

impl ariadne::Cache<Symbol> for FileCache {
    type Storage = String;

    fn fetch(
        &mut self,
        _id: &Symbol,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.src)
    }

    fn display<'a>(&self, id: &'a Symbol) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id.to_string()))
    }
}

pub fn report(src: &String, path: Symbol, errs: &[Box<dyn ReportableError>]) {
    let mut colors = ColorGenerator::new();
    for e in errs {
        // let a_span = (src.source(), span);color
        let rawlabels = e.get_labels();
        let labels = rawlabels.iter().map(|(span, message)| {
            Label::new(span.clone())
                .with_message(message)
                .with_color(colors.next())
        });
        let builder = Report::build(ReportKind::Error, path, 4)
            .with_message(e.get_message())
            .with_labels(labels)
            .finish();
        builder
            .eprint(FileCache {
                path,
                src: ariadne::Source::from(src.clone()),
            })
            .unwrap();
    }
}

pub fn dump_to_string(errs: &[Box<dyn ReportableError>]) -> String {
    let mut res = String::new();
    for e in errs {
        res += e.get_message().as_str();
    }
    res
}
