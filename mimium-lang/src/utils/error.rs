use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use std::path;

pub trait ReportableError: std::error::Error {
    fn get_span(&self) -> std::ops::Range<usize>;
    /// message is used for reporting verbose message for ariadne.
    fn get_message(&self, _color: Color) -> String {
        self.to_string()
    }
    /// label is used for indicating error with the specific position for ariadne.
    fn get_label(&self, _color: Color) -> String {
        self.to_string()
    }
}

#[derive(Debug)]
pub struct ReportableErrorDyn {
    pub message: String,
    pub span: std::ops::Range<usize>,
}
impl std::fmt::Display for ReportableErrorDyn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for ReportableErrorDyn {}
impl ReportableError for ReportableErrorDyn {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }
}

pub fn report<T>(src: &String, srcpath: T, errs: &[Box<dyn ReportableError>])
where
    T: AsRef<path::Path>,
{
    let path = srcpath.as_ref().to_str().unwrap_or_default();
    let mut colors = ColorGenerator::new();
    for e in errs {
        let color = colors.next();
        let span = e.get_span();
        // let a_span = (src.source(), span);color
        let label = Label::new((path, span.clone()))
            .with_message(e.get_label(color))
            .with_color(color);
        let builder = Report::build(ReportKind::Error, "test", 4)
            .with_message(e.get_message(color))
            .with_label(label)
            .finish();
        builder.eprint((path, Source::from(src))).unwrap();
    }
}

pub fn dump_to_string(errs: &[Box<dyn ReportableError>]) -> String {
    let mut res = String::new();
    for e in errs {
        res += e.get_message(Color::Green).as_str();
    }
    res
}
