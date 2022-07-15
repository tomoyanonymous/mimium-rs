use ariadne::{Label, Report, ReportKind, Source};
use std::path;

pub trait ReportableError: std::error::Error {
    /// message is used for reporting verbose message for ariadne.
    ///
    fn get_span(&self) -> std::ops::Range<usize>;
    fn get_message(&self) -> String;
    /// label is used for indicating error with the specific position for ariadne.
    fn get_label(&self) -> String;
}

pub fn report(src: &String, srcpath: path::PathBuf, errs: Vec<Box<dyn ReportableError>>) {
    let path = srcpath.to_str().unwrap_or_default();
    for e in errs {
        let span = e.get_span();
        // let a_span = (src.source(), span);
        let builder = Report::build(ReportKind::Error, "test", 4)
            .with_message(e.get_message().as_str())
            .with_label(Label::new((path, span.clone())))
            .with_message(e.get_label().as_str())
            .finish();
        builder.eprint((path, Source::from(src.as_str()))).unwrap();
    }
}
