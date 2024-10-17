use super::{parse, Span};
use crate::interner::ExprNodeId;
use crate::utils::error::{ReportableError, ReportableErrorDyn};
use crate::utils::fileloader;

pub(super) fn resolve_include(
    mmm_filepath: &str,
    target_path: &str,
    span: Span,
) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let make_err = |e: fileloader::Error| {
        vec![Box::new(ReportableErrorDyn {
            message: e.to_string(),
            span: span.clone(),
        }) as Box<dyn ReportableError>]
    };
    let abspath = fileloader::get_canonical_path(mmm_filepath, target_path).map_err(make_err)?;
    let content = fileloader::load(abspath.to_str().unwrap()).map_err(make_err)?;
    parse(&content, Some(abspath))
}
