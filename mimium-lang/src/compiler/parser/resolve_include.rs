use super::{parse, Span};
use crate::interner::ExprNodeId;
use crate::utils::error::{ReportableError, ReportableErrorDyn};
use crate::utils::fileloader;

fn make_vec_error<E: std::error::Error>(e: E, span: Span) -> Vec<Box<dyn ReportableError>> {
    vec![Box::new(ReportableErrorDyn {
        message: e.to_string(),
        span: span.clone(),
    }) as Box<dyn ReportableError>]
}

pub(super) fn resolve_include(
    mmm_filepath: &str,
    target_path: &str,
    span: Span,
) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let abspath = fileloader::get_canonical_path(mmm_filepath, target_path)
        .map_err(|e| make_vec_error(e, span.clone()))?;
    let content =
        fileloader::load(abspath.to_str().unwrap()).map_err(|e| make_vec_error(e, span.clone()))?;
    parse(&content, Some(abspath))
}
