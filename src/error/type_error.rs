use std::sync::Arc;
use crate::error::Reportable;
use crate::parser::Span;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Span as AriadneSpan};

use crate::syntax::r#type::Type;

#[derive(Clone, Debug)]
pub enum TypeError {
    BoxedExprHasLocalDeps(usize, Span, String),
    Mismatch {
        offset: usize,
        span: Span,
        expected: Type,
        actual: Type,
    },
    MissingVariants(usize, Span, Vec<(String, Type)>),
    UndefinedVariable(usize, Span, String),
    UnknownField(usize, Span, String, Type),
    UnknownIndex {
        offset: usize,
        span: Span,
        index: usize,
        tuple_type: Type,
    },
    UnknownType(usize, Span, String),
}

impl Reportable for TypeError {
    fn build_report(&self) -> Report<Span> {
        let report = Report::<Span>::build(ReportKind::Error, self.span().source(), self.offset());

        match self {
            Self::BoxedExprHasLocalDeps(_, span, expr) => report
                .with_message("Boxed expr depends on local values")
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!("Expr {} has local deps", expr.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            Self::Mismatch {
                offset: _,
                span,
                actual,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected `{}`, found `{}`",
                        expected.to_string().fg(Color::Green),
                        actual.to_string().fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            ),
            TypeError::MissingVariants(_, span, variants) => report
                .with_message("missing variants")
                .with_labels(variants.iter().map(|(label, variant_type)| {
                    Label::new(span.clone())
                        .with_message(format!(
                            "missing {}{} {}",
                            label.to_string().fg(Color::Red),
                            "{}".fg(Color::Red),
                            variant_type.to_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red)
                }))
                .with_note("Ensure all variants are exhausted by match arms."),
            Self::UndefinedVariable(_, span, name) => report
                .with_message(format!("cannot find variable `{name}` in this scope"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownField(_, span, label, field_type) => report
                .with_message(format!("no field `{label}` on record `{field_type}`"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("unknown field")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownIndex {
                offset: _,
                span,
                index,
                tuple_type,
            } => report
                .with_message(format!("no index `{index}` in tuple `{tuple_type}`"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("unknown index")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownType(_, span, name) => report
                .with_message(format!("cannot find type `{name}` in this scope"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                ),
        }
        .finish()
    }

    fn offset(&self) -> usize {
        *match self {
            Self::BoxedExprHasLocalDeps(offset, _, _) => offset,
            Self::Mismatch {
                offset,
                span: _,
                actual: _,
                expected: _,
            } => offset,
            Self::MissingVariants(offset, _, _) => offset,
            Self::UndefinedVariable(offset, _, _) => offset,
            Self::UnknownField(offset, _, _, _) => offset,
            Self::UnknownIndex {
                offset,
                span: _,
                index: _,
                tuple_type: _,
            } => offset,
            Self::UnknownType(offset, _, _) => offset,
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::BoxedExprHasLocalDeps(_, span, _) => span,
            Self::Mismatch {
                offset: _,
                span,
                actual: _,
                expected: _,
            } => span,
            Self::MissingVariants(_, span, _) => span,
            Self::UndefinedVariable(_, span, _) => span,
            Self::UnknownField(_, span, _, _) => span,
            Self::UnknownIndex {
                offset: _,
                span,
                index: _,
                tuple_type: _,
            } => span,
            Self::UnknownType(_, span, _) => span,
        }
    }
}

impl From<TypeError> for super::Error {
    fn from(err: TypeError) -> Self {
        Box::new(err)
    }
}

impl From<TypeError> for super::Errors {
    fn from(err: TypeError) -> Self {
        vec![Box::new(err)]
    }
}

impl<T> From<TypeError> for Result<T, super::Errors> {
    fn from(err: TypeError) -> Self {
        Err(vec![Box::new(err)])
    }
}
