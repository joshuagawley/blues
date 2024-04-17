use crate::error::Reportable;
use crate::parser::Span;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Span as AriadneSpan};

use crate::syntax::{pattern::Pattern, r#type::Type};

#[derive(Clone, Debug)]
pub enum PatternError {
    Incompatible {
        span: Span,
        actual: Pattern,
        expected: Type,
    },

    MissingElements {
        span: Span,
        actual: usize,
        expected: usize,
    },
}

impl Reportable for PatternError {
    fn build_report(&self) -> Report<Span> {
        let report = Report::<Span>::build(ReportKind::Error, self.span().source(), self.offset());
        match self {
            Self::Incompatible {
                span,
                actual: _,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected {}",
                        expected.to_string().fg(Color::Green)
                    ))
                    .with_color(Color::Red),
            ),
            Self::MissingElements {
                span,
                actual,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected a tuple with {} elements, found one with {} elements",
                        expected.to_string().fg(Color::Green),
                        actual.to_string().fg(Color::Red),
                    ))
                    .with_color(Color::Red),
            ),
        }
        .finish()
    }

    fn offset(&self) -> usize {
        match self {
            Self::Incompatible { span, .. } | Self::MissingElements { span, .. } => span.start(),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::Incompatible { span, .. } | Self::MissingElements { span, .. } => span,
        }
    }
}

impl From<PatternError> for super::Error {
    fn from(err: PatternError) -> Self {
        Box::new(err)
    }
}

impl From<PatternError> for super::Errors {
    fn from(err: PatternError) -> Self {
        vec![err.into()]
    }
}

impl<T> From<PatternError> for Result<T, super::Errors> {
    fn from(err: PatternError) -> Self {
        Err(err.into())
    }
}
