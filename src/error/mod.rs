use crate::parser::Span;
use crate::syntax::r#type::Type;
use ariadne::Report;
use core::fmt::Debug;

pub mod pattern_error;
pub mod type_error;

pub type Error = Box<dyn Reportable>;
pub type Errors = Vec<Error>;

pub type MaybeType = Result<Type, Errors>;

pub trait Reportable: Debug {
    fn build_report(&self) -> Report<Span>;
    fn offset(&self) -> usize;
    fn span(&self) -> &Span;
}
