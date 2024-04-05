use crate::syntax::r#type::Type;

pub mod pattern_error;
pub mod type_error;

pub type Errors = Vec<anyhow::Error>;

pub type MaybeType = Result<Type, Errors>;
