use thiserror::Error;

use crate::syntax::r#type::Type;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Mismatched types: expected {expected}, got {actual}")]
    Mismatch { expected: Type, actual: Type },
    #[error("Missing variants: ")]
    MissingVariants(Vec<(String, Type)>),
    #[error("Cannot find variable `{0}` in this scope")]
    UndefinedVariable(String),
    #[error("No field `{0}` in record `{1}`")]
    UnknownField(String, Type),
    #[error("No index `{0}` in tuple `{1}`")]
    UnknownIndex(usize, Type),
    #[error("Cannot find type `{0}` in this scope")]
    UnknownType(String),
}
