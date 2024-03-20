use thiserror::Error;

use crate::syntax::{pattern::Pattern, r#type::Type};

#[derive(Error, Debug)]
pub enum PatternError {
    #[error("Expected {expected}, got {actual}")]
    Incompatible { actual: Pattern, expected: Type },

    #[error("Expected {expected}, got {actual}")]
    MissingElements { actual: usize, expected: usize },
}
