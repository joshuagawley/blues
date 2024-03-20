use super::{pattern::Pattern, r#type::Type, term::Term};

pub struct Program(pub Vec<Declaration>);

#[derive(Clone, Debug)]
pub enum Declaration {
    Term(Pattern, Term),
    Type(String, Type),
}
