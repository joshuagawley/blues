use super::{pattern::Pattern, r#type::Type, term::Term};
use core::fmt::Display;

#[derive(Clone, Debug)]
pub struct Abstraction {
    pub param: Pattern,
    pub param_type: Type,
    pub body: Box<Term>,
}

impl Display for Abstraction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let Abstraction {
            param,
            param_type,
            body,
        } = self;
        write!(f, "{param}: {param_type} => {body}")
    }
}
