use crate::parser::Span;
use core::fmt::Display;

#[derive(Clone, Debug)]
pub enum Pattern {
    Tuple(Span, Vec<Pattern>),
    Variable(Span, String),
    Wildcard(Span),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Pattern::Tuple(_, patterns) => {
                let patterns = patterns.iter().map(Pattern::to_string).collect::<Vec<_>>();
                write!(f, "({})", patterns.join(", "))
            }
            Pattern::Variable(_, name) => write!(f, "{name}"),
            Pattern::Wildcard(_) => write!(f, "_"),
        }
    }
}
