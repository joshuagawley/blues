use core::fmt::Display;

#[derive(Clone, Debug)]
pub enum Pattern {
    Box(String),
    Tuple(Vec<Pattern>),
    Variable(String),
    Wildcard,
}

impl Display for Pattern {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Pattern::Box(name) => write!(f, "box {name}"),
            Pattern::Tuple(patterns) => {
                let patterns = patterns.iter().map(Pattern::to_string).collect::<Vec<_>>();
                write!(f, "({})", patterns.join(", "))
            }
            Pattern::Variable(name) => write!(f, "{name}"),
            Pattern::Wildcard => write!(f, "_"),
        }
    }
}
