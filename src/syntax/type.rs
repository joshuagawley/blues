use crate::error::type_error::TypeError;
use crate::error::Errors;
use crate::parser::Span;
use ariadne::Span as AriadneSpan;
use core::fmt::Display;
use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub enum Type {
    Abstraction(Span, Box<Type>, Box<Type>),
    Bool(Span),
    Int(Span),
    Tuple(Span, Vec<Type>),
    Variable(Span, String),
    // IndexMap preserves the order in which (k,v) pairs are inserted
    Variant(Span, IndexMap<String, Type>),
    Unit(Span),
    Mobile(Span, Box<Type>),
}

impl Type {
    pub fn make_error_abs() -> Self {
        Self::Abstraction(
            Span::default(),
            Box::new(Type::Variable(Span::default(), "*".to_owned())),
            Box::new(Type::Variable(Span::default(), "*".to_owned())),
        )
    }
    pub fn span(&self) -> &Span {
        match self {
            Self::Abstraction(span, _, _) => span,
            Self::Bool(span) => span,
            Self::Int(span) => span,
            Self::Tuple(span, _) => span,
            Self::Variable(span, _) => span,
            Self::Variant(span, _) => span,
            Self::Unit(span) => span,
            Self::Mobile(span, _) => span,
        }
    }

    pub fn get_inner_type(&self) -> &Type {
        match self {
            Type::Mobile(_, inner) => inner.get_inner_type(),
            _ => self,
        }
    }
    pub fn is_bool(&self) -> bool {
        matches!(self.get_inner_type(), Type::Bool(..))
    }

    pub fn is_int(&self) -> bool {
        matches!(self.get_inner_type(), Type::Int(..))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.get_inner_type(), Self::Tuple(..))
    }

    pub fn unroll_abs(self) -> Result<(Box<Type>, Box<Type>), Errors> {
        let Self::Abstraction(_, param_type, return_type) = self else {
            return Err(vec![TypeError::Mismatch {
                offset: self.span().start(),
                span: self.span().clone(),
                expected: Self::make_error_abs(),
                actual: self,
            }
            .into()]);
        };
        Ok((param_type, return_type))
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Abstraction(_, l1, l2), Self::Abstraction(_, r1, r2)) => l1 == r1 && l2 == r2,
            (Self::Bool(_), Self::Bool(_)) => true,
            (Self::Int(_), Self::Int(_)) => true,
            (Self::Unit(_), Self::Unit(_)) => true,
            (Self::Tuple(_, l1), Self::Tuple(_, r1)) => l1 == r1,
            (Self::Variable(_, l1), Self::Variable(_, r1)) => l1 == r1,
            (Self::Variant(_, l1), Self::Variant(_, r1)) => l1 == r1,
            (Self::Mobile(_, l1), Self::Mobile(_, r1)) => l1 == r1,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Type::Abstraction(_, param_type, return_type) => {
                match **param_type {
                    Type::Abstraction(_, _, _) => write!(f, "({param_type})")?,
                    _ => write!(f, "{param_type}")?,
                }
                write!(f, " -> {return_type}")
            }
            Type::Bool(_) => write!(f, "Bool"),
            Type::Int(_) => write!(f, "Int"),
            Type::Unit(_) => write!(f, "Unit"),
            Type::Tuple(_, types) => {
                let types = types.iter().map(Type::to_string).collect::<Vec<_>>();
                write!(f, "({})", types.join(", "))
            }
            Type::Variable(_, name) => write!(f, "{name}"),
            Type::Variant(_, variants) => {
                let variants = variants
                    .iter()
                    .map(|(label, variant_type)| format!("{label}: {variant_type}"))
                    .collect::<Vec<_>>();
                write!(f, "<{}>", variants.join(", "))
            }
            Type::Mobile(_, inner_type) => match **inner_type {
                Type::Abstraction(_, _, _) => write!(f, "[]({inner_type})"),
                _ => write!(f, "[]{inner_type}"),
            },
        }
    }
}
