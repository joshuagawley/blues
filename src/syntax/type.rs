use core::fmt::Display;
use indexmap::IndexMap;
use crate::error::Errors;
use crate::error::type_error::TypeError;

#[derive(Clone, Debug)]
pub enum Type {
    Abstraction(Box<Type>, Box<Type>),
    Bool,
    Int,
    Tuple(Vec<Type>),
    Variable(String),
    // IndexMap preserves the order in which (k,v) pairs are inserted
    Variant(IndexMap<String, Type>),
    Unit,
    Modal(Box<Type>),
}

impl Type {
    
    pub fn get_inner_type(&self) -> &Type {
        match self {
            Type::Modal(inner) => {
                inner.get_inner_type()
            }
            _ => self
        }
    }
    pub fn is_bool(&self) -> bool {
        self == &Self::Bool
    }

    pub fn is_int(&self) -> bool {
        self.get_inner_type() == &Self::Int
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.get_inner_type(), Self::Tuple(..))
    }
    
    pub fn unroll_abs(self) -> Result<(Box<Type>, Box<Type>), Errors> {
        let Self::Abstraction(param_type, return_type) = self else {
            return Err(vec![TypeError::Mismatch {
                expected: Type::Abstraction(
                    Box::new(Type::Variable("*".to_owned())),
                    Box::new(Type::Variable("*".to_owned())),
                ),
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
            (Self::Abstraction(l1, l2), Self::Abstraction(r1, r2)) => l1 == l2 && r1 == r2,
            (Self::Bool, Self::Bool) => true,
            (Self::Int, Self::Int) => true,
            (Self::Unit, Self::Unit) => true,
            (Self::Tuple(l1), Self::Tuple(r1)) => l1 == r1,
            (Self::Variable(l1), Self::Variable(r1)) => l1 == r1,
            (Self::Variant(l1), Self::Variant(r1)) => l1 == r1,
            (Self::Modal(l1), Self::Modal(r1)) => l1 == r1,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Type::Abstraction(param_type, return_type) => {
                match **param_type {
                    Type::Abstraction(_, _) => write!(f, "({param_type})")?,
                    _ => write!(f, "{param_type}")?,
                }
                write!(f, " -> {return_type}")
            }
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Unit => write!(f, "Unit"),
            Type::Tuple(types) => {
                let types = types.iter().map(Type::to_string).collect::<Vec<_>>();
                write!(f, "({})", types.join(", "))
            }
            Type::Variable(name) => write!(f, "{name}"),
            Type::Variant(variants) => {
                let variants = variants
                    .iter()
                    .map(|(label, variant_type)| format!("{label}: {variant_type}"))
                    .collect::<Vec<_>>();
                write!(f, "<{}>", variants.join(", "))
            }
            Type::Modal(inner_type) => match **inner_type {
                Type::Abstraction(_, _) => write!(f, "[]({inner_type})"),
                _ => write!(f, "[]{inner_type}"),
            },
        }
    }
}
