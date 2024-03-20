use core::fmt::Display;

use crate::syntax::abstraction::Abstraction;

use super::environment::Environment;

#[derive(Clone, Debug)]
pub enum Value {
    Abstraction(Abstraction, Environment),
    Bool(bool),
    Int(i64),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Unit,
    Variant { label: String, value: Box<Value> },
}

impl Value {
    pub fn to_variant(label: &str, value: Value) -> Self {
        Self::Variant {
            label: label.to_owned(),
            value: Box::new(value),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (
                Self::Variant {
                    label: l_label,
                    value: l_value,
                },
                Self::Variant {
                    label: r_label,
                    value: r_value,
                },
            ) => l_label == r_label && l_value == r_value,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Value::Abstraction(abstraction, _) => write!(f, "{abstraction}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Int(n) => write!(f, "{n}"),
            Value::List(values) => {
                let values = values.iter().map(Value::to_string).collect::<Vec<_>>();
                write!(f, "[{}]", values.join(", "))
            }
            Value::Tuple(values) => {
                let values = values.iter().map(Value::to_string).collect::<Vec<_>>();
                write!(f, "[{}]", values.join(", "))
            }
            Value::Unit => write!(f, "Unit"),
            Value::Variant { label, value } => write!(f, "<{label} = {value}>"),
        }
    }
}
