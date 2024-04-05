use core::fmt::Display;
use std::collections::HashMap;

use super::{abstraction::Abstraction, pattern::Pattern, r#type::Type};

#[derive(Clone, Debug)]
pub enum Term {
    Abstraction(Abstraction),
    Application(Box<Term>, Box<Term>),
    Ascription(Box<Term>, Type),
    Bool(bool),
    Box(Box<Term>),
    Fix(Box<Term>),
    MFix(Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Int(i64),
    Infix(Box<Term>, Infix, Box<Term>),
    Let(Pattern, Box<Term>, Box<Term>),
    LetBox(Pattern, Box<Term>, Box<Term>),
    List(Vec<Term>),
    Match(Box<Term>, HashMap<String, (Pattern, Term)>),
    Postfix(Box<Term>, Postfix),
    Prefix(Prefix, Box<Term>),
    Tuple(Vec<Term>),
    Unit,
    Variable(String),
    Variant(String, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Term::Abstraction(abstraction) => write!(f, "{abstraction}"),
            Term::Application(abstraction, argument) => write!(f, "{abstraction} {argument}"),
            Term::Ascription(value, as_type) => write!(f, "{value} as {as_type}"),
            Term::Box(value) => write!(f, "box {value}"),
            Term::Bool(b) => write!(f, "{b}"),
            Term::Fix(value) => write!(f, "fix {value}"),
            Term::MFix(value) => write!(f, "mfix {value}"),
            Term::If(guard, if_true, if_false) => {
                write!(f, "if {guard} then {if_true} else {if_false}")
            }
            Term::Int(n) => write!(f, "{n}"),
            Term::Infix(lhs, op, rhs) => write!(f, "{lhs} {op} {rhs}"),
            Term::Let(pattern, value, body) => write!(f, "let {pattern} = {value} in {body}"),
            Term::LetBox(pattern, value, body) => {
                write!(f, "let box {pattern} <= {value} in {body}")
            }
            Term::List(values) => {
                let values = values.iter().map(Term::to_string).collect::<Vec<_>>();
                write!(f, "[{}]", values.join(", "))
            }
            Term::Match(value, arms) => {
                let arms = arms
                    .iter()
                    .map(|(tag, (pattern, body))| format!("<{tag}={pattern}> => {body}"))
                    .collect::<Vec<_>>();
                write!(f, "match {value} with {}", arms.join(", "))
            }
            Term::Postfix(left, op) => write!(f, "{left}{op}"),
            Term::Prefix(op, right) => write!(f, "{op}{right}"),
            Term::Tuple(values) => {
                let values = values.iter().map(Term::to_string).collect::<Vec<_>>();
                write!(f, "[{}]", values.join(", "))
            }
            Term::Unit => write!(f, "Unit"),
            Term::Variable(name) => write!(f, "{name}"),
            Term::Variant(field, value) => write!(f, "<{field} = {value}>"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Infix {
    Or,
    And,
    Eq,
    NtEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Infix {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Infix::Or => write!(f, "||"),
            Infix::And => write!(f, "&&"),
            Infix::Eq => write!(f, "=="),
            Infix::NtEq => write!(f, "!="),
            Infix::Gt => write!(f, ">="),
            Infix::GtEq => write!(f, ">"),
            Infix::Lt => write!(f, "<="),
            Infix::LtEq => write!(f, "<"),
            Infix::Add => write!(f, "+"),
            Infix::Sub => write!(f, "-"),
            Infix::Mul => write!(f, "*"),
            Infix::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Postfix {}

impl Display for Postfix {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "")
    }
}

unsafe impl Sync for Postfix {}

#[derive(Clone, Debug)]
pub enum Prefix {
    Neg,
    Not,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Prefix::Neg => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}
