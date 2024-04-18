use crate::parser::Span;
use core::fmt::Display;
use std::collections::HashMap;
use std::sync::Arc;

use super::{abstraction::Abstraction, pattern::Pattern, r#type::Type};

#[derive(Clone, Debug)]
pub enum Term {
    Abstraction(Abstraction, Span),
    Application(Arc<Term>, Arc<Term>, Span),
    Ascription(Arc<Term>, Type, Span),
    Bool(bool, Span),
    Box(Arc<Term>, Span),
    Fix(Arc<Term>, Span),
    MFix(Arc<Term>, Span),
    If(Arc<Term>, Arc<Term>, Arc<Term>, Span),
    Int(i64, Span),
    Infix(Arc<Term>, Infix, Arc<Term>, Span),
    Let(Pattern, Arc<Term>, Arc<Term>, Span),
    LetBox(Pattern, Arc<Term>, Arc<Term>, Span),
    Match(Arc<Term>, HashMap<String, (Pattern, Term)>, Span),
    #[allow(dead_code)]
    Postfix(Arc<Term>, Postfix, Span),
    Prefix(Prefix, Arc<Term>, Span),
    Tuple(Vec<Term>, Span),
    Unit(Span),
    Variable(String, Span),
    Variant(String, Arc<Term>, Span),
}

impl Term {
    pub fn span(&self) -> &Span {
        match self {
            Term::Abstraction(_, span) => span,
            Term::Application(_, _, span) => span,
            Term::Ascription(_, _, span) => span,
            Term::Bool(_, span) => span,
            Term::Box(_, span) => span,
            Term::Fix(_, span) => span,
            Term::MFix(_, span) => span,
            Term::If(_, _, _, span) => span,
            Term::Int(_, span) => span,
            Term::Infix(_, _, _, span) => span,
            Term::Let(_, _, _, span) => span,
            Term::LetBox(_, _, _, span) => span,
            Term::Match(_, _, span) => span,
            Term::Postfix(_, _, span) => span,
            Term::Prefix(_, _, span) => span,
            Term::Tuple(_, span) => span,
            Term::Unit(span) => span,
            Term::Variable(_, span) => span,
            Term::Variant(_, _, span) => span,
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Term::Abstraction(abstraction, _) => write!(f, "{abstraction}"),
            Term::Application(abstraction, argument, _) => write!(f, "{abstraction} {argument}"),
            Term::Ascription(value, as_type, _) => write!(f, "{value} as {as_type}"),
            Term::Box(value, _) => write!(f, "box {value}"),
            Term::Bool(b, _) => write!(f, "{b}"),
            Term::Fix(value, _) => write!(f, "fix {value}"),
            Term::MFix(value, _) => write!(f, "mfix {value}"),
            Term::If(guard, if_true, if_false, _) => {
                write!(f, "if {guard} then {if_true} else {if_false}")
            }
            Term::Int(n, _) => write!(f, "{n}"),
            Term::Infix(lhs, op, rhs, _) => write!(f, "{lhs} {op} {rhs}"),
            Term::Let(pattern, value, body, _) => write!(f, "let {pattern} = {value} in {body}"),
            Term::LetBox(pattern, value, body, _) => {
                write!(f, "let box {pattern} = {value} in {body}")
            }
            Term::Match(value, arms, _) => {
                let arms = arms
                    .iter()
                    .map(|(tag, (pattern, body))| format!("<{tag}={pattern}> => {body}"))
                    .collect::<Vec<_>>();
                write!(f, "match {value} with {}", arms.join(", "))
            }
            Term::Postfix(left, op, _) => write!(f, "{left}{op}"),
            Term::Prefix(op, right, _) => write!(f, "{op}{right}"),
            Term::Tuple(values, _) => {
                let values = values.iter().map(Term::to_string).collect::<Vec<_>>();
                write!(f, "[{}]", values.join(", "))
            }
            Term::Unit(_) => write!(f, "Unit"),
            Term::Variable(name, _) => write!(f, "{name}"),
            Term::Variant(field, value, _) => write!(f, "<{field} = {value}>"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
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

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Postfix {}

impl Display for Postfix {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "")
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
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
