use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;

use anyhow::anyhow;
use crate::parser::Span;

use crate::syntax::{
    abstraction::Abstraction,
    pattern::Pattern,
    r#type::Type,
    term::{Infix, Prefix, Term},
};

use super::value::Value;

fn eval_infix(left: Value, op: &Infix, right: Value) -> anyhow::Result<Value> {
    match (left, op, right) {
        (Value::Bool(left), Infix::Or, Value::Bool(right)) => Ok(Value::Bool(left || right)),
        (Value::Bool(left), Infix::And, Value::Bool(right)) => Ok(Value::Bool(left && right)),
        (left, Infix::Eq, right) => Ok(Value::Bool(left == right)),
        (left, Infix::NtEq, right) => Ok(Value::Bool(left != right)),
        (Value::Int(left), Infix::GtEq, Value::Int(right)) => Ok(Value::Bool(left >= right)),
        (Value::Int(left), Infix::Gt, Value::Int(right)) => Ok(Value::Bool(left > right)),
        (Value::Int(left), Infix::LtEq, Value::Int(right)) => Ok(Value::Bool(left <= right)),
        (Value::Int(left), Infix::Lt, Value::Int(right)) => Ok(Value::Bool(left < right)),
        (Value::Int(left), Infix::Add, Value::Int(right)) => Ok(Value::Int(left + right)),
        (Value::Int(left), Infix::Sub, Value::Int(right)) => Ok(Value::Int(left - right)),
        (Value::Int(left), Infix::Mul, Value::Int(right)) => Ok(Value::Int(left * right)),
        (Value::Int(left), Infix::Div, Value::Int(right)) => Ok(Value::Int(left / right)),
        _ => unimplemented!(),
    }
}

fn eval_prefix(op: &Prefix, right: Value) -> anyhow::Result<Value> {
    match (op, right) {
        (Prefix::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
        (Prefix::Not, Value::Variant { label, value: _ }) => Ok(Value::Variant {
            label: if label == "true" { "false" } else { "true" }.to_owned(),
            value: Box::new(Value::Tuple(Vec::new())),
        }),
        _ => unimplemented!(),
    }
}

pub fn eval_parallel(mut env: Environment, body: Arc<Term>) -> anyhow::Result<Value> {
    let handle = std::thread::spawn(move || env.eval(&body));
    handle.join().unwrap()
}

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn eval(&mut self, term: &Term) -> anyhow::Result<Value> {
        match term {
            Term::Abstraction(abs, _) => Ok(Value::Abstraction(abs.clone(), self.clone())),
            Term::Application(abs, arg, _) => {
                let arg = self.eval(arg)?;
                let Value::Abstraction(
                    Abstraction {
                        param,
                        param_type: _,
                        body,
                    },
                    env,
                ) = self.eval(abs)?
                else {
                    return Err(anyhow!(
                        "Application expected abstraction in first position."
                    ));
                };
                let mut env = env.clone();
                env.bind_pattern(&param, arg)?;
                env.eval(&body.clone())
            }
            Term::Ascription(term, _, _) => self.eval(term),
            Term::Bool(b, _) => Ok(Value::Bool(*b)),
            Term::Box(body, _) => self.eval(body),
            // Fix and MFix
            Term::Fix(abs, span) | Term::MFix(abs, span) => {
                let arg = Term::Abstraction(Abstraction {
                    param: Pattern::Variable(span.clone(), "v".to_owned()),
                    param_type: Type::Tuple(span.clone(), vec![]),
                    body: Arc::new(Term::Application(
                        Arc::new(term.clone()),
                        Arc::new(Term::Variable("v".into(), Span::default())),
                        span.clone(),
                    ))
                }, Span::default());
                let fixed = Term::Application(abs.clone(), Arc::new(arg), span.clone());
                self.eval(&fixed)
            }
            Term::If(guard, if_true, if_false, _) => {
                let Value::Bool(guard) = self.eval(guard)? else {
                    return Err(anyhow!("If expected boolean in condition!"));
                };
                if guard {
                    self.eval(if_true)
                } else {
                    self.eval(if_false)
                }
            }
            Term::Infix(left, op, right, _) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;
                eval_infix(left, op, right)
            }
            Term::Int(i, _) => Ok(Value::Int(*i)),
            Term::Let(pattern, value, body, _) => {
                let value = self.eval(value)?;
                let mut env = self.clone();
                env.bind_pattern(pattern, value)?;
                env.eval(body)
            }
            Term::LetBox(pattern, value, body, _) => {
                let value = self.eval(value)?;
                let mut env = self.clone();
                env.bind_pattern(pattern, value)?;

                let cloned_body = body.clone();

                match cloned_body.as_ref() {
                    Term::Prefix(op, right, _) => {
                        let right = eval_parallel(env, right.clone())?;
                        eval_prefix(&op, right)
                    }
                    Term::Infix(left, op, right, _) => {
                        let left = eval_parallel(env.clone() , left.clone())?;
                        let right = eval_parallel(env, right.clone())?;
                        eval_infix(left, &op, right)
                    }
                    Term::Int(i, _) => Ok(Value::Int(*i)),
                    Term::Bool(b, _) => Ok(Value::Bool(*b)),
                    Term::Unit(_) => Ok(Value::Unit),
                    _ => eval_parallel(env, cloned_body)
                }
            }
            Term::Match(value, arms, _) => {
                let Value::Variant { label, value } = self.eval(value)? else {
                    return Err(anyhow!("Match expected variant"));
                };
                let Some((pattern, body)) = arms.get(&label) else {
                    return Err(anyhow!("Field {label} not in match arms"));
                };

                let mut env = self.clone();
                env.bind_pattern(pattern, *value)?;
                env.eval(body)
            }
            Term::Postfix(_, _, _) => unimplemented!(),
            Term::Prefix(op, right, _) => {
                let right = self.eval(right)?;
                eval_prefix(op, right)
            }
            Term::Tuple(values, _) => self.eval_vec_terms(values, Value::Tuple),
            Term::Variable(name, _) => self.get(name).cloned(),
            Term::Variant(label, value, _) => {
                let value = self.eval(value)?;
                Ok(Value::to_variant(label, value))
            }
            Term::Unit(_) => Ok(Value::Unit),
        }
    }

    pub fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> anyhow::Result<()> {
        match (pattern, value) {
            (Pattern::Tuple(_, patterns), Value::Tuple(values)) => {
                if patterns.len() != patterns.len() {
                    return Err(anyhow!("Tuple pattern insufficient."));
                }

                for (pattern, value) in patterns.iter().zip(values.into_iter()) {
                    self.bind_pattern(pattern, value)?;
                }
            }
            (Pattern::Tuple(_, _), _) => {
                return Err(anyhow!("Tuple pattern incompatible with value."));
            }
            (Pattern::Variable(_, name), value) => {
                self.insert(name.clone(), value);
            }
            (Pattern::Wildcard(_), _) => {}
            _ => {}
        }
        Ok(())
    }

    pub fn get(&mut self, name: &str) -> anyhow::Result<&Value> {
        self.values.get(name).ok_or(anyhow!("Variable {name} not in environment!"))
    }

    pub fn insert(&mut self, k: String, v: Value) {
        self.values.insert(k, v);
    }

    fn eval_vec_terms(
        &mut self,
        values: &[Term],
        value_type: impl FnOnce(Vec<Value>) -> Value,
    ) -> anyhow::Result<Value> {
        values
            .iter()
            .map(|value| self.eval(&value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(value_type)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::from([
                ("true".to_owned(), Value::Bool(true)),
                ("false".to_owned(), Value::Bool(false)),
            ]),
        }
    }
}
