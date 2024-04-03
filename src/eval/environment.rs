use std::collections::HashMap;

use anyhow::anyhow;

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

pub fn eval_parallel(mut env: Environment, body: Box<Term>) -> anyhow::Result<Value> {
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
            Term::Abstraction(abs) => Ok(Value::Abstraction(abs.clone(), self.clone())),
            Term::Application(abs, arg) => {
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
            Term::Ascription(term, _) => self.eval(term),
            Term::Bool(b) => Ok(Value::Bool(*b)),
            Term::Box(body) => eval_parallel(self.clone(), body.clone()),
            Term::Fix(abs) => {
                let arg = Term::Abstraction(Abstraction {
                    param: Pattern::Variable("v".to_owned()),
                    param_type: Type::Tuple(Vec::new()),
                    body: Box::new(Term::Application(
                        Box::new(term.clone()),
                        Box::new(Term::Variable("v".into())),
                    )),
                });
                let fixed = Term::Application(abs.clone(), Box::new(arg));
                self.eval(&fixed)
            }
            Term::MFix(abs) => {
                let arg = Term::Abstraction(Abstraction {
                    param: Pattern::Variable("v".to_owned()),
                    param_type: Type::Tuple(Vec::new()),
                    body: Box::new(Term::Application(
                        Box::new(term.clone()),
                        Box::new(Term::Variable("v".into())),
                    )),
                });
                let fixed = Term::Application(abs.clone(), Box::new(arg));
                self.eval(&fixed)
            }
            Term::If(guard, if_true, if_false) => {
                let Value::Bool(guard) = self.eval(guard)? else {
                    return Err(anyhow!("If expected boolean in condition!"));
                };
                if guard {
                    self.eval(if_true)
                } else {
                    self.eval(if_false)
                }
            }
            Term::Infix(left, op, right) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;
                eval_infix(left, op, right)
            }
            Term::Int(i) => Ok(Value::Int(*i)),
            Term::Let(pattern, value, body) => {
                let value = self.eval(value)?;
                let mut env = self.clone();
                env.bind_pattern(pattern, value)?;
                env.eval(body)
            }
            Term::LetBox(pattern, value, body) => {
                let value = self.eval(value)?;
                let mut env = self.clone();
                env.bind_pattern(pattern, value)?;
                eval_parallel(env, body.clone())
            }
            Term::List(values) => self.eval_vec_terms(values, Value::List),
            Term::Match(value, arms) => {
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
            Term::Postfix(_, _) => unimplemented!(),
            Term::Prefix(op, right) => {
                let right = self.eval(right)?;
                eval_prefix(op, right)
            }
            Term::Tuple(values) => self.eval_vec_terms(values, Value::Tuple),
            Term::Variable(name) => self.get(name).cloned(),
            Term::Variant(label, value) => {
                let value = self.eval(value)?;
                Ok(Value::to_variant(label, value))
            }
            Term::Unit => Ok(Value::Unit),
        }
    }

    pub fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> anyhow::Result<()> {
        match (pattern, value) {
            (Pattern::Tuple(patterns), Value::Tuple(values)) => {
                if patterns.len() != patterns.len() {
                    return Err(anyhow!("Tuple pattern insufficient."));
                }

                for (pattern, value) in patterns.iter().zip(values.into_iter()) {
                    self.bind_pattern(pattern, value)?;
                }
            }
            (Pattern::Tuple(_), _) => {
                return Err(anyhow!("Tuple pattern incompatible with value.".to_owned()))
            }
            (Pattern::Variable(name), value) => {
                self.insert(name.clone(), value);
            }
            (Pattern::Wildcard, _) => {}
            _ => {}
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> anyhow::Result<&Value> {
        self.values
            .get(name)
            .ok_or(anyhow!("Variable {name} not in environment!"))
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
