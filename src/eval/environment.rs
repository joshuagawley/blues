use std::collections::HashMap;
use std::sync::Arc;

use crate::parser::Span;
use anyhow::anyhow;
use rayon::ThreadPool;
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
        (Prefix::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
        _ => unimplemented!(),
    }
}

pub fn eval_parallel(
    thread_pool: &ThreadPool,
    mut env: Environment,
    body: Arc<Term>,
) -> anyhow::Result<Value> {
    match body.as_ref() {
        Term::Int(i, _) => Ok(Value::Int(*i)),
        Term::Bool(b, _) => Ok(Value::Bool(*b)),
        Term::Unit(_) => Ok(Value::Unit),
        _ => thread_pool.install(|| env.eval(thread_pool, &body)),
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn eval(&mut self, thread_pool: &ThreadPool, term: &Term) -> anyhow::Result<Value> {
        match term {
            Term::Abstraction(abs, _) => Ok(Value::Abstraction(abs.clone(), self.clone())),
            Term::Application(abs, arg, _) => {
                let arg = self.eval(thread_pool, arg)?;
                let Value::Abstraction(
                    Abstraction {
                        param,
                        param_type: _,
                        body,
                    },
                    mut env,
                ) = self.eval(thread_pool, abs)?
                else {
                    return Err(anyhow!(
                        "Application expected abstraction in first position."
                    ));
                };

                env.bind_and_eval(thread_pool, &param, arg, &body.clone())
            }
            Term::Ascription(term, _, _) => self.eval(thread_pool, term),
            Term::Bool(b, _) => Ok(Value::Bool(*b)),
            Term::Box(body, _) => self.eval(thread_pool, body),
            // Fix and MFix
            Term::Fix(abs, span) | Term::MFix(abs, span) => {
                let arg = Term::Abstraction(
                    Abstraction {
                        param: Pattern::Variable(span.clone(), "v".to_owned()),
                        param_type: Type::Tuple(span.clone(), vec![]),
                        body: Arc::new(Term::Application(
                            Arc::new(term.clone()),
                            Arc::new(Term::Variable("v".into(), Span::default())),
                            span.clone(),
                        )),
                    },
                    Span::default(),
                );
                let fixed = Term::Application(abs.clone(), Arc::new(arg), span.clone());
                self.eval(thread_pool, &fixed)
            }
            Term::If(guard, if_true, if_false, _) => {
                let Value::Bool(guard) = self.eval(thread_pool, guard)? else {
                    return Err(anyhow!("If expected boolean in condition!"));
                };
                if guard {
                    self.eval(thread_pool, if_true)
                } else {
                    self.eval(thread_pool, if_false)
                }
            }
            Term::Infix(left, op, right, _) => {
                let left = self.eval(thread_pool, left)?;
                let right = self.eval(thread_pool, right)?;
                eval_infix(left, op, right)
            }
            Term::Int(i, _) => Ok(Value::Int(*i)),
            Term::Let(pattern, value, body, _) => {
                let value = self.eval(thread_pool, value)?;
                self.bind_and_eval(thread_pool, pattern, value, body)
            }
            Term::LetBox(pattern, value, body, _) => {
                let value = self.eval(thread_pool, value)?;
                let mut env = self.clone();
                env.bind_pattern(pattern, value)?;

                eval_parallel(thread_pool, env, body.clone())
            }
            Term::Match(value, arms, _) => {
                let Value::Variant { label, value } = self.eval(thread_pool, value)? else {
                    return Err(anyhow!("Match expected variant"));
                };
                let Some((pattern, body)) = arms.get(&label) else {
                    return Err(anyhow!("Field {label} not in match arms"));
                };

                self.bind_and_eval(thread_pool, pattern, *value, body)
            }
            Term::Postfix(_, _, _) => unimplemented!(),
            Term::Prefix(op, right, _) => {
                let right = self.eval(thread_pool, right)?;
                eval_prefix(op, right)
            }
            Term::Tuple(values, _) => self.eval_vec_terms(thread_pool, values, Value::Tuple),
            Term::TupleProjection(tuple, index, _) => self.eval_tuple_proj(thread_pool, tuple, *index),
            Term::Variable(name, _) => self.get(name).cloned(),
            Term::Variant(label, value, _) => {
                let value = self.eval(thread_pool, value)?;
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
        }
        Ok(())
    }

    pub fn get(&mut self, name: &str) -> anyhow::Result<&Value> {
        self.values
            .get(name)
            .ok_or(anyhow!("Variable {name} not in environment!"))
    }

    pub fn insert(&mut self, k: String, v: Value) {
        self.values.insert(k, v);
    }

    fn bind_and_eval(
        &mut self,
        thread_pool: &ThreadPool,
        pattern: &Pattern,
        value: Value,
        term: &Term,
    ) -> anyhow::Result<Value> {
        let mut env = self.clone();
        env.bind_pattern(pattern, value)?;
        env.eval(thread_pool, term)
    }

    fn eval_vec_terms(
        &mut self,
        thread_pool: &ThreadPool,
        values: &[Term],
        value_type: impl FnOnce(Vec<Value>) -> Value,
    ) -> anyhow::Result<Value> {
        values
            .iter()
            .map(|value| self.eval(thread_pool, &value.clone()))
            .collect::<Result<Vec<_>, _>>()
            .map(value_type)
    }
    
    fn eval_tuple_proj(&mut self, thread_pool: &ThreadPool, tuple: &Term, index: usize) -> anyhow::Result<Value> {
        let Value::Tuple(values) = self.eval(thread_pool, tuple)? else {
            return Err(anyhow!("Projection expected tuple but got {tuple:?}!"));
        };
        
        values.get(index).cloned().ok_or(anyhow!("Index {index} not in tuple!"))
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
