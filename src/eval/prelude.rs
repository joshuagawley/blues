use std::vec;

use crate::syntax::r#type::Type;

use super::{context::Context, environment::Environment, value::Value};

#[derive(Clone, Debug)]
pub struct Prelude {
    types: Vec<(String, Type)>,
    context: Vec<(String, Type)>,
    env: Vec<(String, Value)>,
}

impl Prelude {
    pub fn add_prelude_to(self, context: &mut Context, env: &mut Environment) {
        self.types
            .into_iter()
            .for_each(|(name, r#type)| context.insert_type(name, r#type));
        self.context
            .into_iter()
            .for_each(|(name, r#type)| context.insert(name, r#type));
        self.env
            .into_iter()
            .for_each(|(name, value)| env.insert(name, value));
    }
}

impl Default for Prelude {
    fn default() -> Self {
        Self {
            types: make_type_prelude(),
            context: make_context_prelude(),
            env: make_env_prelude(),
        }
    }
}

fn make_type_prelude() -> Vec<(String, Type)> {
    vec![
        ("Bool".to_owned(), Type::Bool),
        ("Int".to_owned(), Type::Int),
    ]
}

fn make_context_prelude() -> Vec<(String, Type)> {
    vec![
        ("true".to_owned(), Type::Variable("Bool".to_owned())),
        ("false".to_owned(), Type::Variable("Bool".to_owned())),
    ]
}

fn make_env_prelude() -> Vec<(String, Value)> {
    vec![]
}
