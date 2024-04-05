use std::collections::HashMap;
use std::hash::Hash;
use std::slice::RSplit;

use crate::{
    error::{pattern_error::PatternError, type_error::TypeError},
    syntax::{
        abstraction::Abstraction,
        pattern::Pattern,
        r#type::Type,
        term::{Infix, Prefix, Term},
    },
};

use anyhow::anyhow;
use indexmap::IndexMap;
use crate::error::{Errors, MaybeType};

#[derive(Clone, Default, Debug)]
pub struct Context {
    types: HashMap<String, Type>,
    bindings: HashMap<String, Type>,
    // local_values: HashMap<String, Type>,
    // modal_values: HashMap<String, Type>, // _context_type: PhantomData,
}

impl Context {
    // pub fn get_local(&self, name: &str) -> Option<&Type> {
    //     self.local_values.get(name)
    // }
    //
    // pub fn get_modal(&self, name: &str) -> Option<&Type> {
    //     self.modal_values.get(name)
    // }

    // pub fn insert_local(&mut self, name: String, r#type: Type) {
    //     if matches!(r#type, Type::Modal(..)) {
    //         panic!("{name} is modal, so cannot be put in local context!")
    //     }
    //     self.local_values.insert(name, r#type);
    // }
    //
    // pub fn insert_modal(&mut self, name: String, r#type: Type) {
    //     if !matches!(r#type, Type::Modal(..)) {
    //         panic!("{name} is not modal, it has local dependencies!")
    //     }
    //     self.modal_values.insert(name, r#type);
    // }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }

    pub fn insert(&mut self, name: String, r#type: Type) {
        self.bindings.insert(name, r#type);
    }

    pub fn insert_type(&mut self, name: String, r#type: Type) {
        self.types.insert(name, r#type);
    }

    fn type_of_infix(&mut self, left: &Term, op: &Infix, right: &Term) -> MaybeType {
        let left_type = self.resolve_type_of(left)?;
        let right_type = self.resolve_type_of(right)?;

        match op {
            Infix::Or | Infix::And => {
                let mut errors = Vec::new();
                if !left_type.is_bool() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Bool,
                            actual: left_type,
                        }
                        .into(),
                    );
                }
                if !right_type.is_bool() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Bool,
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Bool)
                } else {
                    Err(errors)
                }
            }

            Infix::Eq | Infix::NtEq => {
                if left_type == right_type {
                    Ok(Type::Bool)
                } else {
                    Err(vec![TypeError::Mismatch {
                        expected: left_type,
                        actual: right_type,
                    }
                    .into()])
                }
            }

            Infix::GtEq | Infix::Gt | Infix::LtEq | Infix::Lt => {
                let mut errors = Vec::new();
                if !left_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Int,
                            actual: left_type,
                        }
                        .into(),
                    );
                }
                if !right_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Int,
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Bool)
                } else {
                    Err(errors)
                }
            }

            Infix::Add | Infix::Sub | Infix::Mul | Infix::Div => {
                let mut errors = Vec::new();
                if !left_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Int,
                            actual: left_type,
                        }
                        .into(),
                    );
                }
                if !right_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            expected: Type::Int,
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Int)
                } else {
                    Err(errors)
                }
            }
        }
    }

    fn type_of_abstraction(
        &mut self,
        term: &Term,
        param: &Pattern,
        param_type: &Type,
        body: &Term,
    ) -> MaybeType {
        let mut context = self.clone();
        let param_type = self.resolve(param_type.clone())?;
        context.bind_pattern(param, term, &param_type)?;
        let body_type = context.resolve_type_of(body)?;
        Ok(Type::Abstraction(Box::new(param_type), Box::new(body_type)))
    }

    fn type_of_application(&mut self, abs: &Term, arg: &Term) -> MaybeType {
        let abs_type = self.resolve_type_of(abs)?;
        let arg_type = self.resolve_type_of(arg)?;

        match abs_type {
            Type::Abstraction(param_type, return_type) => {
                let param_type = self.resolve(*param_type)?;
                let return_type = self.resolve(*return_type)?;
                
                if param_type == Type::Modal(Box::new(return_type.clone())) {
                    Ok(return_type)
                }
                else if param_type == return_type {
                    Ok(return_type)
                } else {
                    Err(vec![TypeError::Mismatch {
                        expected: arg_type,
                        actual: param_type,
                    }
                    .into()])
                }
            }
            _ => Err(vec![TypeError::Mismatch {
                expected: Type::Abstraction(
                    Box::new(Type::Variable("*".to_owned())),
                    Box::new(Type::Variable("*".to_owned())),
                ),
                actual: abs_type,
            }
            .into()]),
        }
    }

    fn type_of_ascription(&mut self, value: &Term, as_type: &Type) -> MaybeType {
        let as_type = self.resolve(as_type.clone())?;
        let value_type = self.resolve_type_of(value)?;

        if value_type == as_type {
            return Ok(value_type);
        }

        match (&value_type, &as_type) {
            (Type::Variant(term_variants), Type::Variant(variants))
                if term_variants
                    .iter()
                    .all(|(label, r#type)| variants.get(label) == Some(r#type)) =>
            {
                Ok(as_type)
            }
            (Type::List(None), Type::List(Some(_))) => Ok(as_type),
            (_, _) if value_type == as_type => Ok(value_type),
            _ => Err(vec![TypeError::Mismatch {
                expected: value_type,
                actual: as_type,
            }
            .into()]),
        }
    }

    fn type_of_fix(&mut self, abs: &Term) -> MaybeType {
        // eprintln!("Getting type of local fixpoint");
        // eprintln!("Getting function type");
        // eprintln!("abs: {abs}");
        let (param_type, return_type) = self.resolve_type_of(abs)?.unroll_abs()?;
        // eprintln!("abs_type: {abs_type:#?}");

        let param_type = self.resolve(*param_type)?;
        let return_type = self.resolve(*return_type)?;

        if param_type == return_type {
            Ok(return_type)
        } else {
            Err(vec![TypeError::Mismatch {
                expected: param_type,
                actual: return_type,
            }
            .into()])
        }
    }

    fn type_of_mfix(&mut self, abs: &Term) -> MaybeType {
        // eprintln!("Getting type of mobile fixpoint");
        // eprintln!("Getting function type");
        // eprintln!("abs: {abs}");
        let abs_type = self.resolve_type_of(abs)?;
        // eprintln!("abs_type: {abs_type:#?}");
        let (param_type, return_type) = abs_type.unroll_abs()?;

        // eprintln!("Resolving parameter type");
        let param_type = self.resolve(*param_type)?;
        let return_type = self.resolve(*return_type)?;
        
        let (input_param_type, input_return_type) = param_type.unroll_abs()?;
        
        let (output_param_type, output_return_type) = return_type.unroll_abs()?;
        
        if input_return_type != output_return_type {
            return Err(vec![TypeError::Mismatch {
                expected: *input_return_type,
                actual: *output_return_type,
            }
                .into()]);
        }
        
        if input_param_type.get_inner_type() != &*output_param_type {
            return Err(vec![TypeError::Mismatch {
                expected: *input_param_type,
                actual: *output_param_type,
            }
                .into()]); 
        }
        
        Ok(Type::Abstraction(input_param_type, output_return_type))
    }

    fn type_of_if(
        &mut self,
        guard: &Term,
        if_true: &Term,
        if_false: &Term,
    ) -> MaybeType {
        let guard_type = self.resolve_type_of(guard)?;
        if !guard_type.is_bool() {
            return Err(vec![TypeError::Mismatch {
                expected: Type::Bool,
                actual: guard_type,
            }
            .into()]);
        }

        let if_true_type = self.resolve_type_of(if_true)?;
        let if_false_type = self.resolve_type_of(if_false)?;

        if if_true_type == if_false_type {
            Ok(if_true_type)
        } else {
            Err(vec![TypeError::Mismatch {
                expected: if_true_type,
                actual: if_false_type,
            }
            .into()])
        }
    }

    fn type_of_match(
        &mut self,
        term: &Term,
        value: &Term,
        arms: &HashMap<String, (Pattern, Term)>,
    ) -> MaybeType {
        let value_type = self.resolve_type_of(value)?;
        let Type::Variant(variants) = &value_type else {
            return Err(vec![TypeError::Mismatch {
                expected: Type::Variant(HashMap::from([(
                    "...".to_owned(),
                    Type::Variable("*".to_owned()),
                )])),
                actual: value_type,
            }
            .into()]);
        };

        let absent = variants
            .iter()
            .filter_map(|(label, variant_type)| {
                (!arms.contains_key(label)).then_some((label.clone(), variant_type.clone()))
            })
            .collect::<Vec<(String, Type)>>();

        let extraneous = arms
            .iter()
            .filter_map(|(label, _)| (!arms.contains_key(label)).then_some(label.clone()))
            .collect::<Vec<String>>();

        let mut errors = Vec::new();

        if !absent.is_empty() {
            errors.push(TypeError::MissingVariants(absent).into());
        }

        if !extraneous.is_empty() {
            todo!()
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let mut arm_types = IndexMap::new();
        for (label, (pattern, body)) in arms {
            let mut content = self.clone();
            let label_type = variants.get(label).unwrap().clone();
            if let Err(mut err) = content.bind_pattern(pattern, term, &label_type) {
                errors.append(&mut err);
            }
            match content.resolve_type_of(body) {
                Ok(arm_type) => {
                    arm_types.insert(label.clone(), arm_type);
                }
                Err(mut errs) => errors.append(&mut errs),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let match_type = arm_types.first().unwrap().1.clone();
        for (_, arm_type) in arm_types {
            if arm_type != match_type {
                errors.push(
                    TypeError::Mismatch {
                        expected: match_type.clone(),
                        actual: arm_type,
                    }
                    .into(),
                );
            }
        }

        if errors.is_empty() {
            Ok(match_type)
        } else {
            Err(errors)
        }
    }

    fn type_of_prefix(&mut self, op: &Prefix, right: &Term) -> MaybeType {
        let right_type = self.resolve_type_of(right)?;
        match (op, right_type.get_inner_type()) {
            (Prefix::Neg, Type::Int) => Ok(Type::Int),
            (Prefix::Neg, other_type) => Err(vec![TypeError::Mismatch {
                expected: Type::Int,
                actual: other_type.clone(),
            }
            .into()]),
            (Prefix::Not, Type::Bool) => Ok(Type::Bool),
            (Prefix::Not, other_type) => Err(vec![TypeError::Mismatch {
                expected: Type::Bool,
                actual: other_type.clone(),
            }
            .into()]),
        }
    }

    fn check_local_deps(&self, term: &Term) -> bool {
        // eprintln!("checking local deps of {term}");
        match term {
            Term::Variable(name) => {
                let r#type = self.get(name).unwrap();
                matches!(r#type, Type::Int | Type::Bool | Type::Modal(..))
            }

            Term::Abstraction(Abstraction {
                param,
                param_type,
                body,
            }) => matches!(param_type, Type::Modal(..)) && self.check_local_deps(body),
            Term::Application(abs, arg) => {
                // eprintln!("abs: {abs}");
                // eprintln!("arg: {arg}");
                self.check_local_deps(arg)
            },
            Term::Ascription(term, _) => self.check_local_deps(term),
            Term::Bool(..) => true,
            Term::Box(term) => self.check_local_deps(term),
            Term::Fix(..) => false,
            Term::MFix(term) => self.check_local_deps(term),
            Term::If(guard, if_true, if_false) => {
                self.check_local_deps(guard)
                    && self.check_local_deps(if_true)
                    && self.check_local_deps(if_false)
            }
            Term::Int(..) => true,
            Term::Infix(left, _, right) => {
                self.check_local_deps(left) && self.check_local_deps(right)
            }
            Term::Let(..) => false,
            Term::LetBox(_, value, body) => {
                self.check_local_deps(value) && self.check_local_deps(body)
            }
            Term::List(terms) => terms.iter().all(|term| self.check_local_deps(term)),
            Term::Match(value, arms) => {
                self.check_local_deps(value)
                    && arms.values().all(|(_, term)| self.check_local_deps(term))
            }
            Term::Postfix(term, _) => self.check_local_deps(term),
            Term::Prefix(_, term) => self.check_local_deps(term),
            Term::Tuple(terms) => terms.iter().all(|term| self.check_local_deps(term)),
            Term::Unit => true,
            Term::Variant(_, arms) => self.check_local_deps(arms),
        }
    }

    pub fn type_of(&mut self, term: &Term) -> MaybeType {
        // eprintln!("Checking type of {term}");
        match term {
            Term::Abstraction(Abstraction {
                param,
                param_type,
                body,
            }) => self.type_of_abstraction(term, param, param_type, body),
            Term::Application(abs, arg) => self.type_of_application(abs, arg),
            Term::Ascription(value, as_type) => self.type_of_ascription(value, as_type),
            Term::Bool(_) => Ok(Type::Bool),
            Term::Box(term) => {
                let no_local_deps = self.check_local_deps(term);
                if !no_local_deps {
                    return Err(vec![TypeError::BoxedExprHasLocalDeps(term.to_string()).into()]);
                }
                let inner_type = self.resolve_type_of(term)?;
                Ok(Type::Modal(Box::new(inner_type)))
            }
            Term::Fix(abs) => self.type_of_fix(abs),
            Term::MFix(abs) => self.type_of_mfix(abs),
            Term::If(guard, if_true, if_false) => self.type_of_if(guard, if_true, if_false),
            Term::Int(_) => Ok(Type::Int),
            Term::Infix(left, op, right) => self.type_of_infix(left, op, right),
            Term::Let(pattern, value, body) | Term::LetBox(pattern, value, body)=> {
                let value_type = self.resolve_type_of(value)?;
                let mut context = self.clone();
                context.bind_pattern(pattern, term, &value_type)?;
                context.resolve_type_of(body)
            }
            Term::List(values) => {
                if values.is_empty() {
                    Ok(Type::List(None))
                } else {
                    let values_type = self.resolve_type_of(values.first().unwrap())?;
                    let mut errors = Vec::new();
                    for value in values.iter().skip(1) {
                        match self.resolve_type_of(value) {
                            Ok(value_type) if value_type != values_type => errors.push(
                                TypeError::Mismatch {
                                    expected: values_type.clone(),
                                    actual: value_type,
                                }
                                .into(),
                            ),
                            Ok(_) => {}
                            Err(mut err) => errors.append(&mut err),
                        }
                    }

                    if errors.is_empty() {
                        Ok(Type::List(Some(Box::new(values_type))))
                    } else {
                        Err(errors)
                    }
                }
            }
            Term::Match(value, arms) => self.type_of_match(term, value, arms),
            // Only postfix term is "as" which we've already dealt with
            Term::Postfix(..) => unimplemented!(),
            Term::Prefix(op, right) => self.type_of_prefix(op, right),
            Term::Tuple(values) => values
                .iter()
                .map(|value| self.resolve_type_of(value))
                .collect::<Result<_, _>>()
                .map(Type::Tuple),
            Term::Unit => Ok(Type::Unit),
            Term::Variable(name) => self
                .get(name)
                .cloned()
                .ok_or_else(|| vec![TypeError::UndefinedVariable(name.clone()).into()]),
            Term::Variant(label, value) => {
                let value_type = self.resolve_type_of(value)?;
                Ok(Type::Variant(HashMap::from([(label.clone(), value_type)])))
            }
            _ => unimplemented!(),
        }
    }

    pub fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        _term: &Term,
        raw_type: &Type,
    ) -> Result<(), Errors> {
        let r#type = self.resolve(raw_type.clone())?;
        match (pattern, r#type) {
            (Pattern::Tuple(patterns), Type::Tuple(types)) => {
                if patterns.len() != types.len() {
                    return Err(vec![PatternError::MissingElements {
                        actual: patterns.len(),
                        expected: types.len(),
                    }
                    .into()]);
                }
                for (pattern, r#type) in patterns.iter().zip(types.into_iter()) {
                    self.bind_pattern(pattern, _term, &r#type)?;
                }
            }
            (Pattern::Tuple(_), other_type) => {
                return Err(vec![PatternError::Incompatible {
                    actual: pattern.clone(),
                    expected: other_type,
                }
                .into()]);
            }
            (Pattern::Variable(name), value) => self.insert(name.clone(), value),
            (Pattern::Box(_), _) => todo!(),
            (Pattern::Wildcard, _) => {}
        }

        Ok(())
    }

    pub fn resolve(&self, r#type: Type) -> MaybeType {
        match r#type {
            Type::Abstraction(param_type, return_type) => {
                let param_type = self.resolve(*param_type)?;
                let return_type = self.resolve(*return_type)?;
                Ok(Type::Abstraction(
                    Box::new(param_type),
                    Box::new(return_type),
                ))
            }
            Type::List(Some(value_time)) => self
                .resolve(*value_time)
                .map(|value_type| Type::List(Some(Box::new(value_type)))),
            Type::Tuple(values) => {
                let mut value_types = Vec::new();
                let mut errors = Vec::new();

                for value_type in values {
                    match self.resolve(value_type) {
                        Ok(value_type) => value_types.push(value_type),
                        Err(mut err) => errors.append(&mut err),
                    }
                }

                if errors.is_empty() {
                    Ok(Type::Tuple(value_types))
                } else {
                    Err(errors)
                }
            }
            Type::Variable(name) => self
                .types
                .get(&name)
                .cloned()
                .ok_or(vec![anyhow!(TypeError::UnknownType(name))]),
            Type::Variant(variants) => {
                let mut variant_types = HashMap::new();
                let mut errors = Vec::new();
                for (label, variant_type) in variants {
                    match self.resolve(variant_type) {
                        Ok(variant_type) => drop(variant_types.insert(label, variant_type)),
                        Err(mut err) => errors.append(&mut err),
                    }
                }

                if errors.is_empty() {
                    Ok(Type::Variant(variant_types))
                } else {
                    Err(errors)
                }
            }
            Type::Modal(inner_type) => Ok(Type::Modal(Box::new(self.resolve(*inner_type)?))),
            _ => Ok(r#type),
        }
    }

    pub fn resolve_type_of(&mut self, term: &Term) -> MaybeType {
        let r#type = self.type_of(term)?;
        // eprintln!("Type: {}", r#type);
        self.resolve(r#type)
    }
}
