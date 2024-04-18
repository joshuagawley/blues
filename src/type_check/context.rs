use std::collections::HashMap;

use crate::{
    error::{pattern_error::PatternError, type_error::TypeError},
    syntax::{
        abstraction::Abstraction,
        pattern::Pattern,
        r#type::Type,
        term::{Infix, Prefix, Term},
    },
};

use crate::error::{Errors, MaybeType};
use crate::parser::Span;
use ariadne::Span as AriadneSpan;
use indexmap::IndexMap;

#[derive(Clone, Default, Debug)]
pub struct Context {
    types: HashMap<String, Type>,
    pub(crate) bindings: HashMap<String, Type>,
}

impl Context {
    pub fn type_of(&mut self, term: &Term) -> MaybeType {
        match term {
            Term::Abstraction(
                Abstraction {
                    param,
                    param_type,
                    body,
                },
                span,
            ) => self.type_of_abstraction(term, param, param_type, body, span),
            Term::Application(abs, arg, span) => self.type_of_application(abs, arg, span),
            Term::Ascription(value, as_type, span) => self.type_of_ascription(value, as_type, span),
            Term::Bool(_, span) => Ok(Type::Bool(span.clone())),
            Term::Box(term, span) => {
                if self.has_local_deps(term) {
                    return Err(vec![TypeError::BoxedExprHasLocalDeps(
                        span.start(),
                        span.clone(),
                        term.to_string(),
                    )
                    .into()]);
                }
                let inner_type = self.resolve_type_of(term)?;
                Ok(Type::Modal(span.clone(), Box::new(inner_type)))
            }
            Term::Fix(abs, span) => self.type_of_fix(abs, span),
            Term::MFix(abs, span) => self.type_of_mfix(abs, span),
            Term::If(guard, if_true, if_false, span) => {
                self.type_of_if(guard, if_true, if_false, span)
            }
            Term::Int(_, span) => Ok(Type::Int(span.clone())),
            Term::Infix(left, op, right, span) => self.type_of_infix(left, op, right, span),
            Term::Let(pattern, value, body, _) => {
                let value_type = self.resolve_type_of(value)?;
                let mut context = self.clone();
                context.bind_pattern(pattern, term, &value_type)?;
                context.resolve_type_of(body)
            }

            Term::LetBox(pattern, value, body, _) => {
                let modal_value_type = self.resolve_type_of(value)?;
                let value_type = modal_value_type.get_inner_type();
                let mut context = self.clone();
                context.bind_pattern(pattern, term, value_type)?;
                Ok(context.resolve_type_of(body)?.clone())
            }
            Term::Match(value, arms, span) => self.type_of_match(term, value, arms, span),
            // Only postfix term is "as" which we've already dealt with
            Term::Postfix(..) => unimplemented!(),
            Term::Prefix(op, right, span) => self.type_of_prefix(op, right, span),
            Term::Tuple(values, span) => values
                .iter()
                .map(|value| self.resolve_type_of(value))
                .collect::<Result<_, _>>()
                .map(|value_types| Type::Tuple(span.clone(), value_types)),
            Term::Unit(span) => Ok(Type::Unit(span.clone())),
            Term::Variable(name, span) => self.get(name).cloned().ok_or_else(|| {
                vec![TypeError::UndefinedVariable(span.start(), span.clone(), name.clone()).into()]
            }),
            Term::Variant(label, value, span) => {
                let value_type = self.resolve_type_of(value)?;
                Ok(Type::Variant(
                    span.clone(),
                    IndexMap::from([(label.clone(), value_type)]),
                ))
            }
        }
    }

    pub fn resolve_type_of(&mut self, term: &Term) -> MaybeType {
        let r#type = self.type_of(term)?;
        // eprintln!("Type of {term}: {}", r#type);
        self.resolve(r#type)
    }

    pub fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        _term: &Term,
        raw_type: &Type,
    ) -> Result<(), Errors> {
        let r#type = self.resolve(raw_type.clone())?;
        match (pattern, r#type) {
            (Pattern::Tuple(span, patterns), Type::Tuple(_, types)) => {
                if patterns.len() != types.len() {
                    return Err(vec![PatternError::MissingElements {
                        span: span.clone(),
                        actual: patterns.len(),
                        expected: types.len(),
                    }
                    .into()]);
                }
                for (pattern, r#type) in patterns.iter().zip(types.into_iter()) {
                    self.bind_pattern(pattern, _term, &r#type)?;
                }
            }
            (Pattern::Tuple(span, _), other_type) => {
                return Err(vec![PatternError::Incompatible {
                    span: span.clone(),
                    actual: pattern.clone(),
                    expected: other_type,
                }
                .into()]);
            }
            (Pattern::Variable(_, name), value) => self.insert(name.clone(), value),
            (Pattern::Wildcard(_), _) => {}
        }

        Ok(())
    }

    pub fn resolve(&self, r#type: Type) -> MaybeType {
        match r#type {
            Type::Abstraction(span, param_type, return_type) => {
                let param_type = self.resolve(*param_type)?;
                let return_type = self.resolve(*return_type)?;
                Ok(Type::Abstraction(
                    span,
                    Box::new(param_type),
                    Box::new(return_type),
                ))
            }
            Type::Tuple(span, values) => {
                let mut value_types = Vec::new();
                let mut errors = Vec::new();

                for value_type in values {
                    match self.resolve(value_type) {
                        Ok(value_type) => value_types.push(value_type),
                        Err(mut err) => errors.append(&mut err),
                    }
                }

                if errors.is_empty() {
                    Ok(Type::Tuple(span, value_types))
                } else {
                    Err(errors)
                }
            }
            Type::Variable(span, name) => self
                .types
                .get(&name)
                .cloned()
                .ok_or(vec![TypeError::UnknownType(span.start(), span, name).into()]),
            Type::Variant(span, variants) => {
                let mut variant_types = IndexMap::new();
                let mut errors = Vec::new();
                for (label, variant_type) in variants {
                    match self.resolve(variant_type) {
                        // We don't need to know if the key exists already
                        Ok(variant_type) => drop(variant_types.insert(label, variant_type)),
                        Err(mut err) => errors.append(&mut err),
                    }
                }

                if errors.is_empty() {
                    Ok(Type::Variant(span, variant_types))
                } else {
                    Err(errors)
                }
            }
            Type::Modal(span, inner_type) => {
                Ok(Type::Modal(span, Box::new(self.resolve(*inner_type)?)))
            }
            _ => Ok(r#type),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }

    pub fn insert(&mut self, name: String, r#type: Type) {
        self.bindings.insert(name, r#type);
    }

    pub fn insert_type(&mut self, name: String, r#type: Type) {
        self.types.insert(name, r#type);
    }

    fn type_of_abstraction(
        &mut self,
        term: &Term,
        param: &Pattern,
        param_type: &Type,
        body: &Term,
        span: &Span,
    ) -> MaybeType {
        let mut context = self.clone();
        let param_type = self.resolve(param_type.clone())?;
        context.bind_pattern(param, term, &param_type)?;
        let body_type = context.resolve_type_of(body)?;
        Ok(Type::Abstraction(
            span.clone(),
            Box::new(param_type),
            Box::new(body_type),
        ))
    }

    fn type_of_application(&mut self, abs: &Term, arg: &Term, span: &Span) -> MaybeType {
        let abs_type = self.resolve_type_of(abs)?;
        let arg_type = self.resolve_type_of(arg)?;

        match abs_type {
            Type::Abstraction(_, param_type, return_type) => {
                let param_type = self.resolve(*param_type)?;
                let return_type = self.resolve(*return_type)?;

                if param_type.get_inner_type() == &return_type {
                    Ok(return_type)
                } else {
                    Err(vec![TypeError::Mismatch {
                        offset: span.start(),
                        span: arg_type.span().clone(),
                        expected: arg_type,
                        actual: param_type,
                    }
                    .into()])
                }
            }
            _ => Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: abs_type.span().clone(),
                expected: Type::make_error_abs(),
                actual: abs_type,
            }
            .into()]),
        }
    }

    fn type_of_ascription(&mut self, value: &Term, as_type: &Type, span: &Span) -> MaybeType {
        let as_type = self.resolve(as_type.clone())?;
        let value_type = self.resolve_type_of(value)?;

        if value_type == as_type {
            return Ok(value_type);
        }

        match (&value_type, &as_type) {
            (Type::Variant(_, term_variants), Type::Variant(_, variants))
                if term_variants
                    .iter()
                    .all(|(label, r#type)| variants.get(label) == Some(r#type)) =>
            {
                Ok(as_type)
            }
            (_, _) if value_type == as_type => Ok(value_type),
            _ => Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: as_type.span().clone(),
                expected: value_type,
                actual: as_type,
            }
            .into()]),
        }
    }

    fn type_of_infix(&mut self, left: &Term, op: &Infix, right: &Term, span: &Span) -> MaybeType {
        let left_type = self.resolve_type_of(left)?;
        let right_type = self.resolve_type_of(right)?;

        match op {
            Infix::Or | Infix::And => {
                let mut errors = Vec::new();

                if !left_type.is_bool() {
                    errors.push(
                        TypeError::Mismatch {
                            offset: span.start(),
                            span: left.span().clone(),
                            expected: Type::Bool(Span::default()),
                            actual: left_type,
                        }
                        .into(),
                    );
                }

                if !right_type.is_bool() {
                    errors.push(
                        TypeError::Mismatch {
                            offset: span.start(),
                            span: right.span().clone(),
                            expected: Type::Bool(Span::default()),
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Bool(span.clone()))
                } else {
                    Err(errors)
                }
            }

            Infix::Eq | Infix::NtEq => {
                if left_type == right_type {
                    Ok(Type::Bool(span.clone()))
                } else {
                    Err(vec![TypeError::Mismatch {
                        offset: span.start(),
                        span: right.span().clone(),
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
                            offset: span.start(),
                            span: left.span().clone(),
                            expected: Type::Int(Span::default()),
                            actual: left_type,
                        }
                        .into(),
                    );
                }
                if !right_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            offset: span.start(),
                            span: right.span().clone(),
                            expected: Type::Int(Span::default()),
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Bool(Span::default()))
                } else {
                    Err(errors)
                }
            }

            Infix::Add | Infix::Sub | Infix::Mul | Infix::Div => {
                let mut errors = Vec::new();
                if !left_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            offset: span.start(),
                            span: left.span().clone(),
                            expected: Type::Int(Span::default()),
                            actual: left_type,
                        }
                        .into(),
                    );
                }
                if !right_type.is_int() {
                    errors.push(
                        TypeError::Mismatch {
                            offset: span.start(),
                            span: right.span().clone(),
                            expected: Type::Int(Span::default()),
                            actual: right_type,
                        }
                        .into(),
                    );
                }

                if errors.is_empty() {
                    Ok(Type::Int(Span::default()))
                } else {
                    Err(errors)
                }
            }
        }
    }

    fn type_of_fix(&mut self, abs: &Term, span: &Span) -> MaybeType {
        let abs_type = self.resolve_type_of(abs)?;
        let (param_type, return_type) = abs_type.unroll_abs()?;

        let param_type = self.resolve(*param_type)?;
        let return_type = self.resolve(*return_type)?;

        if param_type == return_type {
            Ok(return_type)
        } else {
            Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: return_type.span().clone(),
                expected: param_type,
                actual: return_type,
            }
            .into()])
        }
    }

    fn type_of_mfix(&mut self, abs: &Term, span: &Span) -> MaybeType {
        let abs_type = self.resolve_type_of(abs)?;
        let (param_type, return_type) = abs_type.unroll_abs()?;
        let param_type = self.resolve(*param_type)?;
        let return_type = self.resolve(*return_type)?;

        if !matches!(*param_type.clone().unroll_abs()?.0, Type::Modal(..)) {
            return Err(vec![TypeError::ExpectedModal(
                span.start(),
                span.clone(),
                param_type.clone(),
            )
            .into()]);
        }

        if param_type == return_type {
            Ok(return_type)
        } else {
            Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: return_type.span().clone(),
                expected: param_type,
                actual: return_type,
            }
            .into()])
        }
    }

    fn type_of_if(
        &mut self,
        guard: &Term,
        if_true: &Term,
        if_false: &Term,
        span: &Span,
    ) -> MaybeType {
        let guard_type = self.resolve_type_of(guard)?;
        if !guard_type.is_bool() {
            return Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: guard_type.span().clone(),
                expected: Type::Bool(Span::default()),
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
                offset: span.start(),
                span: if_false_type.span().clone(),
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
        _span: &Span,
    ) -> MaybeType {
        let value_type = self.resolve_type_of(value)?;
        let Type::Variant(span, variants) = &value_type else {
            return Err(vec![TypeError::Mismatch {
                offset: term.span().start(),
                span: term.span().clone(),
                expected: Type::Variant(
                    Span::default(),
                    IndexMap::from([(
                        "...".to_owned(),
                        Type::Variable(Span::default(), "*".to_owned()),
                    )]),
                ),
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
            errors
                .push(TypeError::MissingVariants(term.span().start(), span.clone(), absent).into());
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
                    arm_types.insert(label.clone(), (arm_type, body.span().clone()));
                }
                Err(mut errs) => errors.append(&mut errs),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let (match_type, _) = arm_types.first().unwrap().1.clone();
        for (_, (arm_type, span)) in arm_types {
            if arm_type != match_type {
                errors.push(
                    TypeError::Mismatch {
                        offset: term.span().start(),
                        span,
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

    fn type_of_prefix(&mut self, op: &Prefix, right: &Term, span: &Span) -> MaybeType {
        let right_type = self.resolve_type_of(right)?;
        match (op, right_type.get_inner_type()) {
            (Prefix::Neg, Type::Int(..)) => Ok(Type::Int(span.clone())),
            (Prefix::Neg, other_type) => Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: right.span().clone(),
                expected: Type::Int(Span::default()),
                actual: other_type.clone(),
            }
            .into()]),
            (Prefix::Not, Type::Bool(..)) => Ok(Type::Bool(span.clone())),
            (Prefix::Not, other_type) => Err(vec![TypeError::Mismatch {
                offset: span.start(),
                span: right.span().clone(),
                expected: Type::Bool(Span::default()),
                actual: other_type.clone(),
            }
            .into()]),
        }
    }

    fn has_local_deps(&self, term: &Term) -> bool {
        match term {
            Term::Variable(name, _) => {
                let r#type = self.get(name).unwrap();
                matches!(r#type, Type::Int(..) | Type::Bool(..) | Type::Modal(..))
            }

            Term::Abstraction(
                Abstraction {
                    param: _,
                    param_type,
                    body,
                },
                _,
            ) => matches!(param_type, Type::Modal(..)) && self.has_local_deps(body),
            Term::Application(_, arg, _) => self.has_local_deps(arg),
            Term::Ascription(term, _, _) => self.has_local_deps(term),
            Term::Bool(..) => false,
            Term::Box(term, _) => self.has_local_deps(term),
            Term::Fix(..) => true,
            Term::MFix(term, _) => self.has_local_deps(term),
            Term::If(_guard, if_true, if_false, _) => {
                self.has_local_deps(if_true) && self.has_local_deps(if_false)
            }
            Term::Int(..) => false,
            Term::Infix(left, _, right, _) => {
                self.has_local_deps(left) && self.has_local_deps(right)
            }
            Term::Let(..) => true,
            Term::LetBox(_, value, body, _) => {
                self.has_local_deps(value) && self.has_local_deps(body)
            }
            Term::Match(value, arms, _) => {
                self.has_local_deps(value)
                    && arms.values().all(|(_, term)| self.has_local_deps(term))
            }
            Term::Postfix(term, _, _) => self.has_local_deps(term),
            Term::Prefix(_, term, _) => self.has_local_deps(term),
            Term::Tuple(terms, _) => terms.iter().all(|term| self.has_local_deps(term)),
            Term::Unit(..) => false,
            Term::Variant(_, arms, _) => self.has_local_deps(arms),
        }
    }
}
