use crate::parser::Span;
use crate::type_check::context::WhichContext;
use anyhow::anyhow;
use ariadne::Source;
use eval::{environment::Environment, prelude::Prelude, value::Value};
use parser::Parser;
use rayon::ThreadPool;
use std::fs;
use std::sync::Arc;
use syntax::{
    program::{Declaration, Program},
    r#type::Type,
    term::Term,
};
use type_check::context::Context;

mod error;
mod eval;
mod parser;
mod syntax;
mod type_check;

fn make_context() -> (Context, Environment) {
    let mut context = Context::default();
    let mut env = Environment::default();

    Prelude::default().add_prelude_to(&mut context, &mut env);

    (context, env)
}

fn type_check(decl: &Declaration, context: &mut Context,
              source: &str,
              path: &str) -> anyhow::Result<()>  {
    match decl {
        Declaration::Term(pattern, term) => {
            let raw_type = context.type_of(term);
            let Ok(raw_type) = raw_type else {
                let reports = raw_type.unwrap_err();
                for report in reports {
                    report
                        .build_report()
                        .eprint((path.to_owned(), Source::from(&source)))?;
                }
                std::process::exit(1)
            };

            let r#type = context.resolve(raw_type);

            let Ok(r#type) = r#type else {
                let reports = r#type.unwrap_err();
                for report in reports {
                    report
                        .build_report()
                        .eprint((path.to_owned(), Source::from(&source)))?;
                }
                std::process::exit(1)
            };
            let which_context = match term {
                Term::MLet(..) => WhichContext::Mobile,
                _ => WhichContext::Local,
            };
            context
                .bind_pattern(which_context, pattern, term, &r#type)
                .unwrap();
        }
        Declaration::Type(name, r#type) => {
            context.insert_type(name.clone(), r#type.clone())
        }
    }
    Ok(())
}

fn eval(decl: &Declaration,
        env: &mut Environment,
        thread_pool: &ThreadPool) -> anyhow::Result<()> {
    if let Declaration::Term(pattern, term) = decl {
        let value = env.eval(thread_pool, term)?;
        env.bind_pattern(pattern, value)?;
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let Some(path) = std::env::args().nth(1) else {
        return Err(anyhow!("Usage: b7 <file>"));
    };

    // Parsing
    let source = fs::read_to_string(&path)?;
    let parser = Parser::new(path.clone());
    let Program(decls) = parser.parse_source(&source)?;
    
    // Prelude
    let (mut context, mut env) = make_context();
    
    // Type checking
    for decl in &decls {
        type_check(decl, &mut context, &source, &path)?;
    }
    
    // Evaluating
    let thread_pool = rayon::ThreadPoolBuilder::new().build()?;
    for decl in &decls {
        eval(decl, &mut env, &thread_pool)?
    }

    let main_type = context.get("main", 0, Span::default()).unwrap();
    let main = env.get("main")?;

    match (main, main_type) {
        (Value::Abstraction(abs, env), Type::Abstraction(_, param_type, _))
            if param_type.is_tuple() =>
        {
            let span = Span::default();
            let value = env.clone().eval(
                &thread_pool,
                &Term::Application(
                    Arc::new(Term::Abstraction(abs.clone(), span.clone())),
                    Arc::new(Term::Tuple(vec![], span.clone())),
                    span.clone(),
                ),
            )?;
            println!("{value}")
        }
        (value, r#type) => println!("{value}: {type}"),
    }

    Ok(())
}
