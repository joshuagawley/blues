use std::fs;

use eval::{context::Context, environment::Environment, prelude::Prelude, value::Value};
use parser::Parser;
use syntax::{
    program::{Declaration, Program},
    r#type::Type,
    term::Term,
};

mod error;
mod eval;
mod parser;
mod syntax;

fn main() -> anyhow::Result<()> {
    let input = fs::read_to_string("/Users/jg/dev/Projects/new_moody/fac.mdy")?;
    let Program(decls) = Parser::new("input".to_owned()).parse_source(&input)?;

    let mut context = Context::default();
    let mut env = Environment::default();

    Prelude::default().add_prelude_to(&mut context, &mut env);

    for decl in &decls {
        match decl {
            Declaration::Term(pattern, term) => {
                let raw_type = context.type_of(term);
                let Ok(raw_type) = raw_type else {
                    let reports = raw_type.unwrap_err();
                    for report in reports {
                        eprintln!("{report}")
                    }
                    std::process::exit(1)
                };

                let r#type = context.resolve(raw_type);

                let Ok(r#type) = r#type else {
                    let reports = r#type.unwrap_err();
                    for report in reports {
                        eprintln!("{report}")
                    }
                    std::process::exit(1)
                };
                context.bind_pattern(pattern, term, &r#type).unwrap();
                let value = env.eval(term)?;
                env.bind_pattern(pattern, value)?;
            }
            Declaration::Type(name, r#type) => context.insert_type(name.clone(), r#type.clone()),
        }
    }

    let main_type = context.get("main").unwrap();
    let main = env.get("main")?;

    match (main, main_type) {
        (Value::Abstraction(abs, env), Type::Abstraction(param_type, _))
            if param_type.is_tuple() =>
        {
            let value = env.clone().eval(&Term::Application(
                Box::new(Term::Abstraction(abs.clone())),
                Box::new(Term::Tuple(Vec::new())),
            ))?;
            println!("{value}")
        }
        (value, r#type) => println!("{value}: {type}"),
    }

    Ok(())
}
