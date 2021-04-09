use crate::lex::lex;
use crate::parse::parse;
use crate::eval::{eval, Environment, EvalResult};

/// Lexes, parses, and evaluates the given program.
pub fn run_interpreter(program: &str) -> EvalResult {
    let mut default_environment = Environment::default();

    match lex(&program) {
        Ok(l) => match parse(&l) {
            Ok(p) => eval(p.clone(), &mut default_environment),
            Err(_) => EvalResult::Err("Parse Failure".to_string())
        },
        Err(_) => EvalResult::Err("Lex Failure".to_string())
    }
}
