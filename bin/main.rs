use std::env;
use std::fs;
use mlisp::interpreter::run_interpreter;
use mlisp::eval::EvalResult;

/// This function will read the command line arguments and read the mlisp file into a string
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("One file must be interpreted".to_string())
    }

    let filename = &args[1];

    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    // Now we pass the contents to the interpreter
    // Only print the result if it is an error
    match run_interpreter(&contents) {
        EvalResult::Err(e) => println!("{}", e),
        _ => ()
    }
}
