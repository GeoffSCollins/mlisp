use crate::types::Expr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum EvalResult {
    Err(String),
    Expr(Rc<Expr>),
    Unit,
}

#[derive(Debug)]
pub struct Environment {
    pub contexts: Vec<HashMap<String, (Vec<String>, Rc<Expr>)>>,
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            contexts: Vec::new(),
        }
    }

    /// Helper function for tests
    pub fn from_vars(vars: &[(&str, Rc<Expr>)]) -> Environment {
        let mut env = Environment::empty();
        env.push_context();
        vars.iter().for_each(|(name, expr)| {
            let _ = env.add_var(name, expr.clone());
        });
        env
    }

    pub fn default() -> Environment {
        let mut env: Environment = Environment::empty();
        env.push_context();
        let _ = env.add_var(&"False".to_string(), Rc::new(Expr::List(vec!())));
        let _ = env.add_var(&"True".to_string(), Rc::new(Expr::List(vec![Rc::new(Expr::FNum(1.0))])));

        env
    }

    /// Looks up the given symbol in the Environment.
    pub fn lookup(&self, symbol: &str) -> Option<(Vec<String>, Rc<Expr>)> {
        self.contexts
            .iter()
            .rev()
            .find(|ctx| ctx.contains_key(symbol))
            .map(|ctx| ctx.get(symbol))
            .flatten()
            .cloned()
    }

    /// Checks whether the given symbol exists in the Environment.
    pub fn contains_key(&self, symbol: &str) -> bool {
        self.contexts
            .iter()
            .rev()
            .find(|ctx| ctx.contains_key(symbol))
            .is_some()
    }

    /// Pushes a new context on the `contexts` stack.
    pub fn push_context(&mut self) {
        self.contexts.push(HashMap::new());
    }

    /// Pops the last context from the `contexts` stack.
    pub fn pop_context(&mut self) {
        self.contexts.pop();
    }

    /// Adds a variable definition to the Environment
    pub fn add_var(&mut self, var: &str, val: Rc<Expr>) -> Result<(), String> {
        self.contexts
            .last_mut()
            .map_or_else(
                || Err("Environment does not have a context to add to.".to_string()),
                |ctx| { ctx.insert(var.to_string(), (Vec::new(), val.clone())); Ok(()) }
            )
    }

    /// Adds a function definition to the Environment
    pub fn add_fn(&mut self, name: &str, params: &[String], body: Rc<Expr>) -> Result<(), String> {
        self.contexts.last_mut().map_or(
            Err("Environment does not have a context to add to.".to_string()),
            |ctx| {
                let param_names = params.iter().map(|s| s.to_string()).collect();
                ctx.insert(name.to_string(), (param_names, body.clone()));
                Ok(())
            }
        )
    }

    pub fn num_contexts(&self) -> usize {
        self.contexts.len()
    }
}

/// Generates the output printed to standard out when the user calls print.
pub fn gen_print_output(expr: Rc<Expr>, env: &mut Environment) -> String {
    // Pattern match
    // If it is a symbol or num then print it
    // If it is a list, go through the exprs and print out each of them
    let mut output_string: String = "".to_string();
    match &*expr {
        Expr::Symbol(s) => {
            // If it is a function we add the following string <func-object func-name>
            // If it is a variable, we add the number if possible otherwise just the name
            match env.lookup(s) {
                Some(rc_expr) => match &*(rc_expr.1) {
                    Expr::Symbol(_) => output_string.push_str(
                        &format!("<func-object: {}>", &s)
                        ),
                    Expr::FNum(fnum) => output_string.push_str(&fnum.to_string()),
                    Expr::List(_) => ()
                },
                None => output_string.push_str(&s)
            }
        }

        Expr::FNum(n) => output_string.push_str(&n.to_string()),
        Expr::List(l) => {
            // Goal is to print everything in the list between parens
            let sub_strings: Vec<String> = l.iter().cloned().map(|e| {gen_print_output(e, env)} ).collect();
            output_string.push_str(
                &format!("({})", sub_strings.join(" "))
            )
        }
    }

    output_string
}

fn add_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    let total = vals.iter()
        .map(|e| match eval(e.clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => Ok(*n),
                a => Err(format!("Can only sum numbers f. Found {:?}", a))
            },
            a => Err(format!("Can only sum numbers. Found {:?}", a))
        })
        .collect::<Result<Vec<f64>, String>>();

    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| EvalResult::Expr(Expr::fnum(xs.iter().sum()))
    )
}

fn subtract_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() == 1 {
        return EvalResult::Err("More than one value must be provided to subtract".to_string());
    }

    let first_number: f64;
    match eval(vals[0].clone(), env) {
        EvalResult::Expr(exp) => match &*exp {
            Expr::FNum(n) => { first_number = n.clone(); },
            err => return EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
        },
        err => return EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
    }

    if vals.len() == 2 {
        match eval(vals[1].clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => return EvalResult::Expr(Expr::fnum(first_number - n.clone())),
                err => return EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
            },
            err => return EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
        }
    }    

    let subtract_total = add_vals(&vals[1..], env);

    match subtract_total {
        EvalResult::Expr(exp) => match &*exp {
            Expr::FNum(n) => EvalResult::Expr(Expr::fnum(first_number - n.clone())),
            err => return EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
        },
        err => EvalResult::Err(format!("Can only subtract numbers f. Found {:?}", err))
    }
}

fn multiply_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() == 1 {
        return EvalResult::Err(format!("More than one value must be provided to multiply. Value provided was {:?}", vals));
    }
    
    let mut result: f64 = 1.0;

    for val in vals {
        match eval(val.clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => { result = result * n.clone(); },
                err => return EvalResult::Err(format!("Can only multiply numbers f. Found {:?}", err))
            },
            err => return EvalResult::Err(format!("Can only multiply numbers f. Found {:?}", err))
        }
    }

    return EvalResult::Expr(Expr::fnum(result));
}

fn divide_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() == 1 {
        return EvalResult::Err("More than one value must be provided to divide".to_string());
    }

    let first_number;
    match eval(vals[0].clone(), env) {
        EvalResult::Expr(exp) => match &*exp {
            Expr::FNum(n) => { first_number = n.clone(); },
            err => return EvalResult::Err(format!("Can only divide numbers. Found {:?}", err))
        },
        err => return EvalResult::Err(format!("Can only divide numbers. Found {:?}", err))
    }

    if vals.len() == 2 {
        match eval(vals[1].clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => return EvalResult::Expr(Expr::fnum(first_number / n.clone())),
                err => return EvalResult::Err(format!("Can only divide numbers. Found {:?}", err))
            },
            err => return EvalResult::Err(format!("Can only divide numbers. Found {:?}", err))
        }
    }

    let division_total = multiply_vals(&vals[1..], env);

    match division_total {
        EvalResult::Expr(exp) => match &*exp {
            Expr::FNum(n) => EvalResult::Expr(Expr::fnum(first_number / n.clone())),
            err => EvalResult::Err(format!("Can only divide numbers f. Found {:?}", err))
        },
        err => EvalResult::Err(format!("Can only divide numbers f. Found {:?}", err))
    }
}


fn add_var_to_env(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    // Incorrectly defined let statements
    if vals.len() != 2 {
        return EvalResult::Err(
            "Invalid variable definition. Should look like (let someVar someExpr)".to_string()
        )
    }

    // Otherwise, let-statement has the correct number of elements
    match (&*vals[0], &vals[1]) {
        (Expr::Symbol(s), e) => match eval(e.clone(), env) {
            EvalResult::Expr(e) => {
                if s == "+" || s == "-" || s == "*" || s == "/" || s == "or" || s == "and" || s == "not" ||  s == "=" || s == "!=" ||  s == "if" || s == "let" || s == "fn" || s == "print" {
                    EvalResult::Err("Cannot use a keyword as a variable.".to_string())
                } else {
                    env.add_var(s, e).map_or_else(|s| EvalResult::Err(s), |_| EvalResult::Unit)
                }
            },

            // This is the case (let x (let y FNum(5.0)))
            EvalResult::Unit => EvalResult::Err("Cannot assign unit to a variable.".to_string()),
            err => err,
        },
        _ => EvalResult::Err(
            "Second element of variable def must be a symbol and third must be an expression".to_string()
        ),
    }
}

fn add_fn_to_env(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() != 3 {
        return EvalResult::Err("function definitions must follow the pattern fn fn-name (arg1 .. argn) <Expr>".to_string());
    }

    let fn_name = &*vals[0];
    let p_names = &*vals[1];
    let body = &vals[2];

    match (&*fn_name, p_names, body) {
        (Expr::Symbol(fn_name), Expr::List(params), body) => {
            let ps: Result<Vec<String>, String> = params.iter().cloned().map(|e| {
                if let Expr::Symbol(n) = &*e {
                    Ok(n.to_string())
                } else {
                    Err("Function parameters must be symbols.".to_string())
                }
            })
            .collect();

            ps.map_or_else(
                |err| EvalResult::Err(err),
                |xs| env.add_fn(fn_name, xs.as_slice(), body.clone()).map_or_else(
                    |err| EvalResult::Err(err),
                    |_| EvalResult::Unit
                )
            )
        },
        _ => EvalResult::Err("Function definitions must follow the pattern (fn fn-name (arg1 .. argn) <Expr>)".to_string())
    }
}

fn bool_or(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() == 1 {
        return EvalResult::Err("More than one value must be provided to or".to_string());
    }

    for val in vals {
        match eval(val.clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::Symbol(s) => {
                    if s == "True" {
                        return EvalResult::Expr(Rc::new(Expr::Symbol("True".to_string())));
                    } else if s == "False" {
                        continue;
                    } else {
                        return EvalResult::Err("Symbol is not a boolean.".to_string())
                    }
                },
                Expr::List(l) => {
                    if l.len() == 0 {
                        // This is false so continue
                        continue
                    } else if l.len() == 1{
                        // If it is FNum(1.0) then it is true
                        match &*l[0] {
                            Expr::FNum(n) => {
                                if n.clone() == 1.0 {
                                    return EvalResult::Expr(Rc::new(Expr::Symbol("True".to_string())));
                                } else {
                                    return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", l))
                                }
                            }, 
                            err => return EvalResult::Err(format!("And can only evaluate bool values. A {:?} was provided", err))
                        }
                    } else {
                        return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", l))
                    }

                },
                err => return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", err))
            },
            err => return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", err))          
        }
    }

    return EvalResult::Expr(Rc::new(Expr::Symbol("False".to_string())))
}

fn bool_and(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() == 1 {
        return EvalResult::Err("More than one value must be provided and".to_string());
    }

    for val in vals {
        match eval(val.clone(), env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::Symbol(s) => {
                    if s == "False" {
                        return EvalResult::Expr(Rc::new(Expr::Symbol("False".to_string())));
                    } else if s == "True" {
                        continue;
                    } else {
                        return EvalResult::Err("Symbol is not a boolean.".to_string())
                    }
                },
                Expr::List(l) => {
                    if l.len() == 0 {
                        // This is false so return false
                        return EvalResult::Expr(Rc::new(Expr::Symbol("False".to_string())));
                    } else if l.len() == 1{
                        // If it is FNum(1.0) then it is true
                        match &*l[0] {
                            Expr::FNum(n) => {
                                if n.clone() == 1.0 {
                                    // True so continue
                                    continue
                                } else {
                                    return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", l))
                                }
                            },
                            err => return EvalResult::Err(format!("And can only evaluate bool values. A {:?} was provided", err))
                        }
                    } else {
                        return EvalResult::Err(format!("Or can only evaluate bool values. A {:?} was provided", l))
                    }

                },
                err => return EvalResult::Err(format!("And can only evaluate bool values. A {:?} was provided", err))
            },
            err => return EvalResult::Err(format!("And can only evaluate bool values. A {:?} was provided", err))          
        }
    }

    return EvalResult::Expr(Rc::new(Expr::Symbol("True".to_string())))
}

fn bool_not(val: &Rc<Expr>, env: &mut Environment) -> EvalResult {
    let r1 = eval(val.clone(), env);

    match &r1 {        
        EvalResult::Expr(exp_one) => {
            match &**exp_one {
                Expr::Symbol(s_one) => {
                    if s_one == "True" {
                        return EvalResult::Expr(Expr::symbol("False"))
                    } else {
                        return EvalResult::Expr(Expr::symbol("True"))
                    }
                },
                Expr::List(l) => {
                    if l.len() == 0 {
                        return EvalResult::Expr(Expr::symbol("True"))
                    } else {
                        return EvalResult::Expr(Expr::symbol("False"))
                    }
                }
                a => return EvalResult::Err(format!("Symbol is not a boolean. Found {:?}", a))
            }
        },        
        _ => return EvalResult::Err("Symbol is not a boolean.".to_string())
    }
}

fn bool_equal(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() < 2 {
        return EvalResult::Err("= must compare at least two values.".to_string())
    }

    let first_value = eval(vals[0].clone(), env);

    for val in vals.clone() {
        let curr_val = eval(val.clone(), env);
        if curr_val != first_value { return EvalResult::Expr(Expr::symbol("False")) }
    }

    return EvalResult::Expr(Expr::symbol("True"))
}

fn bool_not_equal(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    // Just return the negation of the bool equal function
    if vals.len() < 2 {
        return EvalResult::Err("!= must compare at least two values.".to_string())
    }

    match bool_equal(vals, env) {
        EvalResult::Expr(exp) => match &*exp {
            Expr::Symbol(sym) => {
                if sym == "True" {
                    EvalResult::Expr(Expr::symbol("False"))
                } else {
                    EvalResult::Expr(Expr::symbol("True"))
                }
            },
            _ => EvalResult::Err("Something happened".to_string()) // I don't think it should ever get here
        },
        _ => EvalResult::Err("Something happened".to_string())
    }
}

fn print_expr(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    for val in vals.clone() {
        match &eval(val.clone(), env) {
            EvalResult::Expr(exp) => print!("{} ", gen_print_output(exp.clone(), env)),
            _ => return EvalResult::Err("Something bad happened".to_string())
        }
    }
    EvalResult::Unit
}

fn run_if_statement(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() != 3 {
        return EvalResult::Err("There must be three components to the if statement. If, true expression, false expression".to_string());
    }

    let directing_statement = vals[0].clone();
    let true_expression = vals[1].clone();
    let false_expression = vals[2].clone();

    match eval(directing_statement, env) {
        EvalResult::Expr(exp) => match &*exp {
            Expr::Symbol(s) => {
                if s == "True" {
                    // We run the true expresion and return the result
                    eval(true_expression, env)
                } else {
                    eval(false_expression, env)
                }
            },
            Expr::List(l) if l.len() == 0 => eval(false_expression, env),
            Expr::List(l) if l.len() == 1 => match &*l[0] {
                Expr::FNum(n) if n.clone() == 1.0 => eval(true_expression, env),
                err => EvalResult::Err(format!("Unknown error. Found error {:?}", err))
            }
            err => EvalResult::Err(format!("Unknown error. Found error {:?}", err))
        },
        err => EvalResult::Err(format!("Unknown error here. Found {:?}", err)),
    }
}

fn eval_all_list(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    let mut finalized_vals = Vec::new();
    for val in vals.clone() {
        match &eval(val.clone(), env) {
            EvalResult::Expr(exp) => match &**exp {
                Expr::List(l) => { finalized_vals.push(Expr::list(l)); },
                Expr::FNum(n) => { finalized_vals.push(Expr::fnum(n.clone())); }
                Expr::Symbol(s) => { finalized_vals.push(Expr::symbol(s)); }
            },
            EvalResult::Unit => (),
            err => return EvalResult::Err(format!("Failed evaluating all values in the list. Found {:?}", err))
        }
    }

    EvalResult::Expr(Expr::list(&finalized_vals))
}

fn run_function(function_name: &str, parameters: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    match &env.lookup(function_name) {
        None => EvalResult::Err("Function has not been defined".to_string()),
        Some(x) => match &x {
            (parameter_names, body) => {
                if parameter_names.len() != parameters.len() {
                    return EvalResult::Err("The number of parameters supplied was incorect".to_string())
                }
    
                env.push_context();
    
                // Add all parameters to the environment
                for (i, param) in parameters.clone().iter().enumerate() {
                    match eval(param.clone(), env) {
                        EvalResult::Expr(exp) => match &*exp {
                            Expr::List(_) => return EvalResult::Err("Should never get here".to_string()),
                            _ => env.add_var(&parameter_names[i], exp.clone())
                        },
                        _ => return EvalResult::Err("Parameters provided must be values not symbols".to_string())
                    };
                }
    
                // Now evaluate the function body
                let result = eval(body.clone(), env);

                env.pop_context();

                result
            }
        },
    }
}

/// Evaluates the given expression
pub fn eval(e: Rc<Expr>, env: &mut Environment) -> EvalResult { 
    match &*e {
        Expr::FNum(_) => EvalResult::Expr(e.clone()),
        Expr::Symbol(s) => if env.contains_key(&s) { 
            EvalResult::Expr(env.lookup(&s).unwrap().1) 
            } else { 
                EvalResult::Expr(e)
            }

        Expr::List(vals) => { 
            if vals.is_empty() {
                return EvalResult::Expr(Expr::list(&[]));
            }

            let op = &*vals[0];
            match op {
                // List[FNum(1.0)] is True
                Expr::List(l) if l.len() == 1 => match &*l[0] {
                    Expr::FNum(n) => if n.clone() == 1.0 { 
                        EvalResult::Expr(Expr::symbol("True")) 
                    } else {
                        // eval_all_list(vals[0], env),
                        EvalResult::Err("I don't know man".to_string())
                    },
                    a => EvalResult::Err(format!("Unknown error. In eval, I found {:?}", a)),
                },
                Expr::FNum(n) if n.clone() == 1.0 => EvalResult::Expr(Expr::symbol("True")),
                
                // List[] is False
                Expr::List(l) if l.is_empty() => EvalResult::Expr(Expr::symbol("False")),

                // List[???] just needs to be evaluated
                Expr::List(l) if l.len() > 1 => eval_all_list(&vals, env),

                // Case: <Expr> <Expr> <Expr>
                Expr::Symbol(s) if s == "+" => add_vals(&vals[1..], env),
                Expr::Symbol(s) if s == "-" => subtract_vals(&vals[1..], env),
                Expr::Symbol(s) if s == "*" => multiply_vals(&vals[1..], env),
                Expr::Symbol(s) if s == "/" => divide_vals(&vals[1..], env),

                //or, and, not, =, and !=, print
                Expr::Symbol(s) if s == "or" => bool_or(&vals[1..], env),
                Expr::Symbol(s) if s == "and" => bool_and(&vals[1..], env),
                Expr::Symbol(s) if s == "not" => {
                    if vals.len() != 2 {
                        EvalResult::Err("Only one boolean value can be negated".to_string())
                    } else {
                        bool_not(&vals[1], env)
                    }
                }
                Expr::Symbol(s) if s == "=" => bool_equal(&vals[1..], env),
                Expr::Symbol(s) if s == "!=" => bool_not_equal(&vals[1..], env),
                Expr::Symbol(s) if s == "print" => print_expr(&vals[1..], env),

                // Case: (let x <Expr>) 
                Expr::Symbol(s) if s == "let" => add_var_to_env(&vals[1..], env), 

                // Case: (fn my-func (xl x2 x3) <Expr>) 
                Expr::Symbol(s) if s == "fn" => add_fn_to_env(&vals[1..], env), 

                // Case: if ??? 
                Expr::Symbol(s) if s == "if" => run_if_statement(&vals[1..], env), 

                // Case: x or my-func (x1 x2 x3) <Expr>
                Expr::Symbol(s) if env.contains_key(&s) => {
                    if let Some(x) = env.lookup(&s) {
                        match eval(x.1, env) {
                            EvalResult::Expr(exp) => match &*exp {
                                Expr::List(l) => {
                                    if l.is_empty() {
                                        EvalResult::Expr(Expr::symbol("False"))
                                    } else {
                                        EvalResult::Err("Should not get here".to_string())
                                    }
                                },
                                Expr::Symbol(eval_s) => EvalResult::Expr(Expr::symbol(eval_s)),
                                _ => run_function(&s, &vals[1..].to_vec(), env)
                            },
                            _ => run_function(&s, &vals[1..].to_vec(), env)
                        }
                    } else {
                        EvalResult::Err("I don't think it should get here".to_string())
                    }

                    // s is the function name
                },

                Expr::Symbol(s) if !env.contains_key(&s) => eval_all_list(&vals, env),

                // All other cases
                a => EvalResult::Err(format!("Unknown error. In eval, I found {:?}", a)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
