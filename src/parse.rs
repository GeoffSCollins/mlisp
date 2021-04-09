use crate::lex::Token;
use crate::types::Expr;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {
    BadParse(String),
    EOF,
}

#[derive(Debug)]
pub enum ParseResult {
    Success(usize, Rc<Expr>),
    Failure(ParseError),
}

fn parse_helper(tokens: &[Token], index: usize) -> ParseResult {
    let mut index = index;

    if let Some(mut t) = tokens.get(index) {
        match &*t {
            Token::LPar => {
                index += 1;
                let mut exprs = Vec::new();

                while *t != Token::RPar {
                    match parse_helper(tokens, index) {
                        ParseResult::Success(idx, expr) => {
                            exprs.push(expr);
                            index = idx;
                        },
                        e => return e
                    }
                    if index >= tokens.len() {
                        return ParseResult::Failure(ParseError::BadParse("Unclosed Demiliter".to_string()))
                    }
                    t = &tokens[index]
                }

                return ParseResult::Success(index+1, Expr::list(&exprs))

            },
            Token::RPar => {
                // For example (+ 1 3)
                return ParseResult::Failure(ParseError::BadParse("Unexpected ) encountered".to_string()))
            },
            Token::Literal(s) => {
                if let Ok(n) = s.parse::<f64>() {
                    return ParseResult::Success(index+1, Expr::fnum(n))
                } else {
                    return ParseResult::Success(index+1, Expr::symbol(&s))
                }
            }
        }
    } else {
        ParseResult::Failure(ParseError::EOF)
    }
}

pub fn parse(tokens: &[Token]) -> Result<Rc<Expr>, ParseError> {
    match parse_helper(tokens, 0 as usize) {
        ParseResult::Success(_, r) => {
            return Ok(r)
        },
        ParseResult::Failure(e) => return Err(e)
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
