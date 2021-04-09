#[derive(Debug)]
pub enum Token {
    LPar,
    RPar,
    Literal(String),
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Literal(l1), Token::Literal(l2)) => l1 == l2,
            (Token::LPar, Token::LPar)
            | (Token::RPar, Token::RPar) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownToken(String),
}

fn add_whitespace(input: &str) -> String {
    input
        .chars()
        .fold(Vec::new(), |mut acc, c| {
            if c == '(' || c == ')' {
                acc.append(&mut vec![' ', c, ' ']);
            } else {
                acc.push(c);
            }
            acc
        })
        .iter()
        .collect()
}

/// Code from https://exercism.io/tracks/rust/exercises/matching-brackets/solutions/103e946f00924574bbd5c016b49d1037
pub fn matching_brackets(s: &str) -> bool {
    let mut open_brackets_stack = Vec::new();

    for c in s.chars() {
        match c {
            // opening a bracket/brace is always fine
            '{' | '(' | '[' => open_brackets_stack.push(c),
            // closing is only fine when it matches the most recent opened
            '}' | ')' | ']' => {
                if let Some(prev) = open_brackets_stack.pop() {
                    match (prev, c) {
                        // closing correct pair, all fine (already popped)
                        ('{', '}') | ('(', ')') | ('[', ']') => continue,
                        // closing a wrong pair, bail
                        _ => return false
                    }
                } else { // closing without any open pair, bail
                    return false
                }
            },
            // ignore anything that's not a bracket, for this exercise
            _ => continue
        }
    }

    return true
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let r = add_whitespace(input)
            .split_ascii_whitespace()
            .map(|p| {
                    match p {
                        "(" => Token::LPar,
                        ")" => Token::RPar,
                        _ => Token::Literal(p.to_string())
                    }
            })
            .collect();

    return Ok(r)
}

#[cfg(test)]
mod test {
    use super::*;
}
