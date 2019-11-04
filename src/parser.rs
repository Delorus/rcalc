use std::fmt::{Display, Error, Formatter};
use std::str::Chars;

use crate::parser::Token::{Number, Operator, ParenthesesEnd, ParenthesesOpen};

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f32),
    Operator {
        priority: u8,
        sign: String,
    },
    ParenthesesOpen,
    ParenthesesEnd,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Number(x) => write!(f, "{}", x),
            Token::Operator { sign, priority } => write!(f, "{}", sign),
            Token::ParenthesesOpen => write!(f, "("),
            Token::ParenthesesEnd => write!(f, ")"),
        }
    }
}

pub struct Tokenizer<'a> {
    chars: Chars<'a>,
    last_token: Option<String>,
    buff: String,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.load_next_to_buff();

        let raw_token = Option::from(self.buff.clone())
            .or(self.last_token.clone());
        if raw_token == None {
            return None;
        }
        let raw_token = raw_token.unwrap();

        return match raw_token.as_str() {
            c @ "+" | c @ "-" => Some(Operator {
                sign: c.to_string(),
                priority: 1,
            }),
            c @ "/" | c @ "*" => Some(Operator {
                sign: c.to_string(),
                priority: 0,
            }),
            "(" => Some(ParenthesesOpen),
            ")" => Some(ParenthesesEnd),

            // in other cases it's number
            num => return match num.parse() {
                Ok(x) => Some(Number(x)),
                Err(_) => None,
            },
        };
    }
}

impl Tokenizer<'_> {

    fn load_next_to_buff(&mut self) {
        self.buff.clear();

        while let Some(c) = self.chars.next() {
            if let ('0'..='9') | '.' = c {
                self.buff.push(c);

            } else if c == ' ' {
                if !self.buff.is_empty() {
                    break;
                }

            } else {
                if self.buff.is_empty() {
                    self.buff.push(c);
                } else {
                    self.last_token = Some(c.to_string());
                }
                break;
            }
        }
    }
}


pub trait Tokenized<'a> {
    fn tokenized(self) -> Tokenizer<'a>;
}

impl<'a> Tokenized<'a> for Chars<'a> {
    fn tokenized(self) -> Tokenizer<'a> {
        Tokenizer {
            chars: self,
            last_token: None,
            buff: String::with_capacity(5),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_one_number() {
        assert_token(Number(42.0), "42");
    }

    #[test]
    fn parse_several_numbers() {
        assert_tokens(&[Number(42.0), Number(69.0)], "42 69")
    }

    #[test]
    fn parse_float_number() {
        assert_token(Number(42.66), "42.66")
    }

    #[test]
    fn parse_operator() {
        assert_token(Operator { priority: 1, sign: "+".into() }, "+");
        assert_token(Operator { priority: 1, sign: "-".into() }, "-");
        assert_token(Operator { priority: 0, sign: "*".into() }, "*");
        assert_token(Operator { priority: 0, sign: "/".into() }, "/");
    }

    #[test]
    fn parse_mixed_tokens() {
        assert_tokens(
            &[
                Number(1.23),
                Operator { priority: 1, sign: "+".into() },
                Number(42.0)
            ],
            "1.23 + 42")
    }

    #[test]
    fn support_braces() {
        assert_token(ParenthesesOpen, "(");
        assert_token(ParenthesesEnd, ")");
        assert_tokens(&[ParenthesesOpen, ParenthesesEnd], "()");
    }

    fn assert_token(expected: Token, expr: &str) {
        let token: Token = expr.chars().tokenized().next().expect(format!("expected: {} in {}", expected, expr).as_str());
        assert_eq!(token, expected);
    }

    fn assert_tokens(expected: &[Token], expr: &str) {
        let mut actual = Vec::with_capacity(expected.len());
        for (i, token) in expr.chars().tokenized().enumerate() {
            assert_eq!(token, expected[i], "{}", expr);
            actual.push(token);
        }

        if actual.len() != expected.len() {
            panic!("not all tokens were received, expected: {}, got: {}", to_string(expected), to_string(&actual))
        }
    }

    fn to_string(s: &[Token]) -> String {
        format!("[{}]", s.iter()
            .fold(String::new(), |s, t| {
                if s.is_empty() {
                    t.to_string()
                } else {
                    format!("{} {}", s, t.to_string())
                }
            }))
    }
}