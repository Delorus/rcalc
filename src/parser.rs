use std::str::Chars;

use crate::parser::Token::{Number, Operator, ParenthesesOpen, ParenthesesEnd};
use std::fmt::{Display, Formatter, Error};

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
    last_token: Option<char>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.last_token.take() {
            match c {
                '+' | '-' => return Some(Operator {
                    sign: c.to_string(),
                    priority: 1,
                }),
                '/' | '*' => return Some(Operator {
                    sign: c.to_string(),
                    priority: 0,
                }),
                '(' => return Some(ParenthesesOpen),
                ')' => return Some(ParenthesesEnd),
                _ => {}
            };
        }

        let mut buff = String::with_capacity(5);
        loop {
            match self.chars.next() {
                Some(c) => {
                    if let ('0'..='9') | '.' = c {
                        buff.push(c);
                    } else {
                        if !buff.is_empty() {
                            self.last_token = Some(c);
                            return match buff.parse() {
                                Ok(x) => Some(Number(x)),
                                Err(_) => None,
                            };
                        }

                        return match c {
                            '+' | '-' => Some(Operator {
                                sign: c.to_string(),
                                priority: 1,
                            }),
                            '/' | '*' => Some(Operator {
                                sign: c.to_string(),
                                priority: 0,
                            }),
                            '(' => return Some(ParenthesesOpen),
                            ')' => return Some(ParenthesesEnd),
                            ' ' => continue,

                            _ => None,
                        };
                    }
                }
                None => {
                    if buff.is_empty() {
                        return None;
                    }

                    return match buff.parse() {
                        Ok(x) => Some(Number(x)),
                        Err(_) => None,
                    };
                }
            }
        }
    }
}

pub trait Tokenized<'a> {
    fn tokenized(self) -> Tokenizer<'a>;
}

impl<'a> Tokenized<'a> for Chars<'a> {
    fn tokenized(self) -> Tokenizer<'a> {
        Tokenizer { chars: self, last_token: None }
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
            "1.23+42")
    }

    #[test]
    fn support_braces() {
        assert_token(ParenthesesOpen, "(");
        assert_token(ParenthesesEnd, ")");
        assert_tokens(&[ParenthesesOpen, ParenthesesEnd], "()");
    }

    fn assert_token(expected: Token, actual: &str) {
        let token: Token = actual.chars().tokenized().next().unwrap();
        assert_eq!(token, expected);
    }

    fn assert_tokens(expected: &[Token], actual: &str) {
        for (i, token) in actual.chars().tokenized().enumerate() {
            assert_eq!(token, expected[i]);
        }
    }
}