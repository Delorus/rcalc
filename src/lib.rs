use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::str::Chars;

use parser::Tokenized;

use crate::parser::Token;

mod parser;

//type Priority = u8;
//
//pub struct Operator {
//    symbol: String,
//    priority: Priority
//}
//
//static PLUS: Operator = Operator { symbol: "".to_string(), priority: 0 };
//static MINUS: Operator = Operator { symbol: "".to_string(), priority: 0 };
//static DIVIDE: Operator = Operator { symbol: "".to_string(), priority: 0 };
//static MULTIPLY: Operator = Operator { symbol: "".to_string(), priority: 0 };

#[derive(Debug)]
pub struct FormulaParsingError(String);

impl Error for FormulaParsingError {}

impl Display for FormulaParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

type CalcResult = std::result::Result<f32, Box<dyn Error>>;

pub fn calculate(expr: &str) -> CalcResult {
    assert!(!expr.is_empty());


    let tokens = to_prefix_notation(expr)?;

    let mut result = Vec::new();
    for token in tokens.iter() {
        match token {
            Token::Number(num) => {
                result.push(num.clone());
            }

            Token::Operator { priority, sign } => {
                let y = result.pop();
                let x = result.pop();
                if y.is_none() {
                    return Err(FormulaParsingError(format!("not found number for operator: {}", sign)).into());
                }
                let y = y.unwrap().clone();
                let sign = sign.as_str();
                match sign {
                    "+" => {
                        if let Some(x) = x {
                            result.push(x + y);
                        } else {
                            result.push(y);
                        }
                    }

                    "-" => {
                        if let Some(x) = x {
                            result.push(x - y);
                        } else {
                            result.push(y * -1.0);
                        }
                    }

                    "*" => {
                        let x = x.ok_or(FormulaParsingError(format!("expected at least two operand, but got one: ?*{}", y)))?;
                        result.push(x * y)
                    }

                    "/" => {
                        let x = x.ok_or(FormulaParsingError(format!("expected at least two operand, but got one: ?/{}", y)))?;
                        result.push(x / y)
                    }

                    _ => panic!("Bug in your code! Found operator '{:?}', that could not be processed", token)
                }
            }

            _ => panic!("Bug in your code! Found token '{:?}', that could not be processed", token)
        }
    }

    if result.len() > 1 {
        return Err(FormulaParsingError(format!("incorrect expression: two or more operands without operators: {:?}", result)).into());
    }

    return Ok(result.pop().unwrap_or(0.0));
}

fn to_prefix_notation(expr: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut ops: Vec<Token> = Vec::new();
    let mut result: Vec<Token> = Vec::new();

    for token in expr.chars().tokenized() {
        match token {
            Token::Number(_) => result.push(token),
            Token::Operator { priority, sign } => {
                loop {
                    if let Some(last) = ops.last() {
                        if last == &Token::ParenthesesOpen {
                            ops.push(Token::Operator { priority, sign });
                            break;
                        }

                        if let Token::Operator { priority: last_p, sign: _ } = last {
                            if last_p <= &priority {
                                result.push(ops.pop().take().unwrap());
                            } else {
                                ops.push(Token::Operator { priority, sign });
                                break;
                            }
                        }
                    } else {
                        ops.push(Token::Operator { priority, sign });
                        break;
                    }
                }
            }

            Token::ParenthesesOpen => ops.push(Token::ParenthesesOpen),
            Token::ParenthesesEnd => {
                loop {
                    match ops.pop().ok_or(FormulaParsingError(format!("incorrect expression: no open parentheses found: {:?}", result)))? {
                        t @ Token::Operator { priority: _, sign: _ } => result.push(t),
                        Token::ParenthesesOpen => break,

                        _ => panic!("Bug in your code! Found token '{:?}', that could not be processed", token)
                    };
                }
            }

            _ => panic!("Bug in your code! Found token '{:?}', that could not be processed", token)
        };
    }

    while let Some(token) = ops.pop() {
        if token == Token::ParenthesesOpen {
            return Err(FormulaParsingError(format!("incorrect expression: no close parentheses found: {:?}", result)).into())
        }

        result.push(token);
    }

    return Ok(result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_prefix_notation_test() {
        for (expr, expected) in vec![
            ("1+2", "1 2 +"),
            ("1 + 2 * 3", "1 2 3 * +"),
            ("1 * 2 + 3", "1 2 * 3 +"),
            ("1 + 2 - 3", "1 2 + 3 -"),
        ] {
            let res = to_prefix_notation(expr).unwrap();
            assert_eq!(to_string(res), expected)
        }
    }

    #[test]
    fn prefix_notation_allow_parentheses() {
        for (expr, expected) in vec![
            ("3 * (1 + 2)", "3 1 2 + *"),
            ("(1 + 2) * (3 + 4)", "1 2 + 3 4 + *"),
            ("(1 * (2 + 3) * 4)", "1 2 3 + * 4 *"),
        ] {
            let res = to_prefix_notation(expr).unwrap();
            assert_eq!(to_string(res), expected)
        }
    }

    fn to_string(tokens: Vec<Token>) -> String {
        tokens.iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    }

    #[test]
    fn if_get_one_number_return_this_number() {
        match calculate("0") {
            Ok(actual) => assert_eq!(actual, 0.0),
            Err(e) => panic!("return one number failed: {}", e),
        }
    }

    #[test]
    fn error_on_incorrect_number() {
        if let Ok(x) = calculate("1.,2") {
            panic!("must be error, but got: {}", x);
        }
    }

    #[test]
    fn calc_two_number() {
        for (expr, expected) in vec![
            ("1+2", 3.0),
            ("42+66", 108.0),
            ("99+00", 99.0),
            ("99+01", 100.0),
            ("3.2 + 0.1", 3.3),
            ("1 - 3", -2.0),
            ("1 - 0.5", 0.5),
            ("3 - 2", 1.0),
        ] {
            match calculate(expr) {
                Ok(actual) => assert_eq!(actual, expected, "in {}", expr),
                Err(e) => panic!("fail calc {}: {}", expr, e),
            }
        }
    }

    #[test]
    fn support_operations() {
        for (expr, expected) in vec![
            ("1+2", 3.0),
            ("1-2", -1.0),
            ("1*2", 2.0),
            ("1/2", 0.5),
        ] {
            match calculate(expr) {
                Ok(actual) => assert_eq!(actual, expected, "in {}", expr),
                Err(e) => panic!("fail calc {}: {}", expr, e),
            }
        }
    }

    #[test]
    fn support_unary_operations() {
        for (expr, expected) in vec![
            ("+2", 2.0),
            ("+ 42", 42.0),
            ("-10", -10.0),
            ("-0", 0.0),
            ("+0", 0.0),
            ("+1", 1.0),
            ("-1", -1.0),
            ("1+", 1.0),
        ] {
            match calculate(expr) {
                Ok(actual) => assert_eq!(actual, expected, "in {}", expr),
                Err(e) => panic!("fail calc {}: {}", expr, e),
            }
        }
    }

    #[test]
    fn unsupported_unary_operations() {
        for expr in vec!["++2", "--2", "*2", "/2"] {
            if let Ok(x) = calculate(expr) {
                panic!("expected error, but got value: {}", x);
            }
        }
    }

    #[test]
    fn supported_parentheses() {
        for (expr, expected) in vec![
            ("(1+2)", 3.0),
            ("(1+2) / (3+4)", 0.428571429),
            ("(1 / ((2 + 3) * 4))", 0.05),
        ] {
            match calculate(expr) {
                Ok(actual) => assert_eq!(actual, expected, "in {}", expr),
                Err(e) => panic!("fail calc {}: {}", expr, e),
            }
        }
    }

    #[test]
    fn invalid_expression() {
        for expr in vec![
            ("(1+2"),
            ("1+2)"),
            ("1/"),
        ] {
            if let Ok(actual) = calculate(expr) {
                panic!("expected error, but got: {}", actual)
            }
        }
    }

    #[test]
    #[ignore("not supported")]
    fn mixed_unary_and_binary_operations() {
        for (expr, expected) in vec![
            ("2++2", 4.0),
            ("2+-2", 0.0),
            ("-2+-2", -4.0),
            ("-0+1", 1.0),
            ("+0-1", -1.0),
            ("2--2", 4.0),
            ("0-0", 0.0),
        ] {
            match calculate(expr) {
                Ok(actual) => assert_eq!(actual, expected, "in {}", expr),
                Err(e) => panic!("fail calc {}: {}", expr, e),
            }
        }
    }
}