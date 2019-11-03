use std::{env, process, io};
use stac_calc::calculate;

fn main() {
    let expr = get_expression();

    let result = calculate(expr.as_str()).unwrap_or_else(|err| {
        eprintln!("Sorry, we could not calculate your expression: {}", err);
        process::exit(1);
    });

    println!("{}", result);
}

fn get_expression() -> String {
    let args = env::args().skip(1);
    let mut expr: String = args.collect();
    if expr.is_empty() {
        io::stdin().read_line(&mut expr).unwrap_or_else(|err| {
            eprintln!("Sorry, something went wrong: {}", err);
            process::exit(1);
        });
    }

    return expr;
}