use std::{env, process};
use stac_calc::calculate;

fn main() {
    let args = env::args().skip(1);
    let expr: String = args.collect();
    if expr.is_empty() {
        eprintln!("Hey, you did not write any expression!");
        process::exit(1);
    }

    let result = calculate(expr.as_str()).unwrap_or_else(|err| {
        eprintln!("Sorry, we could not calculate your expression: {}", err);
        process::exit(1);
    });

    println!("{}", result);
}
