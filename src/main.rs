use std::io::{stdin, stdout};
use crate::repl::Repl;

mod token;
mod lexer;
mod repl;

fn main() {
    println!("Welcome to the monkey programming language cli!\n");
    if let Err(err) = Repl::start(Box::new(stdin()), Box::new(stdout())) {
        println!("Error: {}", err)
    }
}
