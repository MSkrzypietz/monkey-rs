use std::io::{BufReader, Result, Read, Write, BufRead};
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct Repl {}

impl Repl {
    pub fn start(r: Box<dyn Read>, mut w: Box<dyn Write>) -> Result<()> {
        let mut reader = BufReader::new(r);

        loop {
            write!(w, ">> ")?;
            w.flush()?;

            let mut input = String::new();
            reader.read_line(&mut input)?;
            let lexer = Lexer::new(&input);
            let mut parser = Parser::new(lexer);

            for error in parser.errors() {
                writeln!(w, "{}", error)?;
            }

            let program = parser.parse_program();
            for stmt in program {
                writeln!(w, "{}", stmt)?;
            }
        }
    }
}