use std::io::{BufReader, Result, Read, Write, BufRead};
use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct Repl {}

impl Repl {
    pub fn start(r: Box<dyn Read>, mut w: Box<dyn Write>) -> Result<()> {
        let mut reader = BufReader::new(r);
        let mut evaluator = Evaluator::new(Environment::new());

        loop {
            write!(w, ">> ")?;
            w.flush()?;

            let mut input = String::new();
            reader.read_line(&mut input)?;
            let lexer = Lexer::new(&input);
            let mut parser = Parser::new(lexer);

            let mut program = parser.parse_program();
            for error in parser.errors() {
                writeln!(w, "{}", error)?;
            }

            let evaluated = evaluator.eval_program(&mut program);
            writeln!(w, "{}", evaluated)?;
        }
    }
}