use std::io::{BufReader, Result, Read, Write, BufRead};
use crate::lexer::Lexer;

pub struct Repl {}

impl Repl {
    pub fn start(reader: Box<dyn Read>, mut writer: Box<dyn Write>) -> Result<()> {
        let mut reader = BufReader::new(reader);

        loop {
            writer.write_all(b">> ")?;
            writer.flush()?;

            let mut input = String::new();
            reader.read_line(&mut input)?;
            let lexer = Lexer::new(&input);
            for token in lexer {
                writer.write_all(format!("{:?}\n", token).as_bytes())?;
            }
        }
    }
}