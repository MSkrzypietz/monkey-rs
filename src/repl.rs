use std::io::{BufReader, Result, Read, Write, BufRead};
use crate::lexer::Lexer;
use crate::token::TokenKind;

pub struct Repl {}

impl Repl {
    pub fn start(reader: Box<dyn Read>, mut writer: Box<dyn Write>) -> Result<()> {
        let mut reader = BufReader::new(reader);

        loop {
            writer.write_all(b">> ")?;
            writer.flush()?;

            let mut input = String::new();
            reader.read_line(&mut input)?;

            let mut lexer = Lexer::new(&input);
            while let token = lexer.next_token() {
                match token.kind {
                    TokenKind::Eof => break,
                    _ => { writer.write_all(format!("{:?}\n", token).as_bytes())?; }
                }
            }
        }
    }
}