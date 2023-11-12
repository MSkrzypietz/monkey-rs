use std::iter::Peekable;
use crate::ast::{Ident, Program, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    curr_token: Token,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer: lexer.peekable(),
            curr_token: Token::Eof,
        };
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.curr_token = match self.lexer.next() {
            Some(token) => token,
            None => Token::Eof
        };
    }

    fn parse_program(&mut self) -> Program {
        let mut prog: Program = Vec::new();

        while self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                prog.push(stmt);
            }
            self.next_token()
        }

        prog
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match &self.curr_token {
            Token::Let => self.parse_let_stmt(),
            kind => panic!("unimplemented token kind: {:?}", kind)
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let ident = self.peek_ident()?;
        let stmt = Stmt::LetStmt(ident);

        self.expect_peek(Token::Assign)?;
        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn curr_token_is(&self, token: Token) -> bool {
        self.curr_token == token
    }

    fn peek_token_is(&mut self, token: Token) -> bool {
        match self.lexer.peek() {
            Some(peek_token) => peek_token == &token,
            None => false
        }
    }

    fn expect_peek(&mut self, token: Token) -> Option<bool> {
        if self.peek_token_is(token) {
            self.next_token();
            Some(true)
        } else {
            None
        }
    }

    fn peek_ident(&mut self) -> Option<Ident> {
        let ident = match self.lexer.peek() {
            Some(Token::Ident(x)) => Some(Ident(x.clone())),
            _ => None
        };

        if ident.is_some() {
            self.next_token();
        }

        ident
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Ident;
    use crate::ast::Stmt::LetStmt;
    use super::*;

    #[test]
    fn test_let_statements() {
        let expected_stmts: Vec<Stmt> = vec![
            LetStmt(Ident("x".to_string())),
            LetStmt(Ident("y".to_string())),
            LetStmt(Ident("foobar".to_string())),
        ];

        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program, expected_stmts);
    }
}