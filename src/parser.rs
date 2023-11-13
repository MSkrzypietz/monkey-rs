use std::iter::Peekable;
use crate::ast::{Expr, Ident, Program, Stmt};
use crate::ast::Expr::{IdentExpr, IntExpr};
use crate::ast::Stmt::{ExprStmt, ReturnStmt};
use crate::lexer::Lexer;
use crate::token::Token;

type Precedence = u8;

const LOWEST: Precedence = 1;
const EQUALS: Precedence = 2;
const LESS_GREATER: Precedence = 3;
const SUM: Precedence = 4;
const PRODUCT: Precedence = 5;
const PREFIX: Precedence = 6;
const CALL: Precedence = 7;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    curr_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer: lexer.peekable(),
            curr_token: Token::Eof,
            errors: Vec::new(),
        };
        p.next_token();
        p
    }

    fn execute_prefix(&mut self) -> Option<Expr> {
        match self.curr_token {
            Token::Ident(_) => self.parse_ident(),
            Token::Int(_) => self.parse_int(),
            _ => None
        }
    }

    fn errors(&self) -> &[String] {
        &self.errors
    }

    fn peek_error(&mut self, token: &Token) {
        let msg = format!("expected next token to be {:?}, got {:?} instead", token, self.curr_token);
        self.errors.push(msg);
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
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expression_stmt()
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let ident = self.peek_ident()?;
        let stmt = Stmt::LetStmt(ident);

        self.expect_peek(&Token::Assign)?;
        while !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        while !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(ReturnStmt)
    }

    fn parse_expression_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(LOWEST)?;
        let stmt = ExprStmt(expr);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        self.execute_prefix()
    }

    fn parse_ident(&self) -> Option<Expr> {
        match &self.curr_token {
            Token::Ident(ident) => Some(IdentExpr(Ident(ident.clone()))),
            _ => None
        }
    }

    fn parse_int(&self) -> Option<Expr> {
        match &self.curr_token {
            Token::Int(x) => {
                match x.parse::<i64>() {
                    Ok(x) => Some(IntExpr(x)),
                    Err(_) => None
                }
            },
            _ => None
        }
    }

    fn curr_token_is(&self, token: &Token) -> bool {
        &self.curr_token == token
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        match self.lexer.peek() {
            Some(peek_token) => peek_token == token,
            None => false
        }
    }

    fn expect_peek(&mut self, token: &Token) -> Option<bool> {
        if self.peek_token_is(token) {
            self.next_token();
            Some(true)
        } else {
            self.peek_error(token);
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
        } else {
            self.peek_error(&Token::Ident("".to_string()))
        }

        ident
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr::IntExpr;
    use crate::ast::Ident;
    use crate::ast::Stmt::{ExprStmt, LetStmt};
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

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_return_statements() {
        let expected_stmts: Vec<Stmt> = vec![
            ReturnStmt,
            ReturnStmt,
        ];

        let input = "
            return 123;
            return add;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_expression_statements() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(IdentExpr(Ident("foobar".to_string()))),
            ExprStmt(IntExpr(5))
        ];

        let input = "\
            foobar;\
            5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }
}