use std::iter::Peekable;
use crate::ast::{Expr, Ident, Infix, Prefix, Program, Stmt};
use crate::ast::Expr::{ArrayExpr, BooleanExpr, FunctionCallExpr, FunctionLiteralExpr, IdentExpr, IfExpr, IndexExpr, InfixExpr, IntExpr, PrefixExpr, StringExpr};
use crate::ast::Stmt::{ExprStmt, LetStmt, ReturnStmt};
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
const INDEX: Precedence = 8;

fn precedence(token: &Token) -> Option<Precedence> {
    match token {
        Token::Eq => Some(EQUALS),
        Token::Ne => Some(EQUALS),
        Token::Lt => Some(LESS_GREATER),
        Token::Gt => Some(LESS_GREATER),
        Token::Plus => Some(SUM),
        Token::Minus => Some(SUM),
        Token::Slash => Some(PRODUCT),
        Token::Asterisk => Some(PRODUCT),
        Token::Lparen => Some(CALL),
        Token::Lbracket => Some(INDEX),
        _ => None
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    curr_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
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
            Token::Ident(_) => self.parse_ident_expression(),
            Token::Int(_) => self.parse_int_expression(),
            Token::String(_) => self.parse_string_expression(),
            Token::Minus => self.parse_prefix_expression(),
            Token::Bang => self.parse_prefix_expression(),
            Token::True => Some(BooleanExpr(true)),
            Token::False => Some(BooleanExpr(false)),
            Token::Lparen => self.parse_grouped_expression(),
            Token::Lbracket => self.parse_array_literal(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal_expression(),
            _ => None
        }
    }

    fn peek_infix_is(&mut self) -> bool {
        match self.lexer.peek() {
            Some(Token::Plus) => true,
            Some(Token::Minus) => true,
            Some(Token::Asterisk) => true,
            Some(Token::Slash) => true,
            Some(Token::Gt) => true,
            Some(Token::Lt) => true,
            Some(Token::Eq) => true,
            Some(Token::Ne) => true,
            Some(Token::Lparen) => true,
            Some(Token::Lbracket) => true,
            _ => false
        }
    }

    pub fn errors(&self) -> &[String] {
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

    pub fn parse_program(&mut self) -> Program {
        let mut prog = Vec::new();

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
        self.expect_peek(&Token::Assign)?;
        self.next_token();
        let expr = self.parse_expression(LOWEST)?;

        if !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(LetStmt(ident, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();
        let expr = self.parse_expression(LOWEST)?;
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }
        Some(ReturnStmt(expr))
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
        let mut left = self.execute_prefix()?;

        while !self.peek_token_is(&Token::Semicolon) && self.smaller_precedence(precedence) {
            if !self.peek_infix_is() {
                return Some(left);
            }

            self.next_token();
            left = self.parse_infix_expression(left)?;
        }

        Some(left)
    }

    fn parse_ident_expression(&self) -> Option<Expr> {
        match &self.curr_token {
            Token::Ident(ident) => Some(IdentExpr(Ident(ident.clone()))),
            _ => None
        }
    }

    fn parse_ident(&self) -> Option<Ident> {
        match &self.curr_token {
            Token::Ident(ident) => Some(Ident(ident.clone())),
            _ => None
        }
    }

    fn parse_int_expression(&self) -> Option<Expr> {
        match &self.curr_token {
            Token::Int(x) => {
                match x.parse::<i64>() {
                    Ok(x) => Some(IntExpr(x)),
                    Err(_) => None
                }
            }
            _ => None
        }
    }

    fn parse_string_expression(&self) -> Option<Expr> {
        match &self.curr_token {
            Token::String(str) => Some(StringExpr(str.clone())),
            _ => None
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expr> {
        let prefix = match self.curr_token {
            Token::Minus => Some(Prefix::Minus),
            Token::Bang => Some(Prefix::Bang),
            _ => None
        }?;
        self.next_token();
        let expr = self.parse_expression(PREFIX)?;
        Some(PrefixExpr(prefix, Box::new(expr)))
    }

    fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.curr_token {
            Token::Plus => Some(Infix::Plus),
            Token::Minus => Some(Infix::Minus),
            Token::Asterisk => Some(Infix::Asterisk),
            Token::Slash => Some(Infix::Slash),
            Token::Gt => Some(Infix::Gt),
            Token::Lt => Some(Infix::Lt),
            Token::Eq => Some(Infix::Eq),
            Token::Ne => Some(Infix::Ne),
            Token::Lparen => return self.parse_function_call_expression(left),
            Token::Lbracket => return self.parse_array_index_expression(left),
            _ => None
        }?;
        let precedence = self.curr_precedence()?;
        self.next_token();
        let expr = self.parse_expression(precedence)?;
        Some(InfixExpr(infix, Box::new(left), Box::new(expr)))
    }

    fn parse_function_call_expression(&mut self, function: Expr) -> Option<Expr> {
        Some(FunctionCallExpr { function: Box::new(function), arguments: self.parse_expression_list(Token::Rparen) })
    }

    fn parse_array_index_expression(&mut self, left: Expr) -> Option<Expr> {
        self.next_token();
        let index = self.parse_expression(LOWEST)?;
        if self.expect_peek(&Token::Rbracket).is_none() {
            return None;
        }
        Some(IndexExpr { left: Box::new(left), index: Box::new(index) })
    }

    fn parse_array_literal(&mut self) -> Option<Expr> {
        Some(ArrayExpr(self.parse_expression_list(Token::Rbracket)))
    }

    fn parse_expression_list(&mut self, end: Token) -> Vec<Expr> {
        let mut list: Vec<Expr> = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return list;
        }

        self.next_token();
        if let Some(expr) = self.parse_expression(LOWEST) {
            list.push(expr);
        } else {
            return list;
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            if let Some(expr) = self.parse_expression(LOWEST) {
                list.push(expr);
            }
        }

        if self.expect_peek(&end).is_none() {
            return Vec::new();
        }

        list
    }

    fn parse_grouped_expression(&mut self) -> Option<Expr> {
        self.next_token();
        let expr = self.parse_expression(LOWEST);
        match self.expect_peek(&Token::Rparen) {
            Some(true) => expr,
            _ => None,
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expr> {
        self.expect_peek(&Token::Lparen)?;
        self.next_token();
        let cond = self.parse_expression(LOWEST)?;
        self.expect_peek(&Token::Rparen)?;

        self.expect_peek(&Token::Lbrace)?;
        let consequence = self.parse_block_statement().unwrap_or_default();

        let alternative: Program = if self.peek_token_is(&Token::Else) {
            self.next_token();
            self.expect_peek(&Token::Lbrace)?;
            self.parse_block_statement().unwrap_or_default()
        } else {
            Program::default()
        };

        Some(IfExpr {
            cond: Box::new(cond),
            consequence,
            alternative,
        })
    }

    fn parse_function_literal_expression(&mut self) -> Option<Expr> {
        self.expect_peek(&Token::Lparen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&Token::Lbrace)?;
        let body = self.parse_block_statement()?;

        Some(FunctionLiteralExpr { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Ident>> {
        let mut parameters: Vec<Ident> = vec![];
        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Some(parameters);
        }

        self.next_token();
        parameters.push(self.parse_ident()?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            parameters.push(self.parse_ident()?);
        }

        self.expect_peek(&Token::Rparen)?;
        Some(parameters)
    }

    fn parse_block_statement(&mut self) -> Option<Program> {
        self.next_token();
        let mut program = Vec::new();
        while !self.curr_token_is(&Token::Rbrace) && !self.curr_token_is(&Token::Eof) {
            if let Some(stmt) = self.parse_stmt() {
                program.push(stmt);
            }
            self.next_token()
        }
        Some(program)
    }

    fn smaller_precedence(&mut self, other: Precedence) -> bool {
        match self.peek_precedence() {
            Some(precedence) => other < precedence,
            None => false
        }
    }

    fn peek_precedence(&mut self) -> Option<Precedence> {
        self.lexer.peek().iter().find_map(|token| precedence(token)).or(Some(LOWEST))
    }

    fn curr_precedence(&self) -> Option<Precedence> {
        precedence(&self.curr_token).or(Some(LOWEST))
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
    use crate::ast::{Ident, Infix, Prefix};
    use crate::ast::Expr::{ArrayExpr, BooleanExpr, FunctionCallExpr, FunctionLiteralExpr, IfExpr, IndexExpr, InfixExpr, IntExpr, PrefixExpr, StringExpr};
    use crate::ast::Stmt::{ExprStmt, LetStmt};

    use super::*;

    #[test]
    fn test_let_statements() {
        let expected_stmts: Vec<Stmt> = vec![
            LetStmt(Ident("x".to_string()), IntExpr(5)),
            LetStmt(Ident("y".to_string()), IntExpr(10)),
            LetStmt(Ident("foobar".to_string()), BooleanExpr(true)),
        ];

        let input = "
            let x = 5;
            let y = 10;
            let foobar = true;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_return_statements() {
        let expected_stmts: Vec<Stmt> = vec![
            ReturnStmt(IntExpr(123)),
            ReturnStmt(IdentExpr(Ident("add".to_string()))),
        ];

        let input = "
            return 123
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
            ExprStmt(IntExpr(5)),
            ExprStmt(StringExpr("hello world".to_string())),
            ExprStmt(PrefixExpr(Prefix::Bang, Box::new(IntExpr(5)))),
            ExprStmt(PrefixExpr(Prefix::Minus, Box::new(IntExpr(15)))),
            ExprStmt(InfixExpr(Infix::Plus, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Minus, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Asterisk, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Slash, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Gt, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Lt, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Eq, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(InfixExpr(Infix::Ne, Box::new(IntExpr(5)), Box::new(IntExpr(5)))),
            ExprStmt(BooleanExpr(true)),
            ExprStmt(BooleanExpr(false)),
            ExprStmt(InfixExpr(Infix::Eq, Box::new(InfixExpr(Infix::Gt, Box::new(IntExpr(3)), Box::new(IntExpr(5)))), Box::new(BooleanExpr(false)))),
        ];

        let input = "
            foobar;
            5;
            \"hello world\";
            !5;
            -15;
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 == 5;
            5 != 5;
            true;
            false;
            3 > 5 == false;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_operator_precedence() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(InfixExpr(Infix::Asterisk, Box::new(InfixExpr(Infix::Plus, Box::new(IntExpr(5)), Box::new(IntExpr(5)))), Box::new(IntExpr(2)))),
            ExprStmt(PrefixExpr(Prefix::Bang, Box::new(InfixExpr(Infix::Eq, Box::new(BooleanExpr(true)), Box::new(BooleanExpr(false)))))),
        ];

        let input = "
            (5 + 5) * 2;
            !(true == false);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_if_expressions() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(IfExpr {
                cond: Box::new(InfixExpr(Infix::Lt, Box::new(IdentExpr(Ident("x".to_string()))), Box::new(IdentExpr(Ident("y".to_string()))))),
                consequence: vec![ExprStmt(IdentExpr(Ident("x".to_string())))],
                alternative: vec![],
            }),
            ExprStmt(IfExpr {
                cond: Box::new(InfixExpr(Infix::Lt, Box::new(IdentExpr(Ident("x".to_string()))), Box::new(IdentExpr(Ident("y".to_string()))))),
                consequence: vec![ExprStmt(IdentExpr(Ident("x".to_string())))],
                alternative: vec![ExprStmt(IdentExpr(Ident("y".to_string())))],
            }),
        ];

        let input = "
            if (x < y) { x };
            if (x < y) { x } else { y };";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_function_literal_expressions() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(FunctionLiteralExpr {
                parameters: vec![Ident("x".to_string()), Ident("y".to_string())],
                body: vec![ExprStmt(InfixExpr(Infix::Plus, Box::new(IdentExpr(Ident("x".to_string()))), Box::new(IdentExpr(Ident("y".to_string())))))],
            }),
            ExprStmt(FunctionLiteralExpr { parameters: vec![], body: Program::default() }),
            ExprStmt(FunctionLiteralExpr { parameters: vec![Ident("x".to_string())], body: Program::default() }),
            ExprStmt(FunctionLiteralExpr { parameters: vec![Ident("x".to_string()), Ident("y".to_string()), Ident("z".to_string())], body: Program::default() }),
        ];

        let input = "
            fn(x, y) { x + y; }
            fn() {}
            fn(x) {}
            fn(x, y, z) {}";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_function_call_expressions() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(FunctionCallExpr {
                function: Box::new(IdentExpr(Ident("add".to_string()))),
                arguments: vec![IntExpr(1), InfixExpr(Infix::Asterisk, Box::new(IntExpr(2)), Box::new(IntExpr(3))), InfixExpr(Infix::Plus, Box::new(IntExpr(4)), Box::new(IntExpr(5)))],
            }),
            ExprStmt(FunctionCallExpr {
                function: Box::new(IdentExpr(Ident("add".to_string()))),
                arguments: vec![
                    IntExpr(1),
                    InfixExpr(Infix::Plus, Box::new(FunctionCallExpr { function: Box::new(IdentExpr(Ident("add".to_string()))), arguments: vec![IntExpr(2), IntExpr(3)] }), Box::new(IntExpr(3))),
                ],
            }),
        ];

        let input = "
            add(1, 2 * 3, 4 + 5);
            add(1, add(2, 3) + 3);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_array_literals() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(ArrayExpr(vec![
                IntExpr(1),
                InfixExpr(Infix::Asterisk, Box::new(IntExpr(2)), Box::new(IntExpr(2))),
                InfixExpr(Infix::Plus, Box::new(IntExpr(3)), Box::new(IntExpr(3))),
            ]))
        ];

        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }

    #[test]
    fn test_index_expressions() {
        let expected_stmts: Vec<Stmt> = vec![
            ExprStmt(IndexExpr {
                left: Box::new(IdentExpr(Ident("myArray".to_string()))),
                index: Box::new(InfixExpr(Infix::Plus, Box::new(IntExpr(1)), Box::new(IntExpr(1)))),
            })
        ];

        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0);
        assert_eq!(program, expected_stmts);
    }
}