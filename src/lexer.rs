use std::iter::Peekable;
use std::str::Chars;
use crate::token::{Token, TokenKind};

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    char: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            chars: input.chars().peekable(),
            char: None,
        };
        lexer.read_char();
        lexer
    }

    fn next_token(&mut self) -> Token {
        let token = match self.char {
            Some('=') => Token::new(TokenKind::Assign, "=".to_string()),
            Some(';') => Token::new(TokenKind::Semicolon, ";".to_string()),
            Some('(') => Token::new(TokenKind::Lparen, "(".to_string()),
            Some(')') => Token::new(TokenKind::Rparen, ")".to_string()),
            Some(',') => Token::new(TokenKind::Comma, ",".to_string()),
            Some('+') => Token::new(TokenKind::Plus, "+".to_string()),
            Some('{') => Token::new(TokenKind::Lbrace, "{".to_string()),
            Some('}') => Token::new(TokenKind::Rbrace, "}".to_string()),
            _ => Token::new(TokenKind::Eof, "".to_string())
        };

        self.read_char();
        token
    }

    fn read_char(&mut self) {
        self.char = self.chars.next();
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenKind;
    use super::*;

    #[test]
    fn test_next_token() {
        struct TestResult {
            expected_kind: TokenKind,
            expected_literal: &'static str,
        }
        let tests: &[TestResult] = &[
            TestResult { expected_kind: TokenKind::Assign, expected_literal: "=" },
            TestResult { expected_kind: TokenKind::Plus, expected_literal: "+" },
            TestResult { expected_kind: TokenKind::Lparen, expected_literal: "(" },
            TestResult { expected_kind: TokenKind::Rparen, expected_literal: ")" },
            TestResult { expected_kind: TokenKind::Lbrace, expected_literal: "{" },
            TestResult { expected_kind: TokenKind::Rbrace, expected_literal: "}" },
            TestResult { expected_kind: TokenKind::Comma, expected_literal: "," },
            TestResult { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestResult { expected_kind: TokenKind::Eof, expected_literal: "" },
        ];

        let mut lexer = Lexer::new("=+(){},;");

        for (i, test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(test.expected_kind, token.kind, "tests[{}] - token kind wrong. expected={:?}, got={:?}", i, test.expected_kind, token.kind);
            assert_eq!(test.expected_literal, token.literal, "tests[{}] - literal wrong. expected={}, got={}", i, test.expected_literal, token.literal);
        }
    }
}