use std::iter::Peekable;
use std::str::Chars;
use crate::token::{lookup_ident, Token, TokenKind};

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            chars: input.chars().peekable(),
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some('=') => {
                match self.chars.peek() {
                    Some('=') => {
                        self.read_char();
                        Token::new(TokenKind::Eq, "==".to_string())
                    }
                    _ => Token::new(TokenKind::Assign, "=".to_string())
                }
            }
            Some(';') => Token::new(TokenKind::Semicolon, ";".to_string()),
            Some('(') => Token::new(TokenKind::Lparen, "(".to_string()),
            Some(')') => Token::new(TokenKind::Rparen, ")".to_string()),
            Some(',') => Token::new(TokenKind::Comma, ",".to_string()),
            Some('+') => Token::new(TokenKind::Plus, "+".to_string()),
            Some('-') => Token::new(TokenKind::Minus, "-".to_string()),
            Some('!') => {
                match self.chars.peek() {
                    Some('=') => {
                        self.read_char();
                        Token::new(TokenKind::Ne, "!=".to_string())
                    }
                    _ => Token::new(TokenKind::Bang, "!".to_string())
                }
            }
            Some('*') => Token::new(TokenKind::Asterisk, "*".to_string()),
            Some('/') => Token::new(TokenKind::Slash, "/".to_string()),
            Some('<') => Token::new(TokenKind::Lt, "<".to_string()),
            Some('>') => Token::new(TokenKind::Gt, ">".to_string()),
            Some('{') => Token::new(TokenKind::Lbrace, "{".to_string()),
            Some('}') => Token::new(TokenKind::Rbrace, "}".to_string()),
            None => Token::new(TokenKind::Eof, "".to_string()),
            Some(ch) => {
                if Self::is_letter(ch) {
                    let ident = self.read_identifier();
                    return Token::new(lookup_ident(&ident), ident);
                } else if Self::is_digit(ch) {
                    return Token::new(TokenKind::Int, self.read_number());
                } else {
                    Token::new(TokenKind::Illegal, ch.to_string())
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        self.read_while(Self::is_letter)
    }

    fn read_number(&mut self) -> String {
        self.read_while(Self::is_digit)
    }

    fn read_while<F>(&mut self, predicate: F) -> String
        where F: Fn(char) -> bool
    {
        let mut ident = String::new();
        while let Some(ch) = self.ch {
            if predicate(ch) {
                ident.push(ch);
                self.read_char();
            } else {
                break;
            }
        }
        ident
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn is_letter(ch: char) -> bool {
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }

    fn is_digit(ch: char) -> bool {
        '0' <= ch && ch <= '9'
    }

    fn read_char(&mut self) {
        self.ch = self.chars.next();
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenKind;
    use super::*;

    #[test]
    fn test_next_token() {
        struct TestCase {
            expected_kind: TokenKind,
            expected_literal: &'static str,
        }
        let tests: &[TestCase] = &[
            TestCase { expected_kind: TokenKind::Let, expected_literal: "let" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "five" },
            TestCase { expected_kind: TokenKind::Assign, expected_literal: "=" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "5" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Let, expected_literal: "let" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "ten" },
            TestCase { expected_kind: TokenKind::Assign, expected_literal: "=" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Let, expected_literal: "let" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "add" },
            TestCase { expected_kind: TokenKind::Assign, expected_literal: "=" },
            TestCase { expected_kind: TokenKind::Function, expected_literal: "fn" },
            TestCase { expected_kind: TokenKind::Lparen, expected_literal: "(" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "x" },
            TestCase { expected_kind: TokenKind::Comma, expected_literal: "," },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "y" },
            TestCase { expected_kind: TokenKind::Rparen, expected_literal: ")" },
            TestCase { expected_kind: TokenKind::Lbrace, expected_literal: "{" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "x" },
            TestCase { expected_kind: TokenKind::Plus, expected_literal: "+" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "y" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Rbrace, expected_literal: "}" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Let, expected_literal: "let" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "result" },
            TestCase { expected_kind: TokenKind::Assign, expected_literal: "=" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "add" },
            TestCase { expected_kind: TokenKind::Lparen, expected_literal: "(" },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "five" },
            TestCase { expected_kind: TokenKind::Comma, expected_literal: "," },
            TestCase { expected_kind: TokenKind::Ident, expected_literal: "ten" },
            TestCase { expected_kind: TokenKind::Rparen, expected_literal: ")" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Bang, expected_literal: "!" },
            TestCase { expected_kind: TokenKind::Minus, expected_literal: "-" },
            TestCase { expected_kind: TokenKind::Slash, expected_literal: "/" },
            TestCase { expected_kind: TokenKind::Asterisk, expected_literal: "*" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "5" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "5" },
            TestCase { expected_kind: TokenKind::Lt, expected_literal: "<" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Gt, expected_literal: ">" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "5" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::If, expected_literal: "if" },
            TestCase { expected_kind: TokenKind::Lparen, expected_literal: "(" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "5" },
            TestCase { expected_kind: TokenKind::Lt, expected_literal: "<" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Rparen, expected_literal: ")" },
            TestCase { expected_kind: TokenKind::Lbrace, expected_literal: "{" },
            TestCase { expected_kind: TokenKind::Return, expected_literal: "return" },
            TestCase { expected_kind: TokenKind::True, expected_literal: "true" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Rbrace, expected_literal: "}" },
            TestCase { expected_kind: TokenKind::Else, expected_literal: "else" },
            TestCase { expected_kind: TokenKind::Lbrace, expected_literal: "{" },
            TestCase { expected_kind: TokenKind::Return, expected_literal: "return" },
            TestCase { expected_kind: TokenKind::False, expected_literal: "false" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Rbrace, expected_literal: "}" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Eq, expected_literal: "==" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "10" },
            TestCase { expected_kind: TokenKind::Ne, expected_literal: "!=" },
            TestCase { expected_kind: TokenKind::Int, expected_literal: "9" },
            TestCase { expected_kind: TokenKind::Semicolon, expected_literal: ";" },
            TestCase { expected_kind: TokenKind::Eof, expected_literal: "" },
        ];

        let input = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            10 == 10;
            10 != 9;";
        let mut lexer = Lexer::new(input);

        for (i, test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(test.expected_kind, token.kind, "tests[{}] - token kind wrong. expected={:?}, got={:?}", i, test.expected_kind, token.kind);
            assert_eq!(test.expected_literal, token.literal, "tests[{}] - literal wrong. expected={}, got={}", i, test.expected_literal, token.literal);
        }
    }
}