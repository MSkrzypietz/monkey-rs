use std::fmt::{Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    LetStmt(Ident),
    ReturnStmt(Expr),
    ExprStmt(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    IntExpr(i64),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    BooleanExpr(bool),
    IfExpr {
        cond: Box<Expr>,
        consequence: Program,
        alternative: Program,
    },
    FunctionLiteralExpr {
        parameters: Vec<Ident>,
        body: Program,
    },
    FunctionCallExpr {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
    Ne,
}

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        self == other
    }
}
