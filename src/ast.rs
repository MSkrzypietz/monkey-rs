use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    LetStmt(Ident),
    ReturnStmt,
    ExprStmt(Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::LetStmt(ident) => f.write_fmt(format_args!("let {:?} = ;", ident)),
            Stmt::ReturnStmt => f.write_str("return ;"),
            _ => f.write_str(";")
        }
    }
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
