use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Stmt {
    LetStmt(Ident),
    ReturnStmt,
    ExprStmt(Expr)
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    IdentExpr(Ident),
    IntExpr(i64)
}

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        self == other
    }
}
