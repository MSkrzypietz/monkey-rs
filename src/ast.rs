use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::LetStmt(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Stmt::ReturnStmt(expr) => write!(f, "return {};", expr),
            Stmt::ExprStmt(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    IntExpr(i64),
    StringExpr(String),
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

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::IdentExpr(ident) => write!(f, "{}", ident),
            Expr::IntExpr(n) => write!(f, "{}", n),
            Expr::StringExpr(str) => write!(f, "{}", str),
            Expr::PrefixExpr(prefix, expr) => write!(f, "{}{}", prefix, expr),
            Expr::InfixExpr(infix, left, right) => write!(f, "({} {} {})", left, infix, right),
            Expr::BooleanExpr(b) => write!(f, "{}", b),
            Expr::IfExpr { cond, consequence, alternative } => {
                write!(f, "if {} {{ ", cond)?;
                for stmt in consequence {
                    write!(f, "{}", stmt)?;
                }
                if !alternative.is_empty() {
                    write!(f, " }} else {{ ")?;
                    for stmt in alternative {
                        write!(f, "{}", stmt)?;
                    }
                }
                write!(f, " }}")?;
                Ok(())
            }
            Expr::FunctionLiteralExpr { parameters, body } => {
                write!(f, "fn(")?;
                if let Some(first_param) = parameters.get(0) {
                    write!(f, "{}", first_param)?;
                }
                for param in parameters.iter().skip(1) {
                    write!(f, ", {}", param)?;
                }
                write!(f, ") {{ ")?;
                for stmt in body {
                    write!(f, "{}", stmt)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Expr::FunctionCallExpr { function, arguments } => {
                write!(f, "{}(", function)?;
                if let Some(first_arg) = arguments.get(0) {
                    write!(f, "{}", first_arg)?;
                }
                for arg in arguments.iter().skip(1) {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus,
    Bang,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Prefix::Minus => write!(f, "-"),
            Prefix::Bang => write!(f, "!"),
        }
    }
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

impl Display for Infix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
            Infix::Gt => write!(f, ">"),
            Infix::Lt => write!(f, "<"),
            Infix::Eq => write!(f, "=="),
            Infix::Ne => write!(f, "!="),
        }
    }
}

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        self == other
    }
}
