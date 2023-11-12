#[derive(Debug, PartialEq)]
pub enum Stmt {
    LetStmt(Ident)
}

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        self == other
    }
}
