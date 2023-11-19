use crate::ast::{Expr, Prefix, Program, Stmt};
use crate::object::Object;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_program(&self, program: &mut Program) -> Object {
        self.eval_block_stmt(program)
    }

    fn eval_block_stmt(&self, program: &mut Program) -> Object {
        match program.len() {
            0 => Object::Null,
            _ => {
                let stmt = program.remove(0);
                self.eval_stmt(stmt)
            }
        }
    }

    fn eval_stmt(&self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::LetStmt(_, _) => unimplemented!(),
            Stmt::ReturnStmt(_) => unimplemented!(),
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
        }
    }

    fn eval_expr(&self, expr: Expr) -> Object {
        match expr {
            Expr::IdentExpr(_) => unimplemented!(),
            Expr::IntExpr(i) => Object::Integer(i),
            Expr::PrefixExpr(prefix, expr) => {
                let right = self.eval_expr(*expr);
                self.eval_prefix_expr(prefix, right)
            },
            Expr::InfixExpr(_, _, _) => unimplemented!(),
            Expr::BooleanExpr(b) => Object::Boolean(b),
            Expr::IfExpr { .. } => unimplemented!(),
            Expr::FunctionLiteralExpr { .. } => unimplemented!(),
            Expr::FunctionCallExpr { .. } => unimplemented!(),
        }
    }

    fn eval_prefix_expr(&self, prefix: Prefix, right: Object) -> Object {
        match prefix {
            Prefix::Minus => self.eval_minus_prefix_operator(right),
            Prefix::Bang => self.eval_bang_operator(right),
        }
    }

    fn eval_minus_prefix_operator(&self, right: Object) -> Object {
        match right {
            Object::Integer(i) => Object::Integer(-i),
            _ => Object::Null
        }
    }

    fn eval_bang_operator(&self, right: Object) -> Object {
        match right {
            Object::Boolean(b) => Object::Boolean(!b),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
    use super::*;

    struct TestCase<'a> {
        input: &'a str,
        expected: Object,
    }

    impl<'a> TestCase<'a> {
        fn new(input: &'a str, expected: Object) -> Self {
            Self { input, expected }
        }
    }

    fn test(test_cases: Vec<TestCase>) {
        for test_case in test_cases {
            let lexer = Lexer::new(test_case.input);
            let mut parser = Parser::new(lexer);
            let mut program = parser.parse_program();
            let evaluator = Evaluator::new();
            let obj = evaluator.eval_program(&mut program);

            assert_eq!(obj, test_case.expected);
        }
    }

    #[test]
    fn test_literals() {
        let test_cases = vec![
            TestCase::new("3", Object::Integer(3)),
            TestCase::new("true", Object::Boolean(true)),
            TestCase::new("false", Object::Boolean(false)),
            TestCase::new("", Object::Null),
        ];
        test(test_cases);
    }

    #[test]
    fn test_prefix_operators() {
        let test_cases = vec![
            TestCase::new("!true", Object::Boolean(false)),
            TestCase::new("!false", Object::Boolean(true)),
            TestCase::new("!5", Object::Boolean(false)),
            TestCase::new("!!true", Object::Boolean(true)),
            TestCase::new("!!false", Object::Boolean(false)),
            TestCase::new("!!5", Object::Boolean(true)),
            TestCase::new("5", Object::Integer(5)),
            TestCase::new("10", Object::Integer(10)),
            TestCase::new("-5", Object::Integer(-5)),
            TestCase::new("-10", Object::Integer(-10)),
        ];
        test(test_cases);
    }
}