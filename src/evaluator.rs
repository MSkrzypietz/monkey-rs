use crate::ast::{Expr, Infix, Prefix, Program, Stmt};
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
            Expr::PrefixExpr(prefix, right) => {
                let right = self.eval_expr(*right);
                self.eval_prefix_expr(prefix, right)
            }
            Expr::InfixExpr(infix, left, right) => {
                let left = self.eval_expr(*left);
                let right = self.eval_expr(*right);
                self.eval_infix_expr(infix, left, right)
            }
            Expr::BooleanExpr(b) => Object::Boolean(b),
            Expr::IfExpr {cond, consequence, alternative} => {
                let cond = self.eval_expr(*cond);
                self.eval_if_expr(cond, consequence, alternative)
            },
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

    fn eval_infix_expr(&self, infix: Infix, left: Object, right: Object) -> Object {
        let (left, right) = match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => (l, r),
            (Object::Boolean(l), Object::Boolean(r)) => return match infix {
                Infix::Eq => Object::Boolean(l == r),
                Infix::Ne => Object::Boolean(l != r),
                _ => Object::Null,
            },
            _ => return Object::Null,
        };

        match infix {
            Infix::Plus => Object::Integer(left + right),
            Infix::Minus => Object::Integer(left - right),
            Infix::Asterisk => Object::Integer(left * right),
            Infix::Slash => Object::Integer(left / right),
            Infix::Gt => Object::Boolean(left > right),
            Infix::Lt => Object::Boolean(left < right),
            Infix::Eq => Object::Boolean(left == right),
            Infix::Ne => Object::Boolean(left != right),
        }
    }

    fn eval_if_expr(&self, cond: Object, mut consequence: Program, mut alternative: Program) -> Object {
        match cond {
            Object::Null | Object::Boolean(false) => self.eval_block_stmt(&mut alternative),
            _ => self.eval_block_stmt(&mut consequence),
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

    #[test]
    fn test_infix_operators() {
        let test_cases = vec![
            TestCase::new("5 + 5 + 5 - 2", Object::Integer(13)),
            TestCase::new("2 * 2 * 3", Object::Integer(12)),
            TestCase::new("-50 + 100 + -50", Object::Integer(0)),
            TestCase::new("5 * 2 + 10", Object::Integer(20)),
            TestCase::new("5 + 2 * 10", Object::Integer(25)),
            TestCase::new("20 + 2 * -10", Object::Integer(0)),
            TestCase::new("50 / 2 * 2 + 10", Object::Integer(60)),
            TestCase::new("2 * (5 + 10)", Object::Integer(30)),
            TestCase::new("3 * 3 * 3 + 10", Object::Integer(37)),
            TestCase::new("3 * (3 * 3) + 10", Object::Integer(37)),
            TestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
            TestCase::new("1 < 2", Object::Boolean(true)),
            TestCase::new("1 > 2", Object::Boolean(false)),
            TestCase::new("1 < 1", Object::Boolean(false)),
            TestCase::new("1 > 1", Object::Boolean(false)),
            TestCase::new("1 == 1", Object::Boolean(true)),
            TestCase::new("1 != 1", Object::Boolean(false)),
            TestCase::new("1 == 2", Object::Boolean(false)),
            TestCase::new("1 != 2", Object::Boolean(true)),
            TestCase::new("true == true", Object::Boolean(true)),
            TestCase::new("false == false", Object::Boolean(true)),
            TestCase::new("true == false", Object::Boolean(false)),
            TestCase::new("true != false", Object::Boolean(true)),
            TestCase::new("false != true", Object::Boolean(true)),
            TestCase::new("(1 < 2) == true", Object::Boolean(true)),
            TestCase::new("(1 < 2) == false", Object::Boolean(false)),
            TestCase::new("(1 > 2) == true", Object::Boolean(false)),
            TestCase::new("(1 > 2) == false", Object::Boolean(true)),
        ];
        test(test_cases);
    }

    #[test]
    fn test_if_expressions() {
        let test_cases = vec![
            TestCase::new("if (true) { 10 }", Object::Integer(10)),
            TestCase::new("if (false) { 10 }", Object::Null),
            TestCase::new("if (1) { 10 }", Object::Integer(10)),
            TestCase::new("if (1 < 2) { 10 }", Object::Integer(10)),
            TestCase::new("if (1 > 2) { 10 }", Object::Null),
            TestCase::new("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            TestCase::new("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];
        test(test_cases);
    }
}