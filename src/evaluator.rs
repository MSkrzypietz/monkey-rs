use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;
use crate::ast::{Expr, Infix, Prefix, Program, Stmt};
use crate::environment::Environment;
use crate::object::Object;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(env: Environment) -> Self {
        Self { env: Rc::new(RefCell::new(env)) }
    }

    pub fn eval_program(&mut self, program: &mut Program) -> Object {
        match self.eval_block_stmt(program) {
            Object::Return(obj) => *obj,
            obj => obj
        }
    }

    fn eval_block_stmt(&mut self, program: &mut Program) -> Object {
        match program.len() {
            0 => Object::Null,
            1 => self.eval_stmt(program.remove(0)),
            _ => {
                let stmt = program.remove(0);
                match self.eval_stmt(stmt) {
                    Object::Return(obj) => *obj,
                    Object::Error(err) => Object::Error(err),
                    _ => self.eval_block_stmt(program)
                }
            }
        }
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::LetStmt(ident, expr) => {
                let val = self.eval_expr(expr);
                self.env.borrow_mut().set(ident.0, val);
                Object::Null
            }
            Stmt::ReturnStmt(expr) => Object::Return(Box::new(self.eval_expr(expr))),
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Object {
        match expr {
            Expr::IdentExpr(ident) => self.eval_identifier(ident.0),
            Expr::IntExpr(i) => Object::Integer(i),
            Expr::StringExpr(str) => Object::String(str),
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
            Expr::IfExpr { cond, consequence, alternative } => {
                let cond = self.eval_expr(*cond);
                self.eval_if_expr(cond, consequence, alternative)
            }
            Expr::FunctionLiteralExpr { parameters, body } => Object::Function(parameters, body, Rc::clone(&self.env)),
            Expr::FunctionCallExpr { arguments, function } => self.eval_function_call(arguments, function),
        }
    }

    fn eval_identifier(&self, ident: String) -> Object {
        match self.env.borrow().get(&ident) {
            Some(val) => val,
            None => Object::Error(format!("identifier not found: {}", &ident)),
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
            _ => Object::Error(format!("unknown operator: -{}", right.get_type()))
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
                _ => Object::Error(format!("unknown operator: {} {} {}", Object::Boolean(l).get_type(), infix, Object::Boolean(r).get_type())),
            },
            (l, r) => return Object::Error(format!("type mismatch: {} {} {}", l.get_type(), infix, r.get_type())),
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

    fn eval_if_expr(&mut self, cond: Object, mut consequence: Program, mut alternative: Program) -> Object {
        match cond {
            Object::Null | Object::Boolean(false) => self.eval_block_stmt(&mut alternative),
            _ => self.eval_block_stmt(&mut consequence),
        }
    }

    fn eval_function_call(&mut self, arguments: Vec<Expr>, function: Box<Expr>) -> Object {
        let function = self.eval_expr(*function);
        let args = arguments.into_iter().map(|arg| self.eval_expr(arg)).collect();
        self.apply_function(function, args)
    }

    fn apply_function(&self, function: Object, args: Vec<Object>) -> Object {
        match function {
            Object::Function(parameters, mut program, env) => {
                let mut extended_env = Environment::new_enclosed(Rc::clone(&env));
                zip(parameters, args).into_iter().for_each(|(parameter, arg)| extended_env.set(parameter.0, arg));
                Evaluator::new(extended_env).eval_program(&mut program)
            }
            _ => Object::Error(format!("not a function: {}", function.get_type()))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr::{IdentExpr, InfixExpr, IntExpr};
    use crate::ast::Ident;
    use crate::ast::Stmt::ExprStmt;
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
            let env = Environment::new();
            let mut evaluator = Evaluator::new(env);
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
            TestCase::new("\"Hello World!\"", Object::String("Hello World!".to_string())),
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

    #[test]
    fn test_return_expressions() {
        let test_cases = vec![
            TestCase::new("return 10;", Object::Integer(10)),
            TestCase::new("return 10; 9;", Object::Integer(10)),
            TestCase::new("return 2 * 5; 9;", Object::Integer(10)),
            TestCase::new("9; return 2 * 5; 9;", Object::Integer(10)),
            TestCase::new("
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }", Object::Integer(10)),
        ];
        test(test_cases);
    }

    #[test]
    fn test_error_handling() {
        let test_cases = vec![
            TestCase::new("5 + true;", Object::Error("type mismatch: INTEGER + BOOLEAN".to_string())),
            TestCase::new("5 + true; 5;", Object::Error("type mismatch: INTEGER + BOOLEAN".to_string())),
            TestCase::new("-true", Object::Error("unknown operator: -BOOLEAN".to_string())),
            TestCase::new("true + false;", Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string())),
            TestCase::new("5; true + false; 5", Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string())),
            TestCase::new("if (10 > 1) { true + false; }", Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string())),
            TestCase::new("
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }", Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string())),
            TestCase::new("foobar", Object::Error("identifier not found: foobar".to_string())),
        ];
        test(test_cases);
    }

    #[test]
    fn test_let_statements() {
        let test_cases = vec![
            TestCase::new("let a = 5; a;", Object::Integer(5)),
            TestCase::new("let a = 5 * 5; a;", Object::Integer(25)),
            TestCase::new("let a = 5; let b = a; b;", Object::Integer(5)),
            TestCase::new("let a = 5; let b = a; let c = a + b + 5; c;", Object::Integer(15)),
        ];
        test(test_cases);
    }

    #[test]
    fn test_function_object() {
        let test_cases = vec![
            TestCase::new(
                "fn(x) { x + 2; };",
                Object::Function(
                    vec![Ident("x".to_string())],
                    vec![ExprStmt(InfixExpr(Infix::Plus, Box::new(IdentExpr(Ident("x".to_string()))), Box::new(IntExpr(2))))],
                    Rc::new(RefCell::new(Environment::new()))),
            )
        ];
        test(test_cases);
    }

    #[test]
    fn test_function_application() {
        let test_cases = vec![
            TestCase::new("let identity = fn(x) { x; }; identity(5);", Object::Integer(5)),
            TestCase::new("let identity = fn(x) { return x; }; identity(5);", Object::Integer(5)),
            TestCase::new("let double = fn(x) { x * 2; }; double(5);", Object::Integer(10)),
            TestCase::new("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Integer(10)),
            TestCase::new("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Object::Integer(20)),
            TestCase::new("fn(x) { x; }(5)", Object::Integer(5)),
        ];
        test(test_cases);
    }

    #[test]
    fn test_closures() {
        let test_cases = vec![
            TestCase::new("
                let newAdder = fn(x) {
                    fn(y) { x + y };
                };
                let addTwo = newAdder(2);
                addTwo(2);", Object::Integer(4)),
        ];
        test(test_cases);
    }
}