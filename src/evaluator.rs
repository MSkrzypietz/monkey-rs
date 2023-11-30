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
            Expr::ArrayExpr(elements) => self.eval_array_literal(elements),
            Expr::IndexExpr { left, index } => {
                let left = self.eval_expr(*left);
                let index = self.eval_expr(*index);
                self.eval_index_expr(left, index)
            }
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
            None => match ident.as_str() {
                "len" => Object::Builtin(Self::builtin_len),
                "first" => Object::Builtin(Self::builtin_first),
                "last" => Object::Builtin(Self::builtin_last),
                _ => Object::Error(format!("identifier not found: {}", ident)),
            }
        }
    }

    fn builtin_len(args: &[Object]) -> Object {
        match args {
            [arg] => return match arg {
                Object::String(str) => Object::Integer(str.len() as i64),
                Object::Array(elements) => Object::Integer(elements.len() as i64),
                _ => Object::Error(format!("argument to `len` not supported, got {}", arg.get_type()))
            },
            _ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
        }
    }

    fn builtin_first(args: &[Object]) -> Object {
        Self::builtin_array_get(args, |l| l.get(0))
    }

    fn builtin_last(args: &[Object]) -> Object {
        Self::builtin_array_get(args, |l| l.last())
    }

    fn builtin_array_get(args: &[Object], get_element: fn(&Vec<Object>)->Option<&Object>) -> Object {
        match args {
            [arg] => return match arg {
                Object::Array(elements) => get_element(elements).cloned().unwrap_or(Object::Null),
                _ => Object::Error(format!("argument to `first` must be ARRAY, got {}", arg.get_type()))
            },
            _ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
        }
    }

    fn eval_index_expr(&self, left: Object, index: Object) -> Object {
        match (left.clone(), index) {
            (Object::Array(elements), Object::Integer(i)) => {
                match elements.get(i as usize) {
                    Some(element) => element.clone(),
                    None => Object::Null
                }
            }
            _ => Object::Error(format!("index operator not supported: {}", left.get_type()))
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
            (Object::String(l), Object::String(r)) => return match infix {
                Infix::Plus => Object::String(l + &r),
                _ => Object::Error(format!("unknown operator: {} {} {}", Object::String(l).get_type(), infix, Object::String(r).get_type()))
            },
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
        let args = self.eval_expression_list(arguments);
        self.apply_function(function, args)
    }

    fn eval_array_literal(&mut self, elements: Vec<Expr>) -> Object {
        Object::Array(self.eval_expression_list(elements))
    }

    fn eval_expression_list(&mut self, list: Vec<Expr>) -> Vec<Object> {
        list.into_iter().map(|el| self.eval_expr(el)).collect()
    }

    fn apply_function(&self, function: Object, args: Vec<Object>) -> Object {
        match function {
            Object::Function(parameters, mut program, env) => {
                let mut extended_env = Environment::new_enclosed(Rc::clone(&env));
                zip(parameters, args).into_iter().for_each(|(parameter, arg)| extended_env.set(parameter.0, arg));
                Self::new(extended_env).eval_program(&mut program)
            }
            Object::Builtin(func) => func(args.as_slice()),
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
            TestCase::new("\"Hello\" - \"World\"", Object::Error("unknown operator: STRING - STRING".to_string())),
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

    #[test]
    fn test_string_concat() {
        let test_cases = vec![
            TestCase::new("\"Hello\" + \" \" + \"World!\"", Object::String("Hello World!".to_string())),
        ];
        test(test_cases);
    }

    #[test]
    fn test_builtin_functions() {
        let test_cases = vec![
            TestCase::new("\"Hello\" + \" \" + \"World!\"", Object::String("Hello World!".to_string())),
            TestCase::new("len(\"\")", Object::Integer(0)),
            TestCase::new("len(\"four\")", Object::Integer(4)),
            TestCase::new("len(\"hello world\")", Object::Integer(11)),
            TestCase::new("len(1)", Object::Error("argument to `len` not supported, got INTEGER".to_string())),
            TestCase::new("len(\"one\", \"two\")", Object::Error("wrong number of arguments. got=2, want=1".to_string())),
            TestCase::new("len([])", Object::Integer(0)),
            TestCase::new("len([1, 2, 3])", Object::Integer(3)),
            TestCase::new("len([], [])", Object::Error("wrong number of arguments. got=2, want=1".to_string())),
            TestCase::new("first([])", Object::Null),
            TestCase::new("first([1])", Object::Integer(1)),
            TestCase::new("first([1, 2])", Object::Integer(1)),
            TestCase::new("first(1)", Object::Error("argument to `first` must be ARRAY, got INTEGER".to_string())),
            TestCase::new("first([], [])", Object::Error("wrong number of arguments. got=2, want=1".to_string())),
            TestCase::new("last([])", Object::Null),
            TestCase::new("last([1])", Object::Integer(1)),
            TestCase::new("last([1, 2])", Object::Integer(2)),
            TestCase::new("last([], [])", Object::Error("wrong number of arguments. got=2, want=1".to_string())),
            TestCase::new("last(1)", Object::Error("argument to `last` must be ARRAY, got INTEGER".to_string())),
        ];
        test(test_cases);
    }

    #[test]
    fn test_array_literals() {
        let test_cases = vec![
            TestCase::new("[1, 2 * 2, 3 + 3]", Object::Array(vec![Object::Integer(1), Object::Integer(4), Object::Integer(6)]))
        ];
        test(test_cases);
    }

    #[test]
    fn test_array_index_expressions() {
        let test_cases = vec![
            TestCase::new("[1, 2, 3][0]", Object::Integer(1)),
            TestCase::new("[1, 2, 3][1]", Object::Integer(2)),
            TestCase::new("[1, 2, 3][2]", Object::Integer(3)),
            TestCase::new("let i = 0; [1][i];", Object::Integer(1)),
            TestCase::new("[1, 2, 3][1 + 1];", Object::Integer(3)),
            TestCase::new("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            TestCase::new("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Object::Integer(6)),
            TestCase::new("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Object::Integer(2)),
            TestCase::new("[1, 2, 3][3]", Object::Null),
            TestCase::new("[1, 2, 3][-1]", Object::Null),
        ];
        test(test_cases);
    }
}