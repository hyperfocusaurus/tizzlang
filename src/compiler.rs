use crate::parser::{
    Program,
    Statement,
    FunctionSignature,
    Expression,
};

pub struct Compiler {
    pub errors: Vec<String>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            errors: Vec::new(),
        }
    }
    pub fn compile(&mut self, program: Program) -> Vec<u8> {
        let bytecode = Vec::new();
        for stmt in program.statements {
            match stmt {
                Statement::Expression(_expr) => {
                    self.errors.push(format!("Top-level expressions not supported"));
                }
                Statement::FunctionStatement(signature, body) => {
                    self.compile_function(signature, body);
                }
                Statement::VariableDeclaration(_type, _name, _expr) => {
                    self.errors.push(format!("Global variables not yet supported"));
                }
                Statement::Block(_statements) => {
                    self.errors.push(format!("Global blocks not yet supported"));
                }
                Statement::Import(_path) => {
                    self.errors.push(format!("Imports not yet supported"));
                }
            }
        }
        bytecode
    }
    pub fn compile_function(&mut self, _signature: FunctionSignature, body: Vec<Statement>) -> Vec<u8> {
        let bytecode = Vec::new();
        for stmt in body {
            match stmt {
                Statement::Expression(expr) => {
                    self.compile_expression(expr);
                }
                Statement::FunctionStatement(_signature, _body) => {
                    self.errors.push(format!("Functions cannot be nested"));
                }
                Statement::VariableDeclaration(_type, _name, _expr) => {
                    self.errors.push(format!("Variables not yet supported"));
                }
                Statement::Block(_statements) => {
                    self.errors.push(format!("Blocks not yet supported"));
                }
                Statement::Import(_path) => {
                    self.errors.push(format!("Cannot import inside a function"));
                }
            }
        }
        bytecode
    }
    pub fn compile_expression(&mut self, expr: Expression) -> Vec<u8> {
        let bytecode = Vec::new();
        match expr {
            Expression::Lambda(_parameters, _body) => {
                self.errors.push(format!("Lambda expressions not supported"));
            }
            Expression::LiteralInt(_value) => {
                self.errors.push(format!("Integer literals not supported"));
            }
            Expression::LiteralFloat(_value) => {
                self.errors.push(format!("Float literals not supported"));
            }
            Expression::LiteralString(_value) => {
                self.errors.push(format!("String literals not supported"));
            }
            Expression::LiteralBool(_value) => {
                self.errors.push(format!("Boolean literals not supported"));
            }
            Expression::Identifier(_name) => {
                self.errors.push(format!("Identifiers not supported"));
            }
            Expression::FunctionCall(_name, _args) => {
                self.errors.push(format!("Function calls not supported"));
            }
            Expression::Binary(_left, _operator, _right) => {
                self.errors.push(format!("Binary operations not supported"));
            }
            Expression::Unary(_operator, _operand) => {
                self.errors.push(format!("Unary operations not supported"));
            }
        }
        bytecode
    }
}
