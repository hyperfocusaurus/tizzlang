use crate::lexer::{Lexer, Token, TokenType};

pub struct Parser {
    pub lexer: Lexer,
    pub errors: Vec<String>,
    pub cur_token: Token,
    pub peek_token: Token,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Void,
    Reference(Box<Type>),
    Custom(String),
    //Lambda(Box<FunctionSignature>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    LiteralBool(bool),
    FunctionCall(String, Vec<Expression>),
    Binary(Box<Expression>, TokenType, Box<Expression>),
    Unary(Token, Box<Expression>),
    Lambda(FunctionSignature, Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionStatement(FunctionSignature, Vec<Statement>),
    VariableDeclaration(Type, String, Expression),
    Block(Vec<Statement>),
    Import(String),
    Expression(Expression),
}

impl Statement {
    pub fn get_expression(&self) -> Option<Expression> {
        match self {
            Statement::Expression(expr) => Some(expr.clone()),
            Statement::Block(statements) => {
                if statements.len() == 0 {
                    None
                } else {
                    statements.last().unwrap().get_expression()
                }
            }
            _ => None,
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer: lexer,
            errors: Vec::new(),
            cur_token: cur_token,
            peek_token: peek_token,
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.cur_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            } else {
                self.errors.push(format!(
                    "expected statement, got {:?}",
                    self.cur_token.token_type
                ));
            }
            self.next_token();
        }
        program
    }
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
    pub fn parse_import_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::KeywordImport => {
                self.next_token();
                if self.cur_token.token_type != TokenType::LiteralString {
                    self.errors.push(format!(
                        "expected string literal, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                let path = self.cur_token.literal.clone();
                self.next_token();
                if self.cur_token.token_type != TokenType::Semicolon {
                    self.errors.push(format!(
                        "expected semicolon, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                Some(Statement::Import(path))
            }
            _ => None,
        }
    }
    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::KeywordImport => self.parse_import_statement(),
            TokenType::LeftBrace => Some(Statement::Block(self.parse_block())),
            TokenType::KeywordFunction => self.parse_function_statement(),
            TokenType::KeywordInt
            | TokenType::KeywordFloat
            | TokenType::KeywordString
            | TokenType::KeywordBool
            | TokenType::Identifier => self.parse_variable_declaration(),
            TokenType::EOF => None,
            _ => {
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    Some(Statement::Expression(expr))
                } else {
                    None
                }
            }
        }
    }
    pub fn parse_variable_declaration(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::KeywordInt
            | TokenType::KeywordFloat
            | TokenType::KeywordString
            | TokenType::KeywordBool
            | TokenType::Identifier => {
                let typ = self.parse_type();
                if let Some(typ) = typ {
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Identifier {
                        self.errors.push(format!(
                            "expected identifier, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    let name = self.cur_token.literal.clone();
                    self.next_token();
                    if self.cur_token.token_type != TokenType::OperatorAssignment {
                        self.errors.push(format!(
                            "expected assignment operator, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        self.next_token();
                        if self.cur_token.token_type != TokenType::Semicolon {
                            self.errors.push(format!(
                                "expected semicolon, got {:?}",
                                self.cur_token.token_type
                            ));
                            return None;
                        }
                        Some(Statement::VariableDeclaration(typ, name, expr))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    pub fn parse_expression(&mut self) -> Option<Expression> {
        match self.cur_token.token_type {
            // unary operators
            TokenType::Asterisk |
            TokenType::OperatorPlus |
            TokenType::OperatorMinus |
            TokenType::OperatorNot => {
                let operator = self.cur_token.clone();
                self.next_token();
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    Some(Expression::Unary(operator, Box::new(expr)))
                } else {
                    None
                }
            }
            TokenType::KeywordString => {
                self.errors.push(format!("unexpected keyword 'string'"));
                None
            }
            TokenType::KeywordFloat => {
                self.errors.push(format!("unexpected keyword 'float'"));
                None
            }
            TokenType::KeywordBool => {
                self.errors.push(format!("unexpected keyword 'bool'"));
                None
            }
            TokenType::KeywordVoid => {
                self.errors.push(format!("void expression"));
                None
            }
            TokenType::KeywordImport => {
                self.errors.push(format!("imports cannot form part or all of an expression"));
                None
            }
            TokenType::KeywordLet => {
                // variable declaration
                let typ = self.parse_type();
                if let Some(_typ) = typ {
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Identifier {
                        self.errors.push(format!(
                            "expected identifier, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    let name = self.cur_token.literal.clone();
                    self.next_token();
                    if self.cur_token.token_type != TokenType::OperatorAssignment {
                        self.errors.push(format!(
                            "expected assignment operator, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        Some(Expression::Binary(
                            Box::new(Expression::Identifier(name)),
                            TokenType::OperatorAssignment,
                            Box::new(expr),
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            TokenType::KeywordFunction => {
                // lambda expression
                let mut parameters = Vec::new();
                let return_type;
                self.next_token();
                if self.cur_token.token_type != TokenType::LeftParenthesis {
                    self.errors.push(format!(
                        "expected left parenthesis, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                // now we expect a list of zero or more comma-separated parameters,
                // where each parameter is a type followed by an identifier
                while self.cur_token.token_type != TokenType::RightParenthesis {
                    if self.cur_token.token_type != TokenType::Identifier {
                        self.errors.push(format!(
                            "expected identifier, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Colon {
                        self.errors.push(format!(
                            "expected colon, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    let parameter_type = self.parse_type();
                    if parameter_type == None {
                        self.errors.push(format!(
                            "expected type, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    parameters.push(parameter_type.unwrap());
                    if self.cur_token.token_type == TokenType::Comma {
                        self.next_token();
                    }
                }
                self.next_token();
                if self.cur_token.token_type != TokenType::RightArrow {
                    self.errors.push(format!(
                        "expected -> or =>, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                let maybe_return_type = self.parse_type();
                if let Some(mreturn_type) = maybe_return_type {
                    return_type = mreturn_type;
                } else {
                    return_type = Type::Void;
                }
                if self.cur_token.token_type != TokenType::LeftBrace {
                    self.errors.push(format!(
                        "expected left brace, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                let mut statements = Vec::new();
                while self.cur_token.token_type != TokenType::RightBrace {
                    let stmt = self.parse_statement();
                    if let Some(stmt) = stmt {
                        statements.push(stmt);
                    } else {
                        self.errors.push(format!(
                            "expected statement, got {:?}",
                            self.cur_token.token_type
                        ));
                    }
                    self.next_token();
                }
                Some(Expression::Lambda(
                    FunctionSignature {
                        name: String::from(""),
                        parameters,
                        return_type,
                    },
                    statements,
                ))
            }
            TokenType::Colon => {
                self.errors.push(format!("unexpected colon"));
                None
            }
            TokenType::NamespaceDelimiter => {
                self.errors.push(format!("unexpected namespace delimiter"));
                None
            }
            TokenType::RightArrow => {
                self.errors.push(format!("unexpected right arrow"));
                None
            }
            TokenType::RightParenthesis => {
                self.errors.push(format!("unexpected right parenthesis"));
                None
            }
            TokenType::RightBrace => {
                self.errors.push(format!("unexpected right brace"));
                None
            }
            // Comma at the start of an expression is effectively meaningless, but not strictly an
            // error (maybe we can add a warning when we support warnings)
            TokenType::Comma => {
                None
            }
            TokenType::Illegal => {
                self.errors.push(format!(
                    "illegal token {:?}",
                    self.cur_token.token_type
                ));
                None
            }
            TokenType::Semicolon => {
                self.errors.push(format!("unexpected semicolon"));
                None
            }
            TokenType::LeftBrace => {
                let statements = self.parse_block();
                // return the last expression in the block
                if statements.len() == 0 {
                    None
                } else {
                    statements.last().unwrap().get_expression()
                }
            }
            TokenType::EOF => {
                self.errors.push(format!("unexpected EOF"));
                None
            }
            TokenType::KeywordInt => {
                self.errors.push(format!("Casting not yet supported"));
                None
            }
            TokenType::ForwardSlash |
            TokenType::OperatorAssignment |
            TokenType::OperatorEqual |
            TokenType::OperatorNotEqual |
            TokenType::OperatorLesser |
            TokenType::OperatorLesserEqual |
            TokenType::OperatorGreater |
            TokenType::OperatorGreaterEqual |
            TokenType::OperatorAnd |
            TokenType::OperatorOr |
            TokenType::Percent => {
                self.errors.push(format!("Binary operator at start of expression"));
                None
            }
            TokenType::Identifier => {
                let name = self.cur_token.literal.clone();
                // check if this is an assignment expression
                if self.peek_token.token_type.is_binary() {
                    self.next_token();
                    let operator = self.cur_token.clone();
                    self.next_token();
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        Some(Expression::Binary(
                            Box::new(Expression::Identifier(name)),
                            operator.token_type,
                            Box::new(expr),
                        ))
                    } else {
                        None
                    }
                } else if self.peek_token.token_type == TokenType::LeftParenthesis {
                    self.next_token();
                    let mut args = Vec::new();
                    self.next_token();
                    while self.cur_token.token_type != TokenType::RightParenthesis {
                        let arg = self.parse_expression();
                        if let Some(arg) = arg {
                            args.push(arg);
                        } else {
                            self.errors.push(format!(
                                "expected expression, got {:?}",
                                self.cur_token.token_type
                            ));
                        }
                        self.next_token();
                        if self.cur_token.token_type == TokenType::Comma {
                            self.next_token();
                        }
                    }
                    Some(Expression::FunctionCall(name, args))
                } else {
                    Some(Expression::Identifier(name))
                }
            }
            TokenType::LiteralInteger => {
                let value = self.cur_token.literal.parse::<i64>().unwrap();
                Some(Expression::LiteralInt(value))
            }
            TokenType::LiteralFloat => {
                let value = self.cur_token.literal.parse::<f64>().unwrap();
                Some(Expression::LiteralFloat(value))
            }
            TokenType::LiteralString => {
                let value = self.cur_token.literal.clone();
                Some(Expression::LiteralString(value))
            }
            TokenType::LiteralBool => {
                let value = self.cur_token.literal.parse::<bool>().unwrap();
                Some(Expression::LiteralBool(value))
            }
            TokenType::LeftParenthesis => {
                self.next_token();
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    self.next_token();
                    if self.cur_token.token_type != TokenType::RightParenthesis {
                        self.errors.push(format!(
                            "expected right parenthesis, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    Some(expr)
                } else {
                    None
                }
            }
        }
    }
    pub fn parse_function_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::KeywordFunction => {
                let mut parameters = Vec::new();
                let return_type;
                self.next_token();
                if self.cur_token.token_type != TokenType::Identifier {
                    self.errors.push(format!(
                        "expected identifier, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                let name = self.cur_token.literal.clone();
                self.next_token();
                if self.cur_token.token_type != TokenType::Colon {
                    self.errors.push(format!(
                        "expected left parenthesis, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                if self.cur_token.token_type != TokenType::LeftParenthesis {
                    self.errors.push(format!(
                        "expected left parenthesis, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                // now we expect a list of zero or more comma-separated parameters,
                // where each parameter is a type followed by an identifier
                while self.cur_token.token_type != TokenType::RightParenthesis {
                    if self.cur_token.token_type != TokenType::Identifier {
                        self.errors.push(format!(
                            "expected identifier, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Colon {
                        self.errors.push(format!(
                            "expected colon, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    self.next_token();
                    let parameter_type = self.parse_type();
                    if parameter_type == None {
                        self.errors.push(format!(
                            "expected type, got {:?}",
                            self.cur_token.token_type
                        ));
                        return None;
                    }
                    parameters.push(parameter_type.unwrap());
                    if self.cur_token.token_type == TokenType::Comma {
                        self.next_token();
                    }
                }
                self.next_token();
                if self.cur_token.token_type != TokenType::RightArrow {
                    self.errors.push(format!(
                        "expected -> or =>, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                let maybe_return_type = self.parse_type();
                if let Some(mreturn_type) = maybe_return_type {
                    return_type = mreturn_type;
                } else {
                    return_type = Type::Void;
                }
                if self.cur_token.token_type != TokenType::LeftBrace {
                    self.errors.push(format!(
                        "expected left brace, got {:?}",
                        self.cur_token.token_type
                    ));
                    return None;
                }
                self.next_token();
                let mut statements = Vec::new();
                while self.cur_token.token_type != TokenType::RightBrace {
                    let stmt = self.parse_statement();
                    if let Some(stmt) = stmt {
                        statements.push(stmt);
                    } else {
                        self.errors.push(format!(
                            "expected statement, got {:?}",
                            self.cur_token.token_type
                        ));
                    }
                    self.next_token();
                }
                Some(Statement::FunctionStatement(
                    FunctionSignature {
                        name,
                        parameters,
                        return_type,
                    },
                    statements,
                ))
            }
            _ => None,
        }
    }

    pub fn parse_type(&mut self) -> Option<Type> {
        match self.cur_token.token_type {
            // primitives
            TokenType::KeywordInt => {
                self.next_token();
                Some(Type::Int)
            }
            TokenType::KeywordFloat => {
                self.next_token();
                Some(Type::Float)
            }
            TokenType::KeywordString => {
                self.next_token();
                Some(Type::String)
            }
            TokenType::KeywordBool => {
                self.next_token();
                Some(Type::Bool)
            }
            TokenType::Identifier => {
                let name = self.cur_token.literal.clone();
                self.next_token();
                Some(Type::Custom(name))
            }
            // references
            TokenType::Asterisk => {
                self.next_token();
                let maybe_type = self.parse_type();
                if let Some(mtype) = maybe_type {
                    Some(Type::Reference(Box::new(mtype)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    fn parse_block(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        self.next_token();
        while self.cur_token.token_type != TokenType::RightBrace {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            } else {
                self.errors.push(format!(
                    "expected statement, got {:?}",
                    self.cur_token.token_type
                ));
            }
            self.next_token();
        }
        statements
    }
}
