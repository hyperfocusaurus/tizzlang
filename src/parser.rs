use crate::lexer::{Lexer, Token, TokenType};

pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

pub struct Parser {
    pub lexer: Lexer,
    pub errors: Vec<ParseError>,
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
pub struct NamespacedIdentifier {
    pub namespace: Vec<String>,
    pub identifier: String,
}

impl NamespacedIdentifier {
    pub fn new_anon(name: String) -> Self {
        NamespacedIdentifier {
            namespace: Vec::new(),
            identifier: name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(NamespacedIdentifier),
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
    Import(NamespacedIdentifier),
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

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
            errors: Vec::new(),
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        loop {
            let token = self.next_token();
            match token.token_type {
                TokenType::EOF => break,
                _ => {
                    let stmt = self.parse_statement(token.clone());
                    if let Some(stmt) = stmt {
                        program.statements.push(stmt);
                    } else {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!("expected statement, got {:?}", token.token_type),
                        });
                    }
                }
            }
        }
        program
    }
    pub fn next_token(&mut self) -> Token {
        self.lexer.next_token()
    }
    pub fn peek_token(&mut self) -> Token {
        self.lexer.peek_token()
    }
    pub fn parse_identifier_with_namespaces(&mut self) -> Option<NamespacedIdentifier> {
        let mut identifier = String::new();
        let mut namespace = Vec::new();
        loop {
            let token = self.next_token();
            match token.token_type {
                TokenType::Identifier => {
                    identifier = token.literal.clone();
                }
                TokenType::NamespaceDelimiter => {
                    namespace.push(identifier.clone());
                    identifier = String::new();
                }
                TokenType::Semicolon => {
                    break;
                }
                _ => {
                    self.errors.push(ParseError {
                        line: token.line,
                        column: token.column,
                        message: format!("expected identifier, got {:?}", token.token_type),
                    });
                    return None;
                }
            }
        }
        if identifier.len() == 0 {
            None
        } else {
            Some(NamespacedIdentifier {
                namespace,
                identifier,
            })
        }
    }
    pub fn parse_import_statement(&mut self) -> Option<Statement> {
        match self.next_token().token_type {
            TokenType::KeywordImport => {
                let identifier = self.parse_identifier_with_namespaces();
                let token = self.next_token();
                if token.token_type != TokenType::Semicolon {
                    self.errors.push(ParseError {
                        line: token.line,
                        column: token.column,
                        message: format!("expected semicolon, got {:?}", token.token_type),
                    });
                    return None;
                }
                if let Some(identifier) = identifier {
                    Some(Statement::Import(identifier))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    pub fn parse_statement(&mut self, token: Token) -> Option<Statement> {
        match token.token_type {
            TokenType::KeywordImport => self.parse_import_statement(),
            TokenType::LeftBrace => Some(Statement::Block(self.parse_block())),
            TokenType::KeywordFunction => self.parse_function_statement(),
            TokenType::KeywordInt
            | TokenType::KeywordFloat
            | TokenType::KeywordString
            | TokenType::KeywordBool
            | TokenType::Identifier => self.parse_variable_declaration(),
            TokenType::EOF => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected EOF"),
                });
                None
            }
            _ => {
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    println!("expr: {:?}", expr);
                    Some(Statement::Expression(expr))
                } else {
                    None
                }
            }
        }
    }
    pub fn parse_variable_declaration(&mut self) -> Option<Statement> {
        let mut token = self.next_token();
        match token.token_type {
            TokenType::KeywordInt
            | TokenType::KeywordFloat
            | TokenType::KeywordString
            | TokenType::KeywordBool
            | TokenType::Identifier => {
                let typ = self.parse_type();
                if let Some(typ) = typ {
                    token = self.next_token();
                    if token.token_type != TokenType::Identifier {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected identifier, got {:?}",
                                token.token_type
                            ),
                        });
                        return None;
                    }
                    let name = token.literal.clone();
                    token = self.next_token();
                    if token.token_type != TokenType::OperatorAssignment {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected assignment operator, got {:?}",
                                token.token_type
                            ),
                        });
                        return None;
                    }
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        token = self.next_token();
                        if token.token_type != TokenType::Semicolon {
                            self.errors.push(ParseError {
                                line: token.line,
                                column: token.column,
                                message: format!(
                                    "expected semicolon, got {:?}",
                                    token.token_type
                                ),
                            });
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
        let mut token = self.next_token();
        match token.token_type {
            // unary operators
            TokenType::Asterisk
            | TokenType::OperatorPlus
            | TokenType::OperatorMinus
            | TokenType::OperatorNot => {
                let operator = token.clone();
                self.next_token();
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    Some(Expression::Unary(operator, Box::new(expr)))
                } else {
                    None
                }
            }
            TokenType::KeywordString => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected keyword 'string'"),
                });
                None
            }
            TokenType::KeywordFloat => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected keyword 'float'"),
                });
                None
            }
            TokenType::KeywordBool => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected keyword 'bool'"),
                });
                None
            }
            TokenType::KeywordVoid => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("void expression"),
                });
                None
            }
            TokenType::KeywordImport => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("imports cannot form part or all of an expression"),
                });
                None
            }
            TokenType::KeywordLet => {
                // variable declaration
                let typ = self.parse_type();
                if let Some(_typ) = typ {
                    self.next_token();
                    if token.token_type != TokenType::Identifier {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected identifier, got {:?}",
                                token.token_type
                            ),
                        });
                        return None;
                    }
                    let name = token.literal.clone();
                    self.next_token();
                    if token.token_type != TokenType::OperatorAssignment {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected assignment operator, got {:?}",
                                token.token_type
                            ),
                        });

                        return None;
                    }
                    self.next_token();
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        Some(Expression::Binary(
                            Box::new(Expression::Identifier(NamespacedIdentifier::new_anon(name))),
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
                if token.token_type != TokenType::LeftParenthesis {
                    self.errors.push(ParseError {
                        line: token.line,
                        column: token.column,
                        message: format!(
                            "expected left parenthesis, got {:?}",
                            token.token_type
                        ),
                    });

                    return None;
                }
                self.next_token();
                // now we expect a list of zero or more comma-separated parameters,
                // where each parameter is a type followed by an identifier
                while token.token_type != TokenType::RightParenthesis {
                    if token.token_type != TokenType::Identifier {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected identifier, got {:?}",
                                token.token_type
                            ),
                        });

                        return None;
                    }
                    self.next_token();
                    if token.token_type != TokenType::Colon {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!("expected colon, got {:?}", token.token_type),
                        });
                        return None;
                    }
                    self.next_token();
                    let parameter_type = self.parse_type();
                    if parameter_type == None {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!("expected type, got {:?}", token.token_type),
                        });
                        return None;
                    }
                    parameters.push(parameter_type.unwrap());
                    if token.token_type == TokenType::Comma {
                        self.next_token();
                    }
                }
                self.next_token();
                if token.token_type != TokenType::RightArrow {
                    self.errors.push(ParseError {
                        line: token.line,
                        column: token.column,
                        message: format!("expected -> or =>, got {:?}", token.token_type),
                    });
                    return None;
                }
                self.next_token();
                let maybe_return_type = self.parse_type();
                if let Some(mreturn_type) = maybe_return_type {
                    return_type = mreturn_type;
                } else {
                    return_type = Type::Void;
                }
                if token.token_type != TokenType::LeftBrace {
                    self.errors.push(ParseError {
                        line: token.line,
                        column: token.column,
                        message: format!(
                            "expected left brace, got {:?}",
                            token.token_type
                        ),
                    });
                    return None;
                }
                token = self.next_token();
                let mut statements = Vec::new();
                while token.token_type != TokenType::RightBrace {
                    let stmt = self.parse_statement(token.clone());
                    if let Some(stmt) = stmt {
                        statements.push(stmt);
                    } else {
                        self.errors.push(ParseError {
                            line: token.line,
                            column: token.column,
                            message: format!(
                                "expected statement, got {:?}",
                                token.token_type
                            ),
                        });
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
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected colon"),
                });
                None
            }
            TokenType::NamespaceDelimiter => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected namespace delimiter"),
                });
                None
            }
            TokenType::RightArrow => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected right arrow"),
                });
                None
            }
            TokenType::RightParenthesis => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected right parenthesis"),
                });
                None
            }
            TokenType::RightBrace => {
                self.errors.push(ParseError {
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected right brace"),
                });
                None
            }
            // Comma at the start of an expression is effectively meaningless, but not strictly an
            // error (maybe we can add a warning when we support warnings)
            TokenType::Comma => None,
            TokenType::Illegal => {
                self.errors
                    .push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!("illegal token {:?}", token.token_type)});
                None
            }
            TokenType::Semicolon => {
                self.errors.push(ParseError{
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected semicolon")});
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
                self.errors.push(ParseError{
                    line: token.line,
                    column: token.column,
                    message: format!("unexpected EOF")});
                None
            }
            TokenType::KeywordInt => {
                self.errors.push(ParseError{
                    line: token.line,
                    column: token.column,

                    message: format!("Casting not yet supported")});
                None
            }
            TokenType::ForwardSlash
            | TokenType::OperatorAssignment
            | TokenType::OperatorEqual
            | TokenType::OperatorNotEqual
            | TokenType::OperatorLesser
            | TokenType::OperatorLesserEqual
            | TokenType::OperatorGreater
            | TokenType::OperatorGreaterEqual
            | TokenType::OperatorAnd
            | TokenType::OperatorOr
            | TokenType::Percent => {
                self.errors
                    .push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!("Binary operator at start of expression")});
                None
            }
            // todo: this doesn't account for namespaces
            TokenType::Identifier => {
                let name = token.literal.clone();
                // check if this is an assignment expression
                if self.peek_token().token_type.is_binary() {
                    self.next_token();
                    let operator = token.clone();
                    self.next_token();
                    let expr = self.parse_expression();
                    if let Some(expr) = expr {
                        Some(Expression::Binary(
                            Box::new(Expression::Identifier(NamespacedIdentifier::new_anon(name))),
                            operator.token_type,
                            Box::new(expr),
                        ))
                    } else {
                        None
                    }
                } else if self.peek_token().token_type == TokenType::LeftParenthesis {
                    self.next_token();
                    let mut args = Vec::new();
                    self.next_token();
                    while token.token_type != TokenType::RightParenthesis {
                        let arg = self.parse_expression();
                        if let Some(arg) = arg {
                            args.push(arg);
                        } else {
                            self.errors.push(ParseError{
                                line: token.line,
                                column: token.column,
                                message:format!(
                                "expected expression, got {:?}",
                                token.token_type
                            )});
                        }
                        self.next_token();
                        if token.token_type == TokenType::Comma {
                            self.next_token();
                        }
                    }
                    Some(Expression::FunctionCall(name, args))
                } else {
                    Some(Expression::Identifier(NamespacedIdentifier::new_anon(name)))
                }
            }
            TokenType::LiteralInteger => {
                let value = token.literal.parse::<i64>().unwrap();
                Some(Expression::LiteralInt(value))
            }
            TokenType::LiteralFloat => {
                let value = token.literal.parse::<f64>().unwrap();
                Some(Expression::LiteralFloat(value))
            }
            TokenType::LiteralString => {
                let value = token.literal.clone();
                Some(Expression::LiteralString(value))
            }
            TokenType::LiteralBool => {
                let value = token.literal.parse::<bool>().unwrap();
                Some(Expression::LiteralBool(value))
            }
            TokenType::LeftParenthesis => {
                self.next_token();
                let expr = self.parse_expression();
                if let Some(expr) = expr {
                    self.next_token();
                    if token.token_type != TokenType::RightParenthesis {
                        self.errors.push(ParseError{
                            line: token.line,
                            column: token.column,
                            message: format!(
                            "expected right parenthesis, got {:?}",
                            token.token_type
                        )});
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
        let mut token = self.next_token();
        match token.token_type {
            TokenType::KeywordFunction => {
                let mut parameters = Vec::new();
                let return_type;
                token = self.next_token();
                if token.token_type != TokenType::Identifier {
                    self.errors.push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!(
                        "expected identifier, got {:?}",
                        token.token_type
                    )});
                    return None;
                }
                let name = token.literal.clone();
                self.next_token();
                if token.token_type != TokenType::Colon {
                    self.errors.push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!(
                        "expected left parenthesis, got {:?}",
                        token.token_type
                    )});
                    return None;
                }
                self.next_token();
                if token.token_type != TokenType::LeftParenthesis {
                    self.errors.push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!(
                        "expected left parenthesis, got {:?}",
                        token.token_type
                    )});
                    return None;
                }
                self.next_token();
                // now we expect a list of zero or more comma-separated parameters,
                // where each parameter is a type followed by an identifier
                while token.token_type != TokenType::RightParenthesis {
                    if token.token_type != TokenType::Identifier {
                        self.errors.push(ParseError{
                            line: token.line,
                            column: token.column,
                            message:format!(
                            "expected identifier, got {:?}",
                            token.token_type
                        )});
                        return None;
                    }
                    self.next_token();
                    if token.token_type != TokenType::Colon {
                        self.errors.push(ParseError{
                            line: token.line,
                            column: token.column,
                            message: format!(
                            "expected colon, got {:?}",
                            token.token_type
                        )});
                        return None;
                    }
                    self.next_token();
                    let parameter_type = self.parse_type();
                    if parameter_type == None {
                        self.errors.push(ParseError{
                            line: token.line,
                            column: token.column,
                            message: format!(
                            "expected type, got {:?}",
                            token.token_type
                        )});
                        return None;
                    }
                    parameters.push(parameter_type.unwrap());
                    if token.token_type == TokenType::Comma {
                        self.next_token();
                    }
                }
                self.next_token();
                if token.token_type != TokenType::RightArrow {
                    self.errors.push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!(
                        "expected -> or =>, got {:?}",
                        token.token_type
                    )});
                    return None;
                }
                self.next_token();
                let maybe_return_type = self.parse_type();
                if let Some(mreturn_type) = maybe_return_type {
                    return_type = mreturn_type;
                } else {
                    return_type = Type::Void;
                }
                if token.token_type != TokenType::LeftBrace {
                    self.errors.push(ParseError{
                        line: token.line,
                        column: token.column,
                        message: format!(
                        "expected left brace, got {:?}",
                        token.token_type
                    )});
                    return None;
                }
                let token = self.next_token();
                let mut statements = Vec::new();
                while token.token_type != TokenType::RightBrace {
                    let stmt = self.parse_statement(token.clone());
                    if let Some(stmt) = stmt {
                        statements.push(stmt);
                    } else {
                        self.errors.push(ParseError{
                            line: token.line,
                            column: token.column,
                            message: format!(
                            "expected statement, got {:?}",
                            token.token_type
                        )});
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
        let token = self.next_token();
        match token.token_type {
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
                let name = token.literal.clone();
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
        let token = self.next_token();
        while token.token_type != TokenType::RightBrace {
            let stmt = self.parse_statement(token.clone());
            if let Some(stmt) = stmt {
                statements.push(stmt);
            } else {
                self.errors.push(ParseError{
                    line: token.line,
                    column: token.column,
                    message: format!(
                    "expected statement, got {:?}",
                    token.token_type
                )});
            }
            self.next_token();
        }
        statements
    }
}
