#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + literals
    Identifier,
    LiteralInteger,
    LiteralString,
    LiteralFloat,
    LiteralBool,

    // Binary Operators
    OperatorAssignment,
    OperatorPlus,
    OperatorMinus,
    OperatorEqual,
    OperatorGreater,
    OperatorGreaterEqual,
    OperatorLesser,
    OperatorLesserEqual,
    OperatorNotEqual,
    ForwardSlash, // division
    Percent, // modulo
    OperatorAnd,
    OperatorOr,
    
    // Unary operators
    Asterisk, 
    OperatorNot,

    // Delimiters
    Comma,
    Semicolon,
    Colon,
    NamespaceDelimiter,
    RightArrow,

    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,

    // Keywords
    KeywordFunction,
    KeywordLet,
    KeywordImport,
    
    // Primitive Type Keywords
    KeywordVoid,
    KeywordInt,
    KeywordString,
    KeywordBool,
    KeywordFloat,
}

impl TokenType {
    pub fn is_binary(&self) -> bool {
        match self {
            TokenType::OperatorAssignment |
            TokenType::OperatorPlus |
            TokenType::OperatorMinus |
            TokenType::OperatorEqual |
            TokenType::OperatorGreater |
            TokenType::OperatorGreaterEqual |
            TokenType::OperatorLesser |
            TokenType::OperatorLesserEqual |
            TokenType::OperatorNotEqual |
            TokenType::ForwardSlash |
            TokenType::Percent |
            TokenType::OperatorAnd |
            TokenType::OperatorOr => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)] 
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input: input,
            position: 0,
            read_position: 0,
            line: 1,
            column: 1,
        }
    }
    pub fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
    pub fn peek_token(&self) -> Token {
        let mut lexer = self.clone();
        lexer.next_token().clone()
    }
    pub fn next_token(&mut self) -> Token {
        let mut ch = self.read_char();
        match ch {
            b'%' => Token {
                token_type: TokenType::Percent,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'&' => {
                match self.peek_char() {
                    b'&' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorAnd,
                            literal: "&&".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::Illegal,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'|' => {
                match self.peek_char() {
                    b'|' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorOr,
                            literal: "||".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::Illegal,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'!' => {
                match self.peek_char() {
                    b'=' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorNotEqual,
                            literal: "!=".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorNot,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'<' => {
                match self.peek_char() {
                    b'=' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorLesserEqual,
                            literal: "<=".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorLesser,
                        literal: "<".to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'>' => {
                match self.peek_char() {
                    b'=' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorGreaterEqual,
                            literal: ">=".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorGreater,
                        literal: ">".to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'*' => Token {
                token_type: TokenType::Asterisk,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b':' => {
                if self.peek_char() == b':' {
                    self.read_char();
                    Token {
                        token_type: TokenType::NamespaceDelimiter,
                        literal: "::".to_string(),
                        line: self.line,
                        column: self.column,
                    }
                } else {
                    Token {
                        token_type: TokenType::Colon,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    }
                }
            },
            b'=' => {
                match self.peek_char() {
                    b'>' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::RightArrow,
                            literal: "=>".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    b'=' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorEqual,
                            literal: "==".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorAssignment,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b'-' => {
                match self.peek_char() {
                    b'>' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::RightArrow,
                            literal: "->".to_string(),
                            line: self.line,
                            column: self.column,
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorMinus,
                        literal: ch.to_string(),
                        line: self.line,
                        column: self.column,
                    },
                }
            },
            b';' => Token {
                token_type: TokenType::Semicolon,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'(' => Token {
                token_type: TokenType::LeftParenthesis,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b')' => Token {
                token_type: TokenType::RightParenthesis,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b',' => Token {
                token_type: TokenType::Comma,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'+' => Token {
                token_type: TokenType::OperatorPlus,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'{' => Token {
                token_type: TokenType::LeftBrace,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'}' => Token {
                token_type: TokenType::RightBrace,
                literal: ch.to_string(),
                line: self.line,
                column: self.column, 
            },
            0 => Token {
                token_type: TokenType::EOF,
                literal: "".to_string(),
                line: self.line,
                column: self.column,
            },
            b'0'..=b'9' => {
                let mut literal = String::new();
                let line = self.line;
                let column = self.column;
                while ch.is_ascii_digit() {
                    literal.push(ch as char);
                    ch = self.read_char();
                }
                // if the next token is a '.', this is a float literal
                if ch == b'.' || ch == b'f' {
                    literal.push(ch as char);
                    ch = self.read_char();
                    while ch.is_ascii_digit() {
                        literal.push(ch as char);
                        ch = self.read_char();
                    }
                    Token {
                        token_type: TokenType::LiteralFloat,
                        literal: literal.clone(),
                        line,
                        column
                    }
                } else {
                    Token {
                        token_type: TokenType::LiteralInteger,
                        literal: literal.clone(),
                        line,
                        column
                    }
                }
            }
            b'"' => {
                let mut literal = String::new();
                let line = self.line;
                let column = self.column;
                self.read_char();
                while ch != b'"' {
                    literal.push(ch as char);
                    ch = self.read_char();
                }
                self.read_char();
                Token {
                    token_type: TokenType::LiteralString,
                    literal: literal.clone(),
                    line,
                    column,
                }
            },
            b'/' => Token {
                token_type: TokenType::ForwardSlash,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let mut literal = String::new();
                let line = self.line;
                let column = self.column;
                while ch.is_ascii_alphanumeric() || ch == b'_' {
                    literal.push(ch as char);
                    ch = self.read_char();
                }
                match literal.as_str() {
                    "true" => Token {
                        token_type: TokenType::LiteralBool,
                        literal: literal,
                        line, column
                    },
                    "false" => Token {
                        token_type: TokenType::LiteralBool,
                        literal: literal,
                        line, column
                    },
                    "import" => Token {
                        token_type: TokenType::KeywordImport,
                        literal: literal,
                        line, column
                    },
                    "void" => Token {
                        token_type: TokenType::KeywordVoid,
                        literal: literal,
                        line, column
                    },
                    "int" => Token {
                        token_type: TokenType::KeywordInt,
                        literal: literal,
                        line, column
                    },
                    "float" => Token {
                        token_type: TokenType::KeywordFloat,
                        literal: literal,
                        line, column
                    },
                    "string" => Token {
                        token_type: TokenType::KeywordString,
                        literal: literal,
                        line, column
                    },
                    "bool" => Token {
                        token_type: TokenType::KeywordBool,
                        literal: literal,
                        line, column
                    },
                    "fun" => Token {
                        token_type: TokenType::KeywordFunction,
                        literal: literal,
                        line, column
                    },
                    "let" => Token {
                        token_type: TokenType::KeywordLet,
                        literal: literal,
                        line, column
                    },
                    _ => Token {
                        token_type: TokenType::Identifier,
                        literal: literal,
                        line, column
                    },
                }
            }
            _ => Token {
                token_type: TokenType::Illegal,
                literal: ch.to_string(),
                line: self.line,
                column: self.column,
            },
        }
    }
    fn read_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            let ch = self.input.as_bytes()[self.read_position];
            self.position = self.read_position;
            self.read_position += 1;
            if ch == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            ch
        }
    }
}
