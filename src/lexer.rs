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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }
    pub fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            b'%' => Token {
                token_type: TokenType::Percent,
                literal: self.ch.to_string(),
            },
            b'&' => {
                match self.peek_char() {
                    b'&' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorAnd,
                            literal: "&&".to_string(),
                        }
                    },
                    _ => Token {
                        token_type: TokenType::Illegal,
                        literal: self.ch.to_string(),
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
                        }
                    },
                    _ => Token {
                        token_type: TokenType::Illegal,
                        literal: self.ch.to_string(),
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
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorNot,
                        literal: self.ch.to_string(),
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
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorLesser,
                        literal: "<".to_string(),
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
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorGreater,
                        literal: ">".to_string(),
                    },
                }
            },
            b'*' => Token {
                token_type: TokenType::Asterisk,
                literal: self.ch.to_string(),
            },
            b':' => {
                if self.peek_char() == b':' {
                    self.read_char();
                    Token {
                        token_type: TokenType::NamespaceDelimiter,
                        literal: "::".to_string(),
                    }
                } else {
                    Token {
                        token_type: TokenType::Colon,
                        literal: self.ch.to_string(),
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
                        }
                    },
                    b'=' => {
                        self.read_char();
                        Token {
                            token_type: TokenType::OperatorEqual,
                            literal: "==".to_string(),
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorAssignment,
                        literal: self.ch.to_string(),
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
                        }
                    },
                    _ => Token {
                        token_type: TokenType::OperatorMinus,
                        literal: self.ch.to_string(),
                    },
                }
            },
            b';' => Token {
                token_type: TokenType::Semicolon,
                literal: self.ch.to_string(),
            },
            b'(' => Token {
                token_type: TokenType::LeftParenthesis,
                literal: self.ch.to_string(),
            },
            b')' => Token {
                token_type: TokenType::RightParenthesis,
                literal: self.ch.to_string(),
            },
            b',' => Token {
                token_type: TokenType::Comma,
                literal: self.ch.to_string(),
            },
            b'+' => Token {
                token_type: TokenType::OperatorPlus,
                literal: self.ch.to_string(),
            },
            b'{' => Token {
                token_type: TokenType::LeftBrace,
                literal: self.ch.to_string(),
            },
            b'}' => Token {
                token_type: TokenType::RightBrace,
                literal: self.ch.to_string(),
            },
            0 => Token {
                token_type: TokenType::EOF,
                literal: "".to_string(),
            },
            b'0'..=b'9' => {
                let mut literal = String::new();
                while self.ch.is_ascii_digit() {
                    literal.push(self.ch as char);
                    self.read_char();
                }
                // if the next token is a '.', this is a float literal
                if self.ch == b'.' || self.ch == b'f' {
                    literal.push(self.ch as char);
                    self.read_char();
                    while self.ch.is_ascii_digit() {
                        literal.push(self.ch as char);
                        self.read_char();
                    }
                    Token {
                        token_type: TokenType::LiteralFloat,
                        literal: literal,
                    }
                } else {
                    Token {
                        token_type: TokenType::LiteralInteger,
                        literal: literal,
                    }
                }
            }
            b'"' => {
                let mut literal = String::new();
                self.read_char();
                while self.ch != b'"' {
                    literal.push(self.ch as char);
                    self.read_char();
                }
                self.read_char();
                Token {
                    token_type: TokenType::LiteralString,
                    literal: literal,
                }
            },
            b'/' => Token {
                token_type: TokenType::ForwardSlash,
                literal: self.ch.to_string(),
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let mut literal = String::new();
                while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
                    literal.push(self.ch as char);
                    self.read_char();
                }
                match literal.as_str() {
                    "true" => Token {
                        token_type: TokenType::LiteralBool,
                        literal: literal,
                    },
                    "false" => Token {
                        token_type: TokenType::LiteralBool,
                        literal: literal,
                    },
                    "import" => Token {
                        token_type: TokenType::KeywordImport,
                        literal: literal,
                    },
                    "void" => Token {
                        token_type: TokenType::KeywordVoid,
                        literal: literal,
                    },
                    "int" => Token {
                        token_type: TokenType::KeywordInt,
                        literal: literal,
                    },
                    "float" => Token {
                        token_type: TokenType::KeywordFloat,
                        literal: literal,
                    },
                    "string" => Token {
                        token_type: TokenType::KeywordString,
                        literal: literal,
                    },
                    "bool" => Token {
                        token_type: TokenType::KeywordBool,
                        literal: literal,
                    },
                    "fun" => Token {
                        token_type: TokenType::KeywordFunction,
                        literal: literal,
                    },
                    "let" => Token {
                        token_type: TokenType::KeywordLet,
                        literal: literal,
                    },
                    _ => Token {
                        token_type: TokenType::Identifier,
                        literal: literal,
                    },
                }
            }
            _ => Token {
                token_type: TokenType::Illegal,
                literal: self.ch.to_string(),
            },
        };
        self.read_char();
        token
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}
