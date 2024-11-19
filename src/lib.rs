//! A library for interpreting D&D-style dice notation
//! # Example
//! ```
//! let text = "2d6 - 1d8 + 10"
//! let mut parser = sdi::Parser::from_text(text);
//! let ast = parser.parse();
//! let result = ast.eval();
//! println("you rolled a {:?}");
//! ```

use rand::prelude::*;

fn roll(count: usize, size: usize) -> usize {
    let mut result = 0;
    let mut rng = thread_rng();
    for _ in 0..count {
        result += rng.gen_range(1..=size);
    }
    result
}


/// The different types of tokens the Lexer can process
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    /// A literal (unsigend) integer
    INT(usize),
    /// The '+' sign
    ADD,
    /// The '-' sign
    SUB,
    /// The '*' sign
    MUL,
    /// The '/' sign
    DIV,
    /// The left parentheses '('
    LPAREN,
    /// The right parentheses ')'
    RPAREN,
    /// The 'd' character
    DICE
}

/// The Lexer converts from a string to a vector of tokens (tokenization)
#[derive(Clone)]
pub struct Lexer {
    text: Vec<char>,
    position: usize,
}

impl Lexer {
    /// Create a new lexer for a text
    /// * `input` - The input text that is going to be tokenized
    pub fn new(
        text: impl Into<String>
    ) -> Lexer {
        let str: String = text.into();
        Lexer {
            text: str.chars().collect(),
            position: 0
        }
    }
    /// Get the current character
    fn get_char(&self, pos: usize) -> Option<char> {
        self.text.get(pos).copied()
    }
    /// Check if `c` is considered a whitespace character
    /// for now `' '`, `'\n'` and `'\t'` are considered whitespace
    fn is_whitespace(c: &char) -> bool {
        match c {
            ' ' | '\n' | '\t' => true,
            _ => false
        }
    }
    /// Keep incrementing position until the current character isn't whitespace
    fn skip_whitespace(&mut self) {
        while {
            // Check if we can even get a character
            if let Some(c) = self.get_char(self.position) {
                // If there is a character, is it a whitespace?
                Lexer::is_whitespace(&c)
            } else {
                // No character -> no more text -> no more whitespace
                false
            }
        } {
            self.position += 1;
        }
    }
    /// Get the next token if it exists
    pub fn get_next_token(&mut self) -> Option<Token> {
        if self.position == self.text.len() {
            // text is exhausted
            return None;
        }

        // Ignore whitespace in between tokens
        self.skip_whitespace();

        // Get the current character
        let char = self.get_char(self.position).unwrap();

        // Numbers can have different lengths, so we have to
        // handle them differently
        if char.is_ascii_digit() {
            // Create a buffer
            let mut number: String = char.into();

            loop {
                self.position += 1;
                if let Some(char) = self.get_char(self.position) {
                    if char.is_ascii_digit() {
                        number.push(char);
                    } else {
                        // No more digits
                        break;
                    }
                } else {
                    // Text is exhausted
                    break;
                }
            }
            // if the number isn't able to be parsed it's to big
            // to fit in a usize
            if let Ok(value) = number.parse::<usize>() {
                return Some(Token::INT(value));
            } else {
                // TODO: raise an exception instead
                return None;
            }
        }

        // TODO: handle and skip over comments

        // following tokens are all 1 character long so we can already
        // increment the current position
        self.position += 1;
        match char.to_ascii_lowercase() {
            '+' => Some(Token::ADD),
            '-' => Some(Token::SUB),
            '*' => Some(Token::MUL),
            '/' => Some(Token::DIV),
            '(' => Some(Token::LPAREN),
            ')' => Some(Token::RPAREN),
            'd' => Some(Token::DICE),
            _ => None
        }
    }
}

/// Nodes are part of an abstract syntax tree
#[derive(Debug)]
pub enum Node {
    /// A node representing a binary operation
    /// * `lhs` - the left hand side of the operation
    /// * `op` - the operator itself
    /// * `rhs` - the right hand side of the operation
    BinOp {lhs: Box<Node>, op: Token, rhs: Box<Node>},
    /// A node representing a unary operation
    /// * `op` - the operator itself
    /// * `child` - the node the operator gets executed on
    UnOp  {op: Token, child: Box<Node>},
    /// A leaf node means a literal (no operator is executed on
    /// the value)
    /// * `value` - the value of the leaf node (usually an integer literal)
    Leaf {value: Token}
}

impl Node {
    /// Evaluate the AST with this node as the root node.
    pub fn eval(&self) -> isize {
        let result = match &self {
            Self::BinOp { lhs, op, rhs } => {
                match &op {
                    Token::ADD => {
                        lhs.eval() + rhs.eval()
                    },
                    Token::SUB => {
                        lhs.eval() - rhs.eval()
                    },
                    Token::MUL => {
                        lhs.eval() * rhs.eval()
                    },
                    Token::DIV => {
                        lhs.eval() / rhs.eval()
                    },
                    Token::DICE => {
                        let count = lhs.eval();
                        let size = rhs.eval();
                        assert!(count > 0 && size > 0);
                        roll(count as usize, size as usize) as isize
                    },
                    _ => {unreachable!()}
                }
            },
            Self::UnOp { op, child } => {
                match &op {
                    Token::SUB => {
                        -child.eval()
                    },
                    Token::DICE => {
                        let size = child.eval();
                        assert!(size > 0);
                        roll(1, size as usize) as isize
                    }
                    _ => {unreachable!()}
                }
            },
            Self::Leaf { value } => {
                if let Token::INT(v) = value {
                    *v as isize
                } else {
                    unreachable!()
                }
            }
        };
        result
    }
}

/// Parses a text using a lexer into an abstract syntax tree
pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
}

impl Parser {
    /// Create a new parser to parse the output of a lexer
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {current_token: lexer.get_next_token(), lexer}
    }
    /// Create a lexer from a piece of text
    pub fn from_text(text: impl Into<String>) -> Parser {
        let mut lexer = Lexer::new(text);
        Parser {current_token: lexer.get_next_token(), lexer}
    }
    /// 'eats' any token (fetches the next token and stores it)
    pub fn eat_any(&mut self) {
        self.current_token = self.lexer.get_next_token();
    }

    /// a factor is something you would evaluate first, or is something
    /// that is already evaluated
    fn factor(&mut self) -> Node {
        match &self.current_token.clone().unwrap() {
            Token::INT(v) => {
                self.eat_any();
                Node::Leaf {value: Token::INT(*v)}
            },
            Token::LPAREN => {
                self.eat_any();
                let node = self.expr();
                self.eat_any();
                node
            },
            Token::SUB => {
                self.eat_any();
                Node::UnOp {
                    op: Token::SUB,
                    child: Box::new(self.factor())
                }
            },
            Token::DICE => {
                self.eat_any();
                Node::UnOp {
                    op: Token::DICE,
                    child: Box::new(self.factor())
                }
            },
            _ => {unreachable!()}
        }
    }
    /// a term is something you would evaluate before addition or
    /// subtraction
    fn term(&mut self) -> Node {
        let mut node = self.factor();
        while {
            if let Some(token) = &self.current_token {
                match token {
                    Token::MUL | Token::DIV | Token::DICE => true,
                    _ => false
                }
            } else {
                false
            }
        } {
            let token = self.current_token.clone().unwrap();
            self.eat_any();
            node = Node::BinOp {
                lhs: Box::new(node),
                op: token,
                rhs: Box::new(self.factor())
            }
        }
        node
    }
    /// an expression is the 'last' level of evaluation, in this
    /// case anything before addition or subtraction
    fn expr(&mut self) -> Node {
        let mut node = self.term();
        while {
            if let Some(token) = &self.current_token {
                match token {
                    Token::ADD | Token::SUB => true,
                    _ => false
                }
            } else {
                false
            }
        } {
            let token = self.current_token.clone().unwrap();
            self.eat_any();
            node = Node::BinOp {
                lhs: Box::new(node),
                op: token,
                rhs: Box::new(self.term())
            }
        }
        node
    }
    /// Parse the text
    pub fn parse(&mut self) -> Node {
        self.expr()
    }
}
