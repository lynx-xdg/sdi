//! A library for interpreting D&D-style dice notation
use rand::prelude::*;
use std::collections::HashMap;

pub fn roll(not: impl Into<String>) -> EvalResult {
    let mut parser = Parser::from_text(not);
    let ast = parser.parse();
    ast.eval()
}

fn roll_single(count: usize, size: usize) -> EvalResult {
    let mut rng = thread_rng();
    // do the roll `count` times
    let mut rolls = Vec::new();
    let mut total = 0;
    for _ in 0..count {
        let roll: usize = rng.gen_range(1..=size);
        rolls.push(roll);
        total += roll;
    }
    let mut hmap = HashMap::new();
    hmap.insert(size, rolls);
    EvalResult {
        total: total as isize,
        rolls: hmap
    }
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
    DICE,
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
    Leaf {value: Token},
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::BinOp { lhs, op, rhs } => {
                if let Self::BinOp { lhs: olhs, op: oop, rhs: orhs } = other {
                    return lhs == olhs && op == oop && rhs == orhs;
                }
            }
            Self::UnOp { op, child } => {
                if let Self::UnOp { op: oop, child: ochild } = other {
                    return op == oop && child == ochild;
                }
            },
            Self::Leaf { value } => {
                if let Self::Leaf { value: ovalue } = other {
                    return value == ovalue;
                }
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct EvalResult {
    //             type   each roll
    rolls: HashMap<usize, Vec<usize>>,
    total: isize
}

fn merge_hashmap(a: HashMap<usize, Vec<usize>>, b: HashMap<usize, Vec<usize>>) -> HashMap<usize, Vec<usize>> {
    let mut result = HashMap::new();
    for (key, value) in a.iter() {
        result.insert(*key, value.clone());
    }
    for (key, value) in b.iter() {
        if let Some(v) = result.get_mut(key) {
            v.extend(value);
        } else {
            result.insert(*key, value.clone());
        }
    }
    result
}

use std::ops::{Add, Neg, Sub, Mul, Div};
impl Neg for EvalResult {
    fn neg(self) -> Self::Output {
        EvalResult {
            rolls: self.rolls,
            total: - self.total
        }
    }
    type Output = Self;
}
impl Add for EvalResult {
    fn add(self, rhs: Self) -> Self::Output {
        EvalResult {
            rolls: merge_hashmap(self.rolls, rhs.rolls),
            total: self.total + rhs.total
        }
    }
    type Output = Self;
}
impl Sub for EvalResult {
    fn sub(self, rhs: Self) -> Self::Output {
        EvalResult {
            rolls: merge_hashmap(self.rolls, rhs.rolls),
            total: self.total - rhs.total
        }
    }
    type Output = Self;
}
impl Mul for EvalResult {
    fn mul(self, rhs: Self) -> Self::Output {
        EvalResult {
            rolls: merge_hashmap(self.rolls, rhs.rolls),
            total: self.total - rhs.total
        }
    }
    type Output = Self;
}
impl Div for EvalResult {
    fn div(self, rhs: Self) -> Self::Output {
        EvalResult {
            rolls: merge_hashmap(self.rolls, rhs.rolls),
            total: self.total - rhs.total
        }
    }
    type Output = Self;
}

impl Node {
    /// Evaluate the AST with this node as the root node.
    pub fn eval(&self) -> EvalResult {
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
                        let lhse = lhs.eval();
                        let rhse = rhs.eval();
                        let count = lhse.total;
                        let size = rhse.total;
                        assert!(count > 0 && size > 0);
                        roll_single(count as usize, size as usize)
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
                        let size = child.eval().total;
                        assert!(size > 0);
                        roll_single(1, size as usize)
                    }
                    _ => {unreachable!()}
                }
            },
            Self::Leaf { value } => {
                if let Token::INT(v) = value {
                    EvalResult {
                        rolls: HashMap::new(),
                        total: *v as isize
                    }
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

#[cfg(test)]
mod tests {
    use super::{*, Node::*, Token::*};
    #[test]
    fn parse_roll() {
        let input = "1d4";
        let expected_output = BinOp {
            lhs: Box::new(Leaf {
                value: INT(1)
            }),
            op: DICE,
            rhs: Box::new(Leaf {
                value: INT(4)
            })
        };
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse();
        assert_eq!(ast, expected_output);
    }

    #[test]
    fn parse_basic_op() {
        let input = "1 + 2 * 3 / 4 - 5";
        let expected_output = BinOp {
            lhs: Box::new(BinOp {
                lhs: Box::new(Leaf { value: INT(1) }),
                op: ADD,
                rhs: Box::new(BinOp {
                    lhs: Box::new(BinOp {
                        lhs: Box::new(Leaf { value: INT(2) }),
                        op: MUL,
                        rhs: Box::new(Leaf { value: INT(3) })
                    }),
                    op: DIV,
                    rhs: Box::new(Leaf { value: INT(4) })
                })
            }),
            op: SUB,
            rhs: Box::new(Leaf { value: INT(5) })
        };
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse();
        println!("{:?}", ast);
        assert_eq!(ast, expected_output);
    }
}
