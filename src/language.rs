/*
Test:
-InvalidCharacter in the middle of tockens.
-UnterminatedBlockComment
-Closing of block comments with * not followed by /
-Opening block comment vs division + multiplication... I guess both would be correct.
-Difference between int.int and float.

*/
use std::fmt;

pub const SIGMA: [char; 85] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
    'V', 'W', 'X', 'Y', 'Z', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '[',
    ']', '_', '{', '}', '&', '|', ' ', '\t', '\n',
];

pub const NONZERO: [char; 9] = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const DIGIT: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const LETTER: [char; 52] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];
pub const SYMBOL: [char; 20] = [
    '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '[', ']', '_', '{', '}', '&',
    '|',
];

pub const WHITESPACE: [char; 3] = [' ', '\t', '\n'];

pub const RESERVED_WORDS: [&str; 11] = [
    "if", "then", "else", "for", "class", "integer", "float", "read", "write", "return", "main",
];

// const ID: &str = "id";
// const KEYWORD: &str = "keyword";
// const INTEGER: &str = "integer";
// const FLOAT: &str = "float";
// const LINE_COMMENT: &str = "line_comment";
// const BLOCK_COMMENT: &str = "block_comment";
// const SMALLER: &str = "smaller";
// const SMALLER_OR_EQUAL: &str = "smaller_or_equal";
// const NOT_EQUAL: &str = "not_equal";
// const GREATER: &str = "greater";
// const GREATER_OR_EQUAL: &str = "greater_or_equal";
// const ASSIGNMENT: &str = "assignment";
// const EQUAL: &str = "equal";

#[derive(Debug, Clone, Copy)]
pub enum LexicalError {
    InvalidCharacter,
    InvalidToken,
    UnterminatedBlockComment,
}
impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::InvalidCharacter => write!(f, "Lexical Error: Invalid Character"),
            LexicalError::InvalidToken => write!(f, "Lexical Error: Invalid Token"),
            LexicalError::UnterminatedBlockComment => {
                write!(f, "Lexical Error: Unterminated Block Comment")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    If,
    Then,
    Else,
    For,
    Class,
    Integer,
    Float,
    Read,
    Write,
    Return,
    Main,
}

impl Keyword {
    pub fn from_str(keyword: &str) -> Self {
        match keyword {
            "if" => Keyword::If,
            "then" => Keyword::Then,
            "else" => Keyword::Else,
            "for" => Keyword::For,
            "class" => Keyword::Class,
            "integer" => Keyword::Integer,
            "float" => Keyword::Float,
            "read" => Keyword::Read,
            "write" => Keyword::Write,
            "return" => Keyword::Return,
            "main" => Keyword::Main,
            _ => panic!("Invalid keyword."),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Smaller,
    SmallerOrEqual,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Assignment,
    Equal,
    Addition,
    Substraction,
    Multiplication,
    Division,
    And,
    Not,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum Separator {
    SemiColon,
    Coma,
    Period,
    Colon,
    ScopeResolution,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
}

#[derive(Debug, Clone, Copy)]
pub enum Comment {
    BlockComment,
    LineComment,
}

#[derive(Clone, Copy)]
pub enum TokenType {
    Id,
    Integer,
    Float,
    Keyword(Keyword),
    Operator(Operator),
    Separator(Separator),
    Comment(Comment),
    InvalidToken,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //use Operator::*;
        match self {
            TokenType::Id => write!(f, "id"),
            TokenType::Integer => write!(f, "integer"),
            TokenType::Float => write!(f, "float"),
            TokenType::Keyword(keyword) => write!(f, "keyword: {:?}", keyword),
            TokenType::Operator(operator) => write!(f, "operator: {:?}", operator),
            TokenType::Separator(separator) => write!(f, "separator: {:?}", separator),
            TokenType::Comment(comment) => write!(f, "{:?}", comment),
            TokenType::InvalidToken => unreachable!(),
            // match operator {
            //     Smaller => write!(f, "smaller"),
            //     SmallerOrEqual => write!(f, "smaller_or_equal"),
            //     NotEqual => write!(f, "not_equal"),
            //     Greater => write!(f, "greater"),
            //     GreaterOrEqual => write!(f, "greater_or_equal"),
            //     Assignment => write!(f, "assignment"),
            //     Equal => write!(f, "equal"),
            // },
        }
    }
}
