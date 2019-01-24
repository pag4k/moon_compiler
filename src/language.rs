/*
Test:
-InvalidCharacter in the middle of tockens.
-UnterminatedBlockComment
-Closing of block comments with * not followed by /
-Opening block comment vs division + multiplication... I guess both would be correct.
-Difference between int.int and float.
-All possible errors in a float

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

#[derive(Debug, Clone, Copy)]
pub enum LexicalError {
    InvalidCharacter,
    InvalidToken,
    IncompleteFloat,
    IncompleteAnd,
    IncompleteOr,
    UnterminatedBlockComment,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::InvalidCharacter => write!(f, "Lexical Error: Invalid Character"),
            LexicalError::InvalidToken => write!(f, "Lexical Error: Invalid Token"),
            LexicalError::IncompleteFloat => write!(f, "Lexical Error: Incomplete Float"),
            LexicalError::IncompleteAnd => {
                write!(f, "Lexical Error: Incomplete And (did you mean '&&'?)")
            }
            LexicalError::IncompleteOr => {
                write!(f, "Lexical Error: Incomplete Or (did you mean '||'?)")
            }

            LexicalError::UnterminatedBlockComment => write!(
                f,
                "Lexical Error: Unterminated Block Comment (you are missing '*/')"
            ),
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
    LexicalError(LexicalError),
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
            TokenType::LexicalError(_) => unreachable!(),
        }
    }
}
