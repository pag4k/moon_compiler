use std::fmt::{Display, Formatter, Result};

// Define the all the arrays of char describing the language.
// Note that the first char is used as an identifier of the array.
pub const SIGMA: [char; 87] = [
    'Σ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A',
    'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '!', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=',
    '>', '[', ']', '_', '{', '}', '&', '|', ' ', '\t', '\n',
];
pub const NONZERO: [char; 10] = ['N', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const DIGIT: [char; 11] = ['D', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const LETTER: [char; 53] = [
    'L', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

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

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        //use Operator::*;
        match self {
            TokenType::Id => write!(f, "Id"),
            TokenType::Integer => write!(f, "Integer"),
            TokenType::Float => write!(f, "Float"),
            TokenType::Keyword(keyword) => write!(f, "{:?}", keyword),
            TokenType::Operator(operator) => write!(f, "{:?}", operator),
            TokenType::Separator(separator) => write!(f, "{:?}", separator),
            TokenType::Comment(comment) => write!(f, "{:?}", comment),
            TokenType::LexicalError(error) => write!(f, "{}", error),
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
    pub fn from_str(keyword: &str) -> Option<Self> {
        match keyword {
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "for" => Some(Keyword::For),
            "class" => Some(Keyword::Class),
            "integer" => Some(Keyword::Integer),
            "float" => Some(Keyword::Float),
            "read" => Some(Keyword::Read),
            "write" => Some(Keyword::Write),
            "return" => Some(Keyword::Return),
            "main" => Some(Keyword::Main),
            _ => None,
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
    Subtraction,
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

#[derive(Debug, Clone, Copy)]
pub enum LexicalError {
    InvalidCharacter,
    InvalidToken,
    InvalidId,
    FloatMissingFraction,
    FloatTrailingZeros,
    FloatMissingExponent,
    IncompleteAnd,
    IncompleteOr,
    UnterminatedBlockComment,
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            LexicalError::InvalidCharacter => write!(f, "Lexical Error: Invalid Character"),
            LexicalError::InvalidToken => write!(f, "Lexical Error: Invalid Token"),
            LexicalError::InvalidId => write!(f, "Lexical Error: Invalid Id"),
            LexicalError::FloatMissingFraction => {
                write!(f, "Lexical Error: Float Missing Fraction")
            }
            LexicalError::FloatTrailingZeros => write!(f, "Lexical Error: Float Trailing Zeros"),
            LexicalError::FloatMissingExponent => {
                write!(f, "Lexical Error: Float Missing Exponent")
            }
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
