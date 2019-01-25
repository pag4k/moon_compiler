use std::fmt;

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
pub const SYMBOL: [char; 21] = [
    'S', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '[', ']', '_', '{', '}',
    '&', '|',
];

pub const WHITESPACE: [char; 4] = ['W', ' ', '\t', '\n'];

pub const RESERVED_WORDS: [&str; 11] = [
    "if", "then", "else", "for", "class", "integer", "float", "read", "write", "return", "main",
];

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

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
            TokenType::LexicalError(error) => write!(f, "{}", error),
        }
    }
}
