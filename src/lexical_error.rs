use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
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
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
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
