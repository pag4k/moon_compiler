use crate::language::*;
use crate::lexical_analyzer_table::*;

use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;
use std::str::FromStr;

/// Location ADT
#[derive(Debug, Clone, Copy)]
pub struct Location {
    line: usize,
    column: usize,
}
impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[line:{}, column:{}]", self.line, self.column)
    }
}

/// Token ADT
#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub location: Location,
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(lexeme) = &self.lexeme {
            write!(f, "[{}: '{}', {}]", self.token_type, lexeme, self.location)
        } else {
            write!(f, "[{}, {}]", self.token_type, self.location)
        }
    }
}
impl Token {
    /// Return a new Token
    fn new(token_type: TokenType, lexeme: Option<&str>, location: Location) -> Self {
        Token {
            token_type,
            lexeme: lexeme.map(ToString::to_string),
            location,
        }
    }
}

/// TokenError ADT
#[derive(Clone)]
pub struct TokenError {
    pub error_type: LexicalError,
    lexeme: String,
    location: Location,
}
impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "[{}: '{}', {}]",
            self.error_type, self.lexeme, self.location
        )
    }
}
impl TokenError {
    /// Return a new TokenError
    fn new(error_type: LexicalError, lexeme: &str, location: Location) -> Self {
        TokenError {
            error_type,
            lexeme: lexeme.to_string(),
            location,
        }
    }
}

/// LexicalAnalyzer ADT
pub struct LexicalAnalyzer<'a> {
    chars: Peekable<Chars<'a>>,
    location: Location,
    table: LexicalAnalyzerTable,
}

impl<'a> LexicalAnalyzer<'a> {
    /// Return a LexicalAnalyzer based a reference to a source String
    pub fn from_string(source: &'a str) -> Self {
        LexicalAnalyzer {
            chars: source.chars().peekable(),
            location: Location { line: 1, column: 0 },
            table: LexicalAnalyzerTable::default(),
        }
    }
    /// Return the next Token or TokenError
    ///
    /// # Remarks
    ///
    /// The return type is a Result in an Option. It will return None when there are no tokens
    /// left. The Result is used to differentiate Token and TokenType.
    pub fn next_token(&mut self) -> Option<Result<Token, TokenError>> {
        // The current state of the DFA.
        let mut state = self.table.get_initial_state();
        // The characters that have been parsed so far for the current token.
        let mut string = String::new();
        // The location of the first character of the current token.
        let mut location: Option<Location> = None;

        // Feed characters to the table until a final state is reached.
        while !self.table.is_final_state(state) {
            // Peek the next character and react according to whether this is the first character
            // of the current token or not. There are 5 cases:
            let lookup = match (self.peek(), string.is_empty()) {
                // Case 1: '\n' and string is is_empty.
                // Advance the char iterator, update location, and go to next iteration.
                (Some('\n'), true) => {
                    self.next_char();
                    self.location.line += 1;
                    self.location.column = 0;
                    continue;
                }
                // Case 2: Whitespace (not '\n') and string is is_empty.
                // Advance the char iterator and go to next iteration.
                // TODO: The whitespace types are hardcoded. It would be better to use the language
                //       definition.
                (Some(' '), true) | (Some('\t'), true) => {
                    self.next_char();
                    continue;
                }
                // Case 3: Any other character.
                // Assign the character to lookup and keep going.
                (Some(c), _) => c,
                // Case 4: No other character and string is no empty.
                // Call the special table function to try to reach a final state.
                (None, false) => match self.table.abort(state) {
                    // If success, get that final state and break out of the loop.
                    Ok(new_state) => {
                        state = new_state;
                        break;
                    }
                    // If failure, return a ErrorToken describing the error.
                    Err(error_type) => {
                        return Some(Err(TokenError::new(
                            error_type,
                            &string,
                            location.expect("No location."),
                        )));
                    }
                },
                // Case 5: No other character and string is empty.
                // Return None.
                (None, true) => {
                    //println!("End");
                    return None;
                }
            };

            // If the string is empty, set the location.
            if string.is_empty() {
                location = Some(self.location);
            }

            // Send the lookup character to the table.
            match (self.table.next(state, lookup), string.is_empty()) {
                // If succes, update the state.
                (Ok(new_state), _) => state = new_state,
                // If failure and the string is empty, return an ErrorToken.
                (Err(error_type), true) => {
                    string.push(lookup);
                    self.next_char();
                    return Some(Err(TokenError::new(
                        error_type,
                        &string,
                        location.expect("No location."),
                    )));
                }
                // If failure and the string is not empty, call the special table function to try
                // to reach a final state.
                (Err(_), false) => {
                    match self.table.abort(state) {
                        // If success, get that final state and break out of the loop.
                        Ok(new_state) => {
                            state = new_state;
                            break;
                        }
                        // If failure, return a ErrorToken describing the error.
                        Err(error_type) => {
                            return Some(Err(TokenError::new(
                                error_type,
                                &string,
                                location.expect("No location."),
                            )));
                        }
                    };
                }
            }

            // If the new state is not a backtrack state, advance to next character.
            if !self.table.is_backtrack_state(state) {
                string.push(lookup);
                self.next_char();
            }
        }

        // After the loop, try to get the token type corresponding to the final state.
        match self.table.get_token_type(state) {
            // If succes, return the appropriate Token.
            Some(token_type) => match token_type {
                // If the token type is an error, return an ErrorToken.
                TokenType::LexicalError(error_type) => Some(Err(TokenError::new(
                    error_type,
                    &string,
                    location.expect("No location."),
                ))),
                // If the token type is an Id, verify if it is a keyword.
                TokenType::Id => {
                    let token_type = match KeywordType::from_str(&string) {
                        Ok(keyword) => TokenType::Keyword(keyword),
                        Err(_) => TokenType::Id,
                    };
                    Some(Ok(Token::new(
                        token_type,
                        Some(&string),
                        location.expect("No location."),
                    )))
                }
                // For any other token type, return a Token.
                _ => Some(Ok(Token::new(
                    token_type,
                    Some(&string),
                    location.expect("No location."),
                ))),
            },
            // If failure, return a an InvalidToken ErrorToken.
            // This should never happen unless there is a problem with the table.
            None => Some(Err(TokenError::new(
                LexicalError::InvalidToken,
                &string,
                location.expect("No location."),
            ))),
        }
    }
    /// Get the next character in the source and advance the char iterator.
    fn next_char(&mut self) -> Option<char> {
        self.location.column += 1;
        self.chars.next()
    }
    /// Get a copy of the next character without advancing the char iterator.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }
}

/// LexicalAnalyzerIntoIterator ADT
/// It is needed to convert a LexicalAnalyzer into an IntoIterator.
pub struct LexicalAnalyzerIntoIterator<'a> {
    lexical_analyzer: LexicalAnalyzer<'a>,
    index: usize,
}
impl<'a> Iterator for LexicalAnalyzerIntoIterator<'a> {
    type Item = Result<Token, TokenError>;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.index += 1;
        self.lexical_analyzer.next_token()
    }
}
impl<'a> IntoIterator for LexicalAnalyzer<'a> {
    type Item = Result<Token, TokenError>;
    type IntoIter = LexicalAnalyzerIntoIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        LexicalAnalyzerIntoIterator {
            lexical_analyzer: self,
            index: 0,
        }
    }
}
