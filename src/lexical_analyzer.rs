use super::language::*;
use super::lexical_analyzer_table::*;

use std::fmt;

use std::iter::Peekable;
use std::str::Chars;

struct Location {
    line: usize,
    column: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line:{}, column:{}]", self.line, self.column)
    }
}

pub struct Token {
    token_type: TokenType,
    lexeme: Option<String>,
    location: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(lexeme) = &self.lexeme {
            write!(f, "[{}: {}, {}]", self.token_type, lexeme, self.location)
        } else {
            write!(f, "[{}, {}]", self.token_type, self.location)
        }
    }
}

impl Token {
    fn new(token_type: TokenType, lexeme: Option<&str>, location: Location) -> Self {
        Token {
            token_type,
            lexeme: lexeme.map(|lexeme| lexeme.to_string()),
            location,
        }
    }
}

pub struct Error {
    error_type: LexicalError,
    lexeme: String,
    location: Location,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}: {}, {}]",
            self.error_type, self.lexeme, self.location
        )
    }
}

impl Error {
    fn new(error_type: LexicalError, lexeme: &str, location: Location) -> Self {
        Error {
            error_type,
            lexeme: lexeme.to_string(),
            location,
        }
    }
}

pub struct LexicalAnalyzer<'a> {
    chars: Peekable<Chars<'a>>,
    location: Location,
    table: LexicalAnalyzerTable,
}

impl<'a> LexicalAnalyzer<'a> {
    pub fn from_string(source: &'a str) -> Self {
        LexicalAnalyzer {
            chars: source.chars().peekable(),
            location: Location { line: 1, column: 0 },
            table: LexicalAnalyzerTable::default(),
        }
    }

    pub fn next_token(&mut self) -> Option<Result<Token, Error>> {
        let mut state = self.table.get_initial_state();
        let mut string = String::new();
        let mut location: Option<Location> = None;
        while !self.table.is_final_state(state) {
            //Maybe add counters for the location?
            //Not sure if this is sure to always work.
            //Unwrap for now.
            let lookup = match (self.peek(), string.is_empty()) {
                (Some(' '), true) | (Some('\t'), true) => {
                    self.next_char();
                    continue;
                }
                (Some('\n'), true) => {
                    self.next_char();
                    self.location.line += 1;
                    self.location.column = 0;
                    continue;
                }
                (Some(c), _) => {
                    //dbg!(c);
                    //dbg!(string.clone());
                    c
                }
                (None, true) => {
                    //println!("End");
                    return None;
                }
                //No char is like a space?
                (None, _) => match self.table.abort(state) {
                    Ok(new_state) => {
                        state = new_state;
                        break;
                    }
                    Err(error_type) => {
                        return Some(Err(Error::new(
                            error_type,
                            &string,
                            location.expect("No location."),
                        )));
                    }
                },
            };

            if string.is_empty() {
                location = Some(self.get_location());
            }

            match self.table.next(state, lookup) {
                Ok(new_state) => state = new_state,
                Err(error_type) => {
                    if string.is_empty() {
                        //dbg!("Error");
                        string.push(lookup);
                        self.next_char();
                        return Some(Err(Error::new(
                            error_type,
                            &string,
                            location.expect("No location."),
                        )));
                    } else {
                        //This does not work for block comment since they can have multiple lines.
                        //I could add a force end symbol.
                        //This is a bad way to force to get a final state.
                        state = self.table.next(state, '\n').unwrap();
                        break;
                    }
                }
            }

            //state = state.table(lookup);
            //state = self.table.next(state, lookup);
            //println!("State: {} {}", lookup, state);

            //if state.need_backtrack() {
            if self.table.is_backtrack_state(state) {
                //println!("BACK");
            } else {
                string.push(lookup);
                self.next_char();
            }
        }

        let token_type = self.table.get_token_type(state);
        if let Some(token_type) = token_type {
            match token_type {
                TokenType::InvalidToken => Some(Err(Error::new(
                    LexicalError::InvalidToken,
                    &string,
                    location.expect("No location."),
                ))),
                _ => Some(Ok(Token::new(
                    token_type,
                    Some(&string),
                    location.expect("No location."),
                ))),
            }
        //println!("Token: {} {}.", token_type.unwrap(), string);
        } else {
            unreachable!(&string);
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.location.column += 1;
        self.chars.next()
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }

    fn get_location(&self) -> Location {
        Location {
            line: self.location.line,
            column: self.location.column,
        }
    }
}

pub struct LexicalAnalyzerIntoIterator<'a> {
    lexical_analyzer: LexicalAnalyzer<'a>,
    index: usize,
}

impl<'a> Iterator for LexicalAnalyzerIntoIterator<'a> {
    type Item = Result<Token, Error>;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.index += 1;
        self.lexical_analyzer.next_token()
    }
}

impl<'a> IntoIterator for LexicalAnalyzer<'a> {
    type Item = Result<Token, Error>;
    type IntoIter = LexicalAnalyzerIntoIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        LexicalAnalyzerIntoIterator {
            lexical_analyzer: self,
            index: 0,
        }
    }
}
