use crate::language::*;
use crate::lexical_analyzer::*;

use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub enum SyntacticError {
    WrongTerminal(Token, TokenType, Option<Location>),
    NotInTableButInFollow(Token, VariableType, bool),
    NotInTableNorInFollow(Token, VariableType, Option<Location>),
    TokenAfterMain(Token),
}

impl Display for SyntacticError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SyntacticError::*;
        match self {
            WrongTerminal(token, expected_type, recovery_location) => write!(
                f,
                "Syntactic error at {}: Expecting {} but found {}. {}.",
                token.location,
                expected_type,
                token.token_type,
                match recovery_location {
                    Some(location) => format!("Recovered at {}", location),
                    None => "Could not recover, reached end of program".to_string(),
                }
            ),
            NotInTableButInFollow(token, variable, recovered) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}. {}.",
                token.location,
                variable,
                token.token_type,
                if *recovered {
                    "Skipping and continuing"
                } else {
                    "Tried to skip, but it is the end of the program"
                }
            ),
            NotInTableNorInFollow(token, variable, recovery_location) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}. {}.",
                token.location,
                variable,
                token.token_type,
                match recovery_location {
                    Some(location) => format!("Recovered at {}", location),
                    None => "Could not recover, reached end of program".to_string(),
                }
            ),
            TokenAfterMain(token) => write!(
                f,
                "Syntactic error at {}: Token \"{}\" after main function.",
                token.location,
                token.lexeme.clone().unwrap()
            ),
        }
    }
}

impl TokenLocation for SyntacticError {
    fn get_location(&self) -> Location {
        use SyntacticError::*;
        match self {
            WrongTerminal(token, _, _) => token.location,
            NotInTableButInFollow(token, _, _) => token.location,
            NotInTableNorInFollow(token, _, _) => token.location,
            TokenAfterMain(token) => token.location,
        }
    }
}

impl SyntacticError {
    pub fn failed_to_recover(&self) -> bool {
        use SyntacticError::*;
        match self {
            WrongTerminal(_, _, recovery_location) => recovery_location.is_none(),
            NotInTableButInFollow(_, _, recovered) => !recovered,
            NotInTableNorInFollow(_, _, recovery_location) => recovery_location.is_none(),
            TokenAfterMain(_) => false,
        }
    }
}
