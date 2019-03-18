use crate::finite_accepter::*;
use crate::language::*;
use crate::lexical_error::*;
use crate::nfa_generator::*;

use std::collections::{HashMap, HashSet};

/// LexicalAnalyzerTable ADT
pub struct LexicalAnalyzerTable {
    dfa: DeterministicFiniteAccepter,
    tokens: HashMap<usize, TokenType>,
    backtrack: HashSet<usize>,
}

impl Default for LexicalAnalyzerTable {
    /// Return a LexicalAnalyzerTable that is constructed from the NFA generated by the
    /// nfa_generator() function.
    fn default() -> Self {
        let (nfa, nfa_tokens, nfa_backtrack) = define_nfa_table();
        let (dfa, nfa_to_dfa_states_map) = DeterministicFiniteAccepter::from_nfa(nfa, &SIGMA);

        // Find the tokens associated to the DFA final states.
        let mut dfa_tokens: HashMap<usize, TokenType> = HashMap::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            //Assuming there is only one final NFA state.
            if let Some(state_id) = current_states
                .iter()
                .find(|&state| nfa_tokens.get(state).is_some())
            {
                dfa_tokens.insert(nfa_to_dfa_states_map[current_states], nfa_tokens[state_id]);
            }
        }

        // Find the backtacks associated to the DFA final states.
        let mut dfa_backtrack: HashSet<usize> = HashSet::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            if current_states
                .iter()
                .any(|state| nfa_backtrack.get(state).is_some() && nfa_backtrack.contains(&state))
            {
                dfa_backtrack.insert(nfa_to_dfa_states_map[current_states]);
            }
        }

        LexicalAnalyzerTable {
            dfa,
            tokens: dfa_tokens,
            backtrack: dfa_backtrack,
        }
    }
}

impl LexicalAnalyzerTable {
    /// Return the initial state of the DFA.
    pub fn get_initial_state(&self) -> usize {
        self.dfa.get_initial_state()
    }
    /// Return the next state based on a state and an input character.
    pub fn next(&self, state: usize, input: char) -> Result<usize, LexicalError> {
        match self.dfa.next(state, input) {
            Some(state) => Ok(state),
            None => Err(LexicalError::InvalidCharacter),
        }
    }
    /// Return, if possible, the final state of the current state.
    pub fn abort(&self, state: usize) -> Result<usize, LexicalError> {
        // Try to reach a final state by using the '\n' input.
        let new_state = self.dfa.next(state, '\n').unwrap();
        // This will work for all tokens except for the BlockComment.
        // If a final state was not reached, return LexicalError::UnterminatedBlockComment.
        if self.is_final_state(new_state) {
            Ok(new_state)
        } else {
            Err(LexicalError::UnterminatedBlockComment)
        }
    }
    /// Return if a state is final.
    pub fn is_final_state(&self, state: usize) -> bool {
        self.dfa.is_final_state(state)
    }
    /// Return the Token associated to the finale state.
    pub fn get_token_type(&self, state: usize) -> Option<TokenType> {
        self.tokens.get(&state).cloned()
    }
    /// Return the Backtrack associated to the finale state.
    pub fn is_backtrack_state(&self, state: usize) -> bool {
        self.backtrack.contains(&state)
    }
}
