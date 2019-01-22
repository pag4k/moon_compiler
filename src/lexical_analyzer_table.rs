use super::finite_accepter::*;
use super::language::*;
use super::nfa_generator::*;

use std::collections::HashMap;
use std::collections::HashSet;

pub struct LexicalAnalyzerTable {
    dfa: DeterministicFiniteAccepter,
    tokens: HashMap<usize, TokenType>,
    backtrack: HashSet<usize>,
}

impl Default for LexicalAnalyzerTable {
    fn default() -> Self {
        let (nfa, nfa_tokens, nfa_backtrack) = define_nfa_table();

        let (dfa, nfa_to_dfa_states_map) = DeterministicFiniteAccepter::from_nfa(nfa, &SIGMA);

        let mut dfa_tokens: HashMap<usize, TokenType> = HashMap::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            //Assuming there is only one final NFA state.
            if let Some(state_id) = current_states
                .iter()
                .find(|&state| nfa_tokens.get(state).is_some())
            {
                dfa_tokens.insert(nfa_to_dfa_states_map[current_states], nfa_tokens[state_id]);
                // if let Some(token_type) = nfa_tokens.get(state_id) {
                //     assert!(nfa_tokens.get(state_id).is_some());
                // }
            }
        }

        let mut dfa_backtrack: HashSet<usize> = HashSet::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            if current_states
                .iter()
                .any(|state| nfa_backtrack.get(state).is_some() && nfa_backtrack.contains(&state))
            {
                dfa_backtrack.insert(nfa_to_dfa_states_map[current_states]);
            }
        }

        //println!("MAP {:?}", nfa_to_dfa_states_map);
        //println!("FINAL {:?}", final_states);
        //println!("BACKTRACK {:?}", backtrack);

        LexicalAnalyzerTable {
            dfa,
            tokens: dfa_tokens,
            backtrack: dfa_backtrack,
        }
    }
}

impl LexicalAnalyzerTable {
    pub fn get_initial_state(&self) -> usize {
        self.dfa.get_initial_state()
    }
    pub fn next(&self, state: usize, input: char) -> Result<usize, LexicalError> {
        match self.dfa.next(state, input) {
            Some(state) => Ok(state),
            None => Err(LexicalError::InvalidCharacter),
        }
    }
    //FIXME: This method is a bad way to force the DFA to give me the final state of its current branch.
    //It is somplicated because of the block comment which cannot be terminted with a simple \n.
    pub fn abort(&self, state: usize) -> Result<usize, LexicalError> {
        let new_state = self.dfa.next(state, '\n').unwrap();
        if self.is_final_state(new_state) {
            Ok(new_state)
        } else {
            Err(LexicalError::UnterminatedBlockComment)
        }
    }
    pub fn is_final_state(&self, state: usize) -> bool {
        self.dfa.is_final_state(state)
    }

    pub fn get_token_type(&self, state: usize) -> Option<TokenType> {
        self.tokens.get(&state).cloned()
    }
    pub fn is_backtrack_state(&self, state: usize) -> bool {
        self.backtrack.contains(&state)
    }
}
