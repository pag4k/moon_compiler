use std::collections::{HashMap, HashSet};

/// NonDeterministicFiniteAccepter ADT
pub struct NonDeterministicFiniteAccepter {
    pub states: HashSet<usize>,
    pub alphabet: HashSet<char>,
    pub function: HashMap<(usize, char), Vec<usize>>,
    pub epsilon_closure: HashMap<usize, Vec<usize>>,
    pub initial_state: usize,
    pub final_states: HashSet<usize>,
}

/// DeterministicFiniteAccepter ADT
#[allow(dead_code)]
pub struct DeterministicFiniteAccepter {
    states: HashSet<usize>,
    alphabet: HashSet<char>,
    function: HashMap<(usize, char), usize>,
    initial_state: usize,
    final_states: HashSet<usize>,
}

impl DeterministicFiniteAccepter {
    /// Return the initial state of the DFA.
    pub fn get_initial_state(&self) -> usize {
        self.initial_state
    }
    /// Return the next state based on a state and an input character.
    pub fn next(&self, state: usize, input: char) -> Option<usize> {
        self.function.get(&(state, input)).cloned()
    }
    /// Return if a state is final.
    pub fn is_final_state(&self, state: usize) -> bool {
        self.final_states.get(&state).is_some()
    }
    /// Return a DFA from a NFA and map descripting the relation between the NFA and DFA states.
    ///
    /// # Remarks
    ///
    /// Based on the Rabin-Scott powerset construction algorithm.
    ///
    /// Since multiple NFA states correspond to a single DFA state, the NFA states will be sorted
    /// to make sure the comparisons are done properly.
    pub fn from_nfa(
        nfa: NonDeterministicFiniteAccepter,
        alphabet: &[char],
    ) -> (Self, HashMap<Vec<usize>, usize>) {
        let mut nfa_to_dfa_states_map: HashMap<Vec<usize>, usize> = HashMap::new();
        let mut marked_states: HashMap<usize, bool> = HashMap::new();
        let mut function: HashMap<(usize, char), usize> = HashMap::new();

        // Get the inital states of the nfa and set the corresponding DFA state to 0.
        let mut initial_states = nfa.epsilon_closure[&nfa.initial_state].clone();
        initial_states.sort_unstable();
        initial_states.dedup();
        nfa_to_dfa_states_map.insert(initial_states.clone(), 0);
        marked_states.insert(0, false);

        // Main loop of the algorithm. On each iteraton, an unmarked state is selected.
        while let Some(unmarked_state) = nfa_to_dfa_states_map
            .clone()
            .into_iter()
            .find(|(_, state)| !marked_states[state])
        {
            // Get and mark the selected state.
            let (unmarked_states_nfa, unmarked_state_dfa) = unmarked_state;
            marked_states.insert(unmarked_state_dfa, true);

            // TODO: The part of the algorithm about the epsilon closure is ignored because it is
            //       only used for the inital NFA state for now.

            // Iterator over all symbol in the alphabet.
            for &current_symbol in alphabet.iter() {
                // Get the resulting DFA state.
                let mut states: Vec<usize> = unmarked_states_nfa
                    .iter()
                    .filter_map(|&state| nfa.function.get(&(state, current_symbol)))
                    .flatten()
                    .cloned()
                    .collect();
                states.sort_unstable();
                states.dedup();

                // Check if the DFA state already exist.
                let next_id = match nfa_to_dfa_states_map.get(&states) {
                    // If so, return its id.
                    Some(next_id) => *next_id,
                    // If not, get a new id and add it.
                    None => {
                        let next_id = nfa_to_dfa_states_map.len();
                        nfa_to_dfa_states_map.insert(states, next_id);
                        marked_states.insert(next_id, false);
                        next_id
                    }
                };
                // Add to transition function.
                function.insert((unmarked_state_dfa, current_symbol), next_id);
            }
        }

        // Based on the NFA to DFA states map, identify the DFA final states.
        // It is assumed that there is only one NFA final state for each DFA state.
        let mut final_states: HashSet<usize> = HashSet::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            if current_states
                .iter()
                .filter(|&state| nfa.final_states.get(state).is_some())
                .count()
                > 1
            {
                unreachable!("A DFA state corresponds to more than one NFA final states.");
            }
            if let Some(state_id) = current_states
                .iter()
                .find(|&state| nfa.final_states.get(state).is_some())
            {
                if nfa.final_states.get(state_id).is_some() {
                    final_states.insert(nfa_to_dfa_states_map[current_states]);
                }
            }
        }

        (
            DeterministicFiniteAccepter {
                states: nfa_to_dfa_states_map.values().cloned().collect(),
                alphabet: HashSet::new(),
                function,
                initial_state: 0,
                final_states,
            },
            nfa_to_dfa_states_map,
        )
    }
}
