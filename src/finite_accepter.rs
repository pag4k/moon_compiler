use std::collections::HashMap;
use std::collections::HashSet;

pub struct NonDeterministicFiniteAccepter {
    pub states: HashSet<usize>,
    pub alphabet: HashSet<char>,
    pub function: HashMap<(usize, char), Vec<usize>>,
    pub epsilon_closure: HashMap<usize, Vec<usize>>,
    pub initial_state: usize,
    pub final_states: HashSet<usize>,
}

#[allow(dead_code)]
pub struct DeterministicFiniteAccepter {
    states: HashSet<usize>,
    alphabet: HashSet<char>,
    function: HashMap<(usize, char), usize>,
    initial_state: usize,
    final_states: HashSet<usize>,
}

impl DeterministicFiniteAccepter {
    pub fn get_initial_state(&self) -> usize {
        self.initial_state
    }
    pub fn next(&self, state: usize, input: char) -> Option<usize> {
        //println!("INPUT: {:?}", input);
        self.function.get(&(state, input)).cloned()
    }
    pub fn is_final_state(&self, state: usize) -> bool {
        self.final_states.get(&state).is_some()
    }

    pub fn from_nfa(
        nfa: NonDeterministicFiniteAccepter,
        alphabet: &[char],
    ) -> (Self, HashMap<Vec<usize>, usize>) {
        let mut nfa_to_dfa_states_map: HashMap<Vec<usize>, usize> = HashMap::new();
        let mut marked_states: HashMap<usize, bool> = HashMap::new();
        let mut function: HashMap<(usize, char), usize> = HashMap::new();

        //Initial state: 0
        let mut initial_states = nfa.epsilon_closure[&nfa.initial_state].clone();
        initial_states.sort_unstable();
        initial_states.dedup();
        nfa_to_dfa_states_map.insert(initial_states.clone(), 0);
        marked_states.insert(0, false);

        while let Some(unmarked_state) = nfa_to_dfa_states_map
            .clone()
            .into_iter()
            .find(|(_, state)| !marked_states[state])
        {
            let (unmarked_states_nfa, unmarked_state_dfa) = unmarked_state;
            //println!("Unmarker state: {:?}", unmarked_state);

            marked_states.insert(unmarked_state_dfa, true);
            //I will ignore the part with epsilon closure for now since it is just used for the initial state.
            //Not very efficient because it will try to add multiple times all symbol in a group.
            for &current_symbol in alphabet.iter() {
                let mut states: Vec<usize> = unmarked_states_nfa
                    .iter()
                    .filter_map(|&state| nfa.function.get(&(state, current_symbol)))
                    .flatten()
                    .cloned()
                    .collect();
                states.sort_unstable();
                states.dedup();
                //println!("Current state: {:?}", states);
                let next_id = if let Some(next_id) = nfa_to_dfa_states_map.get(&states) {
                    *next_id
                } else {
                    let next_id = nfa_to_dfa_states_map.len();
                    //println!("New state: {:?}", next_id);
                    nfa_to_dfa_states_map.insert(states, next_id);
                    marked_states.insert(next_id, false);
                    //final_states.insert(next_id, false);
                    next_id
                };
                function.insert((unmarked_state_dfa, current_symbol), next_id);
            }
        }

        let mut final_states: HashSet<usize> = HashSet::new();
        for current_states in nfa_to_dfa_states_map.keys() {
            //Assuming there is only one final NFA state.
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
