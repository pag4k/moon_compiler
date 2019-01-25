use super::finite_accepter::*;
use super::language::*;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use std::collections::HashMap;
use std::collections::HashSet;

use std::ops::RangeInclusive;

pub struct DotGraph {
    code: String,
}

impl DotGraph {
    pub fn generate(
        nfa: &NonDeterministicFiniteAccepter,
        transtions: &HashMap<(usize, char), usize>,
        tokens: &HashMap<usize, TokenType>,
        backtrack: &HashSet<usize>,
        states: RangeInclusive<usize>,
        filename: &str,
    ) {
        let mut dot_graph = DotGraph {
            code: String::new(),
        };
        dot_graph.add_line("digraph finite_state_machine {");
        dot_graph.add_line("\trankdir=LR;");
        dot_graph.add_line("\tsize=\"8,5\"");

        dot_graph.add_line("");

        for state in states
            .clone()
            .rev()
            .filter(|state| nfa.final_states.contains(&state))
        {
            let line = format!(
                "\tnode [shape = rectangle, label=\"{} -> {}\", fontsize=12] token{};",
                state, tokens[&state], state
            );
            dot_graph.add_line(&line);
        }

        dot_graph.add_line("");

        for state in states.clone() {
            let node = if nfa.final_states.contains(&state) {
                "doublecircle"
            } else {
                "circle"
            };
            let color = if backtrack.contains(&state) {
                "red"
            } else {
                "black"
            };
            let line = format!(
                "\tnode [shape = {}, label=\"{}\", fontsize=12, color={}] {};",
                node, state, color, state
            );
            dot_graph.add_line(&line);
        }

        dot_graph.add_line("");

        let line = "\tnode [shape = point, color=black] q0;";
        dot_graph.add_line(&line);
        let line = format!("\tq0\t->\t{};", states.start());
        dot_graph.add_line(&line);

        dot_graph.add_line("");

        for (from, to) in transtions
            .iter()
            .filter(|(from, _)| *states.start() <= from.0 && from.0 <= *states.end())
        {
            let line = format!("\t{}\t->\t{}\t[ label = \"{}\" ];", from.0, to, from.1);
            dot_graph.add_line(&line);
        }
        dot_graph.add_line("}");

        let path = Path::new(filename);

        let mut file = File::create(&path).unwrap();

        file.write_all(dot_graph.code.as_bytes()).unwrap();
    }
    fn add_line(&mut self, line: &str) {
        self.code.push_str(line);
        self.code.push('\n');
    }
}
