use crate::grammar::*;

use std::collections::HashMap;

use std::fmt::Display;
use std::hash::Hash;

pub struct SyntacticAnalyzerTable<V, T, A> {
    grammar: ContextFreeGrammar<V, T, A>,
    pub first_sets: HashMap<V, FirstSet<T>>,
    pub follow_sets: HashMap<V, FollowSet<T>>,
    table: ParserTable<V, T>,
}

impl<V, T, A> SyntacticAnalyzerTable<V, T, A>
where
    V: Display + Eq + Hash + Copy,
    T: Display + Eq + Hash + Copy,
    A: Display + Eq + Hash + Copy,
{
    /// Create a SyntacticAnalyzerTable from a ContextFreeGrammar.
    pub fn from_grammar(grammar: ContextFreeGrammar<V, T, A>) -> Result<Self, GrammarError> {
        let first_sets = grammar.get_first_sets();

        let follow_sets = grammar.get_follow_sets(&first_sets);
        let table = grammar.get_table(&first_sets, &follow_sets)?;
        Ok(SyntacticAnalyzerTable {
            grammar,
            table,
            first_sets,
            follow_sets,
        })
    }

    /// Get the production needed to parse a Token based on a Variable.
    pub fn get(&self, variable: V, input: FollowType<T>) -> Option<Production<V, T, A>> {
        self.table
            .get(&(variable, input))
            .map(|production_id| self.grammar.productions[*production_id].clone())
    }

    pub fn get_start(&self) -> V {
        self.grammar.start
    }

    #[allow(dead_code)]
    pub fn first_sets_as_string(&self) -> String {
        let mut string = String::new();
        for (lhs, rhs) in self.first_sets.iter() {
            string.push_str(&format!("{}: {} ", lhs, '{'));
            for symbol in rhs.iter() {
                string.push_str(&format!(" {}", symbol));
            }
            string.push_str(" }\n");
        }
        string
    }

    #[allow(dead_code)]
    pub fn follow_sets_as_string(&self) -> String {
        let mut string = String::new();
        for (lhs, rhs) in self.follow_sets.iter() {
            string.push_str(&format!("{}: {}", lhs, '{'));
            for symbol in rhs.iter() {
                string.push_str(&format!(" {}", symbol));
            }
            string.push_str(" }\n");
        }
        string
    }
}
