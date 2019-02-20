use super::grammar::*;
use std::collections::HashMap;

use std::fmt::Debug;
use std::hash::Hash;

pub struct SyntacticAnalyzerTable<V, T, A> {
    grammar: ContextFreeGrammar<V, T, A>,
    pub first_sets: HashMap<V, FirstSet<T>>,
    pub follow_sets: HashMap<V, FollowSet<T>>,
    table: ParserTable<V, T>,
}

impl<V, T, A> SyntacticAnalyzerTable<V, T, A>
where
    V: Debug + Eq + Hash + Copy,
    T: Debug + Eq + Hash + Copy,
    A: Debug + Eq + Hash + Copy,
{
    /// Create a SyntacticAnalyzerTable from a ContextFreeGrammar.
    pub fn from_grammar(grammar: ContextFreeGrammar<V, T, A>) -> Self {
        let first_sets = grammar.get_first_sets();
        let follow_sets = grammar.get_follow_sets(&first_sets);
        let table = grammar.get_table(&first_sets, &follow_sets);
        SyntacticAnalyzerTable {
            grammar,
            table,
            first_sets,
            follow_sets,
        }
    }

    //TODO: MAYBE RETURN A RESULT INSTEAD? I'LL SEE WITH ERROR HANDLING.
    /// Get the production needed to parse a Token based on a Variable.
    pub fn get(&self, variable: V, input: FollowType<T>) -> Option<Production<V, T, A>> {
        self.table
            .get(&(variable, input))
            .map(|production_id| self.grammar.productions[*production_id].clone())
    }

    pub fn get_start(&self) -> V {
        self.grammar.start
    }
}
