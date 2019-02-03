use super::grammar::*;
use std::collections::{HashMap, HashSet};

use std::fmt::Debug;
use std::hash::Hash;

pub struct SyntacticAnalyzerTable<V, T> {
    grammar: ContextFreeGrammar<V, T>,
    table: HashMap<(V, Option<T>), usize>,
}

impl<V, T> SyntacticAnalyzerTable<V, T> {
    pub fn new(grammar: ContextFreeGrammar<V, T>) -> Self
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        SyntacticAnalyzerTable {
            table: grammar.get_table(),
            grammar,
        }
    }
    pub fn get(&self, variable: V, input: Option<T>) -> Option<Production<V, T>>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        self.table
            .get(&(variable, input))
            .map(|production_id| self.grammar.productions[production_id.clone()].clone())
    }

    pub fn get_start(&self) -> V
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        self.grammar.start
    }
}
