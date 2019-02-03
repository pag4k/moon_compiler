use super::grammar::*;
use super::lexical_analyzer::*;
use super::syntactic_analyzer_table::*;

use std::fmt::Debug;
use std::hash::Hash;

pub struct SyntacticAnalyzer<V, T> {
    pub tokens: Vec<T>,
    pub table: SyntacticAnalyzerTable<V, T>,
}

impl<V, T> SyntacticAnalyzer<V, T> {
    pub fn parse(&self)
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        use Symbol::*;
        // Get a reverse iterator over the token stream.
        let mut token_iter = self.tokens.iter();
        // Initialize the stack with '$' (Option<Terminal>: None) and the start non-terminal.
        let mut stack = vec![Terminal(None), NonTerminal(self.table.get_start())];
        let mut token = token_iter.next().cloned();
        while let Some(symbol) = stack.last() {
            match symbol {
                Terminal(terminal) => {
                    if *terminal == token {
                        stack.pop();
                        token = token_iter.next().cloned();
                    } else {
                        println!(
                            "Terminals on stack ({:?}) and token ({:?}) do not match.",
                            terminal, token
                        );
                        println!("Stack: {:?}.", stack);
                        panic!();
                    }
                }
                NonTerminal(non_terminal) => match self.table.get(*non_terminal, token) {
                    Some(production) => {
                        stack.pop();
                        if production.rhs[0] == Terminal(None) {
                            continue;
                        }
                        for symbol in production.rhs.iter().rev() {
                            stack.push(*symbol);
                        }
                    }
                    None => {
                        println!("Not in table ({:?}, {:?}).", non_terminal, token);
                        println!("Stack: {:?}.", stack);
                        panic!();
                    }
                },
            }
        }
        if token.is_some() {
            println!("Last token is not '$': {:?}", token);
            println!("Stack: {:?}.", stack);
            panic!();
        }
        println!("Parse completed succesfully!");
        println!("Tokens: {:?}", token_iter.next());
        println!("Stack: {:?}.", stack);
    }
}
