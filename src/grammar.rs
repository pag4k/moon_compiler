use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result};
use std::hash::Hash;
use std::str::FromStr;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Symbol<V, T> {
    NonTerminal(V),
    // Epsilon will be included as a None terminal.
    Terminal(Option<T>),
}

impl<V, T> Symbol<V, T> {
    fn is_variable(&self) -> bool {
        match self {
            Symbol::NonTerminal(_) => true,
            Symbol::Terminal(_) => false,
        }
    }
    fn is_terminal(&self) -> bool {
        match self {
            Symbol::NonTerminal(_) => false,
            Symbol::Terminal(_) => true,
        }
    }
}

impl<V: Debug, T: Debug> Debug for Symbol<V, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let output = match self {
            Symbol::NonTerminal(variable) => format!("{:?}", variable),
            Symbol::Terminal(terminal) => match terminal {
                Some(terminal) => format!("'{:?}'", terminal),
                None => "EPSILON".to_string(),
            },
        };
        write!(f, "{}", output)
    }
}

#[derive(Clone)]
pub struct Production<V, T> {
    pub lhs: V,
    pub rhs: Vec<Symbol<V, T>>,
}

impl<V: Debug, T: Debug> Debug for Production<V, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut output = format!("{:?} -> ", self.lhs);
        for symbol in &self.rhs {
            output.push_str(&format!("{:?} ", symbol));
        }
        output.pop();
        write!(f, "{}", output)
    }
}

// impl<V, T> Production<V, T> {
//     fn new(lhs: V, rhs: Vec<Symbol<V, T>>) -> Self {
//         Production { lhs, rhs }
//     }
// }

pub struct ContextFreeGrammar<V, T> {
    pub variables: HashSet<V>,
    pub terminals: HashSet<T>,
    pub start: V,
    pub productions: Vec<Production<V, T>>,
}

impl<V, T> ContextFreeGrammar<V, T> {
    #[allow(dead_code)]
    pub fn from_file(file_name: &str) -> Self
    where
        V: Debug + Eq + Hash + Copy + FromStr,
        T: Debug + Eq + Hash + Copy + FromStr,
    {
        let mut variables: HashSet<V> = HashSet::new();
        let mut terminals: HashSet<T> = HashSet::new();
        let mut productions: Vec<Production<V, T>> = Vec::new();

        use std::fs::File;
        use std::io::{BufRead, BufReader, Result};

        let file = File::open(file_name).unwrap();
        for line in BufReader::new(file).lines() {
            let line = line.unwrap();
            let words: Vec<&str> = line.split_whitespace().collect();
            let lhs = match V::from_str(words[0]) {
                Ok(lhs) => {
                    variables.insert(lhs);
                    lhs
                }
                Err(_) => {
                    println!("Error: Cannot find non-terminal: {}", words[0]);
                    unreachable!();
                }
            };
            let rhs: Vec<Symbol<V, T>> = words
                .iter()
                .skip(2)
                .map(|symbol| {
                    if symbol.eq_ignore_ascii_case("EPSILON") {
                        Symbol::Terminal(None)
                    } else if let Ok(non_terminal) = V::from_str(symbol) {
                        variables.insert(non_terminal);
                        Symbol::NonTerminal(non_terminal)
                    } else if let Ok(terminal) = T::from_str(symbol) {
                        terminals.insert(terminal);
                        Symbol::Terminal(Some(terminal))
                    } else {
                        println!("Error: Cannot find symbol: {}", symbol);
                        unreachable!();
                    }
                })
                .collect();
            productions.push(Production { lhs, rhs });
        }

        ContextFreeGrammar {
            variables,
            terminals,
            start: productions[0].lhs,
            productions,
        }
    }

    #[allow(dead_code)]
    fn get_first(&self) -> HashMap<V, HashSet<Option<T>>>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        use Symbol::*;

        let mut first: HashMap<V, HashSet<Option<T>>> = HashMap::new();

        for variable in self.variables.iter().cloned() {
            first.insert(variable, HashSet::new());
        }

        let mut modified = true;
        while modified {
            modified = false;
            for production in self.productions.iter() {
                let mut set: HashSet<Option<T>> = HashSet::new();
                let mut add_epsilon = true;
                for symbol in production.rhs.iter() {
                    match symbol {
                        Terminal(terminal) => {
                            set.insert(*terminal);
                            add_epsilon = false;
                            break;
                        }
                        NonTerminal(variable) => {
                            if first[variable].contains(&None) {
                                let mut without_epsilon = first[variable].clone();
                                without_epsilon.remove(&None);
                                set = set.union(&without_epsilon).cloned().collect();
                            } else {
                                set = set.union(&first[variable]).cloned().collect();
                                add_epsilon = false;
                                break;
                            }
                        }
                    }
                }
                if add_epsilon {
                    set.insert(None);
                }
                if !first[&production.lhs].is_superset(&set) {
                    first.insert(
                        production.lhs,
                        first[&production.lhs].union(&set).cloned().collect(),
                    );
                    modified = true;
                }
            }
        }

        first
    }

    #[allow(dead_code)]
    fn get_follow(&self, first: &HashMap<V, HashSet<Option<T>>>) -> HashMap<V, HashSet<Option<T>>>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        use Symbol::*;
        let mut follow: HashMap<V, HashSet<Option<T>>> = HashMap::new();

        for variable in self.variables.iter().cloned() {
            if variable == self.start {
                // FIXME: None is used for $. I should find a better way.
                let mut set: HashSet<Option<T>> = HashSet::new();
                set.insert(None);
                follow.insert(self.start, set);
            } else {
                follow.insert(variable, HashSet::new());
            }
        }

        let mut modified = true;
        while modified {
            modified = false;
            //Step 2
            for production in self.productions.iter() {
                let mut iterator = production.rhs.iter().peekable();
                while iterator.peek().is_some() {
                    if let NonTerminal(first_variable) = iterator.next().unwrap() {
                        let second_variable = match iterator.peek() {
                            None => break,
                            Some(symbol) => symbol,
                        };
                        let mut set: HashSet<Option<T>> = HashSet::new();
                        match second_variable {
                            NonTerminal(second_variable) => {
                                if first[second_variable].contains(&None) {
                                    let mut without_epsilon = first[second_variable].clone();
                                    without_epsilon.remove(&None);
                                    set = set.union(&without_epsilon).cloned().collect();
                                } else {
                                    set = set.union(&first[second_variable]).cloned().collect();
                                }
                            }
                            Terminal(terminal) => {
                                assert!(terminal.is_some());
                                set.insert(*terminal);
                            }
                        }
                        if !follow[first_variable].is_superset(&set) {
                            follow.insert(
                                *first_variable,
                                follow[first_variable].union(&set).cloned().collect(),
                            );
                            modified = true;
                        }
                    };
                }
            }

            //Step 3
            for production in self.productions.iter() {
                let mut iterator = production.rhs.iter().peekable();

                while iterator.peek().is_some() {
                    if let NonTerminal(first_variable) = iterator.next().unwrap() {
                        let second_symbol = iterator.peek();
                        let mut set: HashSet<Option<T>> = HashSet::new();
                        match second_symbol {
                            Some(second_symbol) => match second_symbol {
                                NonTerminal(second_variable) => {
                                    if first[second_variable].contains(&None) {
                                        set =
                                            set.union(&follow[&production.lhs]).cloned().collect();
                                    }
                                }
                                Terminal(terminal) => {
                                    assert!(terminal.is_some());
                                }
                            },
                            //Case: first_variable is the last symbol
                            None => {
                                set = set.union(&follow[&production.lhs]).cloned().collect();
                            }
                        }
                        if !follow[first_variable].is_superset(&set) {
                            follow.insert(
                                *first_variable,
                                follow[first_variable].union(&set).cloned().collect(),
                            );
                            modified = true;
                        }
                    };
                }
            }
        }

        follow
    }

    #[allow(dead_code)]
    pub fn get_table(&self) -> HashMap<(V, Option<T>), usize>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        use Symbol::*;

        let first = self.get_first();
        //dbg!(&first);
        let follow = self.get_follow(&first);
        //dbg!(&follow);

        let mut table: HashMap<(V, Option<T>), usize> = HashMap::new();
        let mut collisions = 0;

        //let mut production_number = 0;
        for (production_number, production) in self.productions.iter().enumerate() {
            assert!(!production.rhs.is_empty());
            match production.rhs[0] {
                Terminal(terminal) => match terminal {
                    Some(terminal) => {
                        //println!("Adding Terminal Some.");
                        if self.add_cell(
                            &mut table,
                            production.lhs,
                            Some(terminal),
                            production_number,
                        ) {
                            collisions += 1;
                        }
                        // assert!(table
                        //     .insert((production.lhs, Some(terminal)), production_number)
                        //     .is_none());
                    }
                    None => {
                        //println!("Adding Terminal None.");
                        for terminal in follow[&production.lhs].iter() {
                            if self.add_cell(
                                &mut table,
                                production.lhs,
                                *terminal,
                                production_number,
                            ) {
                                collisions += 1;
                            }
                            // assert!(table
                            //     .insert((production.lhs, *terminal), production_number)
                            //     .is_none());
                        }
                    }
                },
                NonTerminal(variable) => {
                    let mut set: HashSet<Option<T>> = HashSet::new();
                    let mut add_epsilon = true;
                    for symbol in production.rhs.iter() {
                        match symbol {
                            Terminal(terminal) => {
                                set.insert(*terminal);
                                add_epsilon = false;
                                break;
                            }
                            NonTerminal(variable) => {
                                if first[variable].contains(&None) {
                                    let mut without_epsilon = first[variable].clone();
                                    without_epsilon.remove(&None);
                                    set = set.union(&without_epsilon).cloned().collect();
                                } else {
                                    set = set.union(&first[variable]).cloned().collect();
                                    add_epsilon = false;
                                    break;
                                }
                            }
                        }
                    }
                    if add_epsilon {
                        set.insert(None);
                    }

                    if set.contains(&None) {
                        //println!("Adding NonTerminal with None.");
                        for terminal in follow[&production.lhs].iter() {
                            if self.add_cell(
                                &mut table,
                                production.lhs,
                                *terminal,
                                production_number,
                            ) {
                                collisions += 1;
                            }
                            // assert!(table
                            //     .insert((production.lhs, *terminal), production_number)
                            //     .is_none());
                        }
                    }
                    //println!("Adding NonTerminal without None.");
                    for terminal in set.iter() {
                        if self.add_cell(&mut table, production.lhs, *terminal, production_number) {
                            collisions += 1;
                        }
                        //     assert!(table
                        //         .insert((production.lhs, *terminal), production_number)
                        //         .is_none());
                    }
                }
            }
        }
        println!("Total number of collisions: {}.", collisions);
        if collisions > 0 {
            panic!();
        }
        table
    }

    fn add_cell(
        &self,
        table: &mut HashMap<(V, Option<T>), usize>,
        variable: V,
        terminal: Option<T>,
        new_production_number: usize,
    ) -> bool
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        if let Some(old_production_number) =
            table.insert((variable, terminal), new_production_number)
        {
            println!(
                "Collison at ({:?}, {:?}) between:\nOld: {:?}\nNew: {:?}",
                variable,
                terminal,
                self.productions[old_production_number],
                self.productions[new_production_number]
            );
            return true;
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    // #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    // enum TerminalEnum {
    //     Id,
    //     Plus,
    //     Minus,
    //     Multiplication,
    //     Division,
    //     LeftParenthesis,
    //     RightParenthesis,
    //     Zero,
    //     One,
    // }
    //
    // #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    // enum VariableEnum1 {
    //     Expression,
    // }
    //
    // #[test]
    // fn bad_grammar() {
    //     use Symbol::*;
    //     use TerminalEnum::*;
    //     use VariableEnum1::*;
    //     let mut variables: HashSet<VariableEnum1> = HashSet::new();
    //     variables.insert(Expression);
    //     let mut terminals: HashSet<TerminalEnum> = HashSet::new();
    //     terminals.insert(Id);
    //     terminals.insert(Plus);
    //     terminals.insert(Minus);
    //     terminals.insert(Multiplication);
    //     terminals.insert(Division);
    //     terminals.insert(LeftParenthesis);
    //     terminals.insert(RightParenthesis);
    //     let start = Expression;
    //     let mut productions: Vec<Production<VariableEnum1, TerminalEnum>> = Vec::new();
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![
    //             NonTerminal(Expression),
    //             Terminal(Some(Plus)),
    //             NonTerminal(Expression),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![
    //             NonTerminal(Expression),
    //             Terminal(Some(Minus)),
    //             NonTerminal(Expression),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![
    //             NonTerminal(Expression),
    //             Terminal(Some(Multiplication)),
    //             NonTerminal(Expression),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![
    //             NonTerminal(Expression),
    //             Terminal(Some(Division)),
    //             NonTerminal(Expression),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![
    //             Terminal(Some(LeftParenthesis)),
    //             NonTerminal(Expression),
    //             Terminal(Some(RightParenthesis)),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: Expression,
    //         rhs: vec![Terminal(Some(Id))],
    //     });
    //
    //     dbg!(&productions);
    //
    //     let grammar = ContextFreeGrammar {
    //         variables,
    //         terminals,
    //         start,
    //         productions,
    //     };
    //
    //     let first = grammar.get_first();
    //     //dbg!(&first);
    //     let follow = grammar.get_follow(&first);
    //     //dbg!(&follow);
    //     let table = grammar.get_table(&first, &follow);
    //     //dbg!(&table);
    //     assert!(first.is_empty());
    // }
    //
    // #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    // enum VariableEnum2 {
    //     E,
    //     EPrime,
    //     T,
    //     TPrime,
    //     F,
    // }
    //
    // #[test]
    // fn good_grammar() {
    //     use Symbol::*;
    //     use TerminalEnum::*;
    //     use VariableEnum2::*;
    //     let mut variables: HashSet<VariableEnum2> = HashSet::new();
    //     variables.insert(E);
    //     variables.insert(EPrime);
    //     variables.insert(T);
    //     variables.insert(TPrime);
    //     variables.insert(F);
    //     let mut terminals: HashSet<TerminalEnum> = HashSet::new();
    //     terminals.insert(Id);
    //     terminals.insert(Plus);
    //     terminals.insert(Minus);
    //     terminals.insert(Multiplication);
    //     terminals.insert(Division);
    //     terminals.insert(LeftParenthesis);
    //     terminals.insert(RightParenthesis);
    //     let start = E;
    //     let mut productions: Vec<Production<VariableEnum2, TerminalEnum>> = Vec::new();
    //     productions.push(Production {
    //         lhs: E,
    //         rhs: vec![NonTerminal(T), NonTerminal(EPrime)],
    //     });
    //     productions.push(Production {
    //         lhs: EPrime,
    //         rhs: vec![Terminal(None)],
    //     });
    //     productions.push(Production {
    //         lhs: EPrime,
    //         rhs: vec![Terminal(Some(Plus)), NonTerminal(T), NonTerminal(EPrime)],
    //     });
    //     productions.push(Production {
    //         lhs: T,
    //         rhs: vec![NonTerminal(F), NonTerminal(TPrime)],
    //     });
    //     productions.push(Production {
    //         lhs: TPrime,
    //         rhs: vec![Terminal(None)],
    //     });
    //     productions.push(Production {
    //         lhs: TPrime,
    //         rhs: vec![
    //             Terminal(Some(Multiplication)),
    //             NonTerminal(F),
    //             NonTerminal(TPrime),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: F,
    //         rhs: vec![
    //             Terminal(Some(LeftParenthesis)),
    //             NonTerminal(E),
    //             Terminal(Some(RightParenthesis)),
    //         ],
    //     });
    //     productions.push(Production {
    //         lhs: F,
    //         rhs: vec![Terminal(Some(Zero))],
    //     });
    //     productions.push(Production {
    //         lhs: F,
    //         rhs: vec![Terminal(Some(One))],
    //     });
    //
    //     dbg!(&productions);
    //
    //     let grammar = ContextFreeGrammar {
    //         variables,
    //         terminals,
    //         start,
    //         productions,
    //     };
    //
    //     let first = grammar.get_first();
    //     //dbg!(&first);
    //     let follow = grammar.get_follow(&first);
    //     //dbg!(&follow);
    //     let table = grammar.get_table(&first, &follow);
    //     for (key, value) in table.iter() {
    //         println!("({:?}): {:?}", key, grammar.productions[*value]);
    //     }
    //
    //     //dbg!(&table);
    //     assert!(first.is_empty());
    // }

    #[test]
    fn file_grammar() {
        use super::super::grammar_generator::*;
        use super::super::language::*;

        let grammar: ContextFreeGrammar<VariableType, TokenType> =
            ContextFreeGrammar::from_file("test.txt");

        dbg!(&grammar.productions);

        // let first = grammar.get_first();
        // //dbg!(&first);
        // let follow = grammar.get_follow(&first);
        // //dbg!(&follow);
        let table = grammar.get_table();
        // for (key, value) in table.iter() {
        //     println!("({:?}): {:?}", key, grammar.productions[*value]);
        // }

        //dbg!(&table);
        assert!(table.is_empty());
    }
}
