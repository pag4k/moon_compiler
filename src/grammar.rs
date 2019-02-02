use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result};
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Symbol<V, T> {
    Variable(V),
    // Epsilon will be included as a None terminal.
    Terminal(Option<T>),
}

impl<V, T> Symbol<V, T> {
    fn is_variable(&self) -> bool {
        match self {
            Symbol::Variable(_) => true,
            Symbol::Terminal(_) => false,
        }
    }
    fn is_terminal(&self) -> bool {
        match self {
            Symbol::Variable(_) => false,
            Symbol::Terminal(_) => true,
        }
    }
}

struct Production<V, T> {
    lhs: V,
    rhs: Vec<Symbol<V, T>>,
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

struct ContextFreeGrammar<V, T> {
    variables: HashSet<V>,
    terminals: HashSet<T>,
    start: V,
    productions: HashMap<V, Vec<Vec<Symbol<V, T>>>>,
}

impl<V, T> ContextFreeGrammar<V, T> {
    // fn generate(start: V) -> Self
    // where
    //     V: Eq + Hash,
    //     T: Eq + Hash,
    // {
    //     let mut variables: HashSet<V> = HashSet::new();
    //     let mut terminals: HashSet<T> = HashSet::new();
    //     //let mut vstart: V = ?;
    //     let mut productions: Vec<ContextFreeProduction<V, T>> = Vec::new();
    //
    //     ContextFreeGrammar {
    //         variables,
    //         terminals,
    //         start,
    //         productions,
    //     }
    // }

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
            for (current_variable, productions) in self.productions.iter() {
                for production in productions.iter() {
                    let mut set: HashSet<Option<T>> = HashSet::new();
                    let mut add_epsilon = true;
                    for symbol in production.iter() {
                        match symbol {
                            Terminal(terminal) => {
                                set.insert(*terminal);
                                add_epsilon = false;
                                break;
                            }
                            Variable(variable) => {
                                // if first.get(variable).unwrap().is_empty() {
                                //     add_epsilon = false;
                                //     break;
                                // }
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
                    if !first[current_variable].is_superset(&set) {
                        first.insert(
                            *current_variable,
                            first[current_variable].union(&set).cloned().collect(),
                        );
                        modified = true;
                    }
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
            for (_, productions) in self.productions.iter() {
                for production in productions.iter() {
                    let mut iterator = production.iter().peekable();

                    while iterator.peek().is_some() {
                        if let Variable(first_variable) = iterator.next().unwrap() {
                            let second_variable = match iterator.peek() {
                                None => break,
                                Some(symbol) => symbol,
                            };
                            let mut set: HashSet<Option<T>> = HashSet::new();
                            match second_variable {
                                Variable(second_variable) => {
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
            }

            //Step 3
            for (lhs, productions) in self.productions.iter() {
                for production in productions.iter() {
                    let mut iterator = production.iter().peekable();

                    while iterator.peek().is_some() {
                        if let Variable(first_variable) = iterator.next().unwrap() {
                            let second_symbol = iterator.peek();
                            let mut set: HashSet<Option<T>> = HashSet::new();
                            match second_symbol {
                                Some(second_symbol) => match second_symbol {
                                    Variable(second_variable) => {
                                        if first[second_variable].contains(&None) {
                                            set = set.union(&follow[lhs]).cloned().collect();
                                        }
                                    }
                                    Terminal(terminal) => {
                                        assert!(terminal.is_some());
                                    }
                                },
                                //Case: first_variable is the last symbol
                                None => {
                                    set = set.union(&follow[lhs]).cloned().collect();
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
        }

        follow
    }

    #[allow(dead_code)]
    fn get_table(
        &self,
        first: &HashMap<V, HashSet<Option<T>>>,
        follow: &HashMap<V, HashSet<Option<T>>>,
    ) -> HashMap<(V, Option<T>), usize>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy,
    {
        use Symbol::*;

        let mut table: HashMap<(V, Option<T>), usize> = HashMap::new();

        let mut production_number = 0;
        for (lhs, productions) in self.productions.iter() {
            for rhs in productions.iter() {
                assert!(!rhs.is_empty());
                match rhs[0] {
                    Terminal(terminal) => match terminal {
                        Some(terminal) => {
                            assert!(table
                                .insert((*lhs, Some(terminal)), production_number)
                                .is_none());
                        }
                        None => {
                            for terminal in follow[lhs].iter() {
                                assert!(table
                                    .insert((*lhs, *terminal), production_number)
                                    .is_none());
                            }
                        }
                    },
                    Variable(variable) => {
                        if first[&variable].contains(&None) {
                            for terminal in follow[lhs].iter() {
                                assert!(table
                                    .insert((*lhs, *terminal), production_number)
                                    .is_none());
                            }
                        } else {
                            for terminal in first[lhs].iter() {
                                assert!(table
                                    .insert((*lhs, *terminal), production_number)
                                    .is_none());
                            }
                        }
                    }
                }
                production_number += 1;
            }
        }

        table
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum TerminalEnum {
        Id,
        Plus,
        Minus,
        Multiplication,
        Division,
        LeftParenthesis,
        RightParenthesis,
        Zero,
        One,
    }

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum VariableEnum1 {
        Expression,
    }

    #[test]
    fn bad_grammar() {
        use Symbol::*;
        use TerminalEnum::*;
        use VariableEnum1::*;
        let mut variables: HashSet<VariableEnum1> = HashSet::new();
        variables.insert(Expression);
        let mut terminals: HashSet<TerminalEnum> = HashSet::new();
        terminals.insert(Id);
        terminals.insert(Plus);
        terminals.insert(Minus);
        terminals.insert(Multiplication);
        terminals.insert(Division);
        terminals.insert(LeftParenthesis);
        terminals.insert(RightParenthesis);
        let start = Expression;
        let mut productions: HashMap<VariableEnum1, Vec<Vec<Symbol<VariableEnum1, TerminalEnum>>>> =
            HashMap::new();
        productions.insert(
            Expression,
            vec![
                vec![
                    Variable(Expression),
                    Terminal(Some(Plus)),
                    Variable(Expression),
                ],
                vec![
                    Variable(Expression),
                    Terminal(Some(Minus)),
                    Variable(Expression),
                ],
                vec![
                    Variable(Expression),
                    Terminal(Some(Multiplication)),
                    Variable(Expression),
                ],
                vec![
                    Variable(Expression),
                    Terminal(Some(Division)),
                    Variable(Expression),
                ],
                vec![
                    Terminal(Some(LeftParenthesis)),
                    Variable(Expression),
                    Terminal(Some(RightParenthesis)),
                ],
                vec![Terminal(Some(Id))],
            ],
        );

        //dbg!(&productions);

        let grammar = ContextFreeGrammar {
            variables,
            terminals,
            start,
            productions,
        };

        let first = grammar.get_first();
        dbg!(&first);
        let follow = grammar.get_follow(&first);
        dbg!(&follow);
        let table = grammar.get_table(&first, &follow);
        dbg!(&table);
        assert!(first.is_empty());
    }

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum VariableEnum2 {
        E,
        EPrime,
        T,
        TPrime,
        F,
    }

    #[test]
    fn good_grammar() {
        use Symbol::*;
        use TerminalEnum::*;
        use VariableEnum2::*;
        let mut variables: HashSet<VariableEnum2> = HashSet::new();
        variables.insert(E);
        variables.insert(EPrime);
        variables.insert(T);
        variables.insert(TPrime);
        variables.insert(F);
        let mut terminals: HashSet<TerminalEnum> = HashSet::new();
        terminals.insert(Id);
        terminals.insert(Plus);
        terminals.insert(Minus);
        terminals.insert(Multiplication);
        terminals.insert(Division);
        terminals.insert(LeftParenthesis);
        terminals.insert(RightParenthesis);
        let start = E;
        let mut productions: HashMap<VariableEnum2, Vec<Vec<Symbol<VariableEnum2, TerminalEnum>>>> =
            HashMap::new();
        productions.insert(E, vec![vec![Variable(T), Variable(EPrime)]]);
        productions.insert(
            EPrime,
            vec![
                vec![Terminal(None)],
                vec![Terminal(Some(Plus)), Variable(T), Variable(EPrime)],
            ],
        );
        productions.insert(T, vec![vec![Variable(F), Variable(TPrime)]]);
        productions.insert(
            TPrime,
            vec![
                vec![Terminal(None)],
                vec![
                    Terminal(Some(Multiplication)),
                    Variable(F),
                    Variable(TPrime),
                ],
            ],
        );
        productions.insert(
            F,
            vec![
                vec![
                    Terminal(Some(LeftParenthesis)),
                    Variable(E),
                    Terminal(Some(RightParenthesis)),
                ],
                vec![Terminal(Some(Zero))],
                vec![Terminal(Some(One))],
            ],
        );

        //dbg!(&productions);

        let grammar = ContextFreeGrammar {
            variables,
            terminals,
            start,
            productions,
        };

        let first = grammar.get_first();
        dbg!(&first);
        let follow = grammar.get_follow(&first);
        dbg!(&follow);
        let table = grammar.get_table(&first, &follow);
        dbg!(&table);
        assert!(first.is_empty());
    }
}
