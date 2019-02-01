use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

trait HasEpsilon<T> {
    fn epsilon() -> T;
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Symbol<V, T>
where
    T: HasEpsilon<T>,
{
    Variable(V),
    Terminal(T),
}

impl<V, T> Symbol<V, T>
where
    T: HasEpsilon<T>,
{
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

// #[derive(PartialEq, Eq, Hash, Clone)]
// struct ContextFreeProduction<V, T>
// where
//     T: HasEpsilon<T>,
// {
//     lhs: V,
//     rhs: Vec<Symbol<V, T>>,
// }

struct ContextFreeGrammar<V, T>
where
    T: HasEpsilon<T>,
{
    variables: HashSet<V>,
    terminals: HashSet<T>,
    start: V,
    productions: HashMap<V, Vec<Vec<Symbol<V, T>>>>,
}

impl<V, T> ContextFreeGrammar<V, T>
where
    T: HasEpsilon<T>,
{
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
    fn get_first(&self) -> HashMap<V, HashSet<T>>
    where
        V: Debug + Eq + Hash + Copy,
        T: Debug + Eq + Hash + Copy + HasEpsilon<T>,
    {
        let mut first: HashMap<V, HashSet<T>> = HashMap::new();

        for variable in self.variables.iter().cloned() {
            first.insert(variable, HashSet::new());
        }

        let mut modified = true;
        while (modified) {
            modified = false;
            for (current_variable, productions) in self.productions.iter() {
                for production in productions.iter() {
                    let mut set: HashSet<T> = HashSet::new();
                    let mut add_epsilon = true;
                    for symbol in production.iter() {
                        match symbol {
                            Symbol::Terminal(terminal) => {
                                assert!(*terminal != T::epsilon());
                                set.insert(*terminal);
                                add_epsilon = false;
                                break;
                            }
                            Symbol::Variable(variable) => {
                                // if first.get(variable).unwrap().is_empty() {
                                //     add_epsilon = false;
                                //     break;
                                // }
                                if first.get(variable).unwrap().contains(&T::epsilon()) {
                                    let mut without_epsilon = first.get(variable).unwrap().clone();
                                    without_epsilon.remove(&T::epsilon());
                                    set = set.union(&without_epsilon).cloned().collect();
                                } else {
                                    set =
                                        set.union(first.get(variable).unwrap()).cloned().collect();
                                    add_epsilon = false;
                                    break;
                                }
                            }
                        }
                    }
                    if add_epsilon {
                        set.insert(T::epsilon());
                    }
                    if !first.get(current_variable).unwrap().is_superset(&set) {
                        modified = true;
                        first.insert(
                            *current_variable,
                            first
                                .get(current_variable)
                                .unwrap()
                                .union(&set)
                                .cloned()
                                .collect(),
                        );
                        modified = true;
                    }
                }
            }
        }

        first
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
        Epsilon,
    }

    impl HasEpsilon<TerminalEnum> for TerminalEnum {
        fn epsilon() -> TerminalEnum {
            TerminalEnum::Epsilon
        }
    }

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum VariableEnum {
        Expression,
    }

    #[test]
    fn bad_grammar() {
        use Symbol::*;
        use TerminalEnum::*;
        use VariableEnum::*;
        let mut variables: HashSet<VariableEnum> = HashSet::new();
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
        let mut productions: HashMap<VariableEnum, Vec<Vec<Symbol<VariableEnum, TerminalEnum>>>> =
            HashMap::new();
        productions.insert(
            Expression,
            vec![
                vec![Variable(Expression), Terminal(Plus), Variable(Expression)],
                vec![Variable(Expression), Terminal(Minus), Variable(Expression)],
                vec![
                    Variable(Expression),
                    Terminal(Multiplication),
                    Variable(Expression),
                ],
                vec![
                    Variable(Expression),
                    Terminal(Division),
                    Variable(Expression),
                ],
                vec![
                    Terminal(LeftParenthesis),
                    Variable(Expression),
                    Terminal(RightParenthesis),
                ],
                vec![Terminal(Id)],
            ],
        );

        dbg!(&productions);

        let grammar = ContextFreeGrammar {
            variables,
            terminals,
            start,
            productions,
        };

        let first = grammar.get_first();
        dbg!(&first);

        assert!(first.is_empty());
    }
}
