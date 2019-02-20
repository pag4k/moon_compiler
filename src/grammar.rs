use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result};
use std::hash::Hash;
use std::str::FromStr;

pub type FirstSet<T> = HashSet<FirstType<T>>;
pub type FollowSet<T> = HashSet<FollowType<T>>;
pub type ParserTable<V, T> = HashMap<(V, FollowType<T>), usize>;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum GrammarSymbol<V, T, A> {
    Variable(V),
    Terminal(T),
    SemanticAction(A),
    Epsilon,
}

impl<V: Debug, T: Debug, A: Debug> Debug for GrammarSymbol<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use GrammarSymbol::*;
        let output = match self {
            Variable(variable) => format!("{:?}", variable),
            Terminal(terminal) => format!("'{:?}'", terminal),
            SemanticAction(semantic_action) => format!("'{:?}'", semantic_action),
            Epsilon => "EPSILON".to_string(),
        };
        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum FirstType<T> {
    Terminal(T),
    Epsilon,
}

impl<T: Debug> Debug for FirstType<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use FirstType::*;
        let output = match self {
            Terminal(terminal) => format!("'{:?}'", terminal),
            Epsilon => "EPSILON".to_string(),
        };
        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum ParserSymbol<V, T, A> {
    Variable(V),
    Terminal(T),
    SemanticAction(A),
    DollarSign,
}

impl<V: Debug, T: Debug, A: Debug> Debug for ParserSymbol<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use ParserSymbol::*;
        let output = match self {
            Variable(variable) => format!("{:?}", variable),
            Terminal(terminal) => format!("'{:?}'", terminal),
            SemanticAction(semantic_action) => format!("'{:?}'", semantic_action),
            DollarSign => "$".to_string(),
        };
        write!(f, "{}", output)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum FollowType<T> {
    Terminal(T),
    DollarSign,
}

impl<T: Debug> Debug for FollowType<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use FollowType::*;
        let output = match self {
            //Variable(variable) => format!("{:?}", variable),
            Terminal(terminal) => format!("'{:?}'", terminal),
            DollarSign => "$".to_string(),
        };
        write!(f, "{}", output)
    }
}

impl<T> From<FirstType<T>> for FollowType<T> {
    fn from(grammar_symbol: FirstType<T>) -> Self {
        match grammar_symbol {
            //FirstType::Variable(variable) => FollowType::Variable(variable),
            FirstType::Terminal(terminal) => FollowType::Terminal(terminal),
            FirstType::Epsilon => unreachable!(),
        }
    }
}

impl<V, T, A> From<GrammarSymbol<V, T, A>> for ParserSymbol<V, T, A> {
    fn from(grammar_symbol: GrammarSymbol<V, T, A>) -> Self {
        match grammar_symbol {
            GrammarSymbol::Variable(variable) => ParserSymbol::Variable(variable),
            GrammarSymbol::Terminal(terminal) => ParserSymbol::Terminal(terminal),
            GrammarSymbol::SemanticAction(semantic_action) => {
                ParserSymbol::SemanticAction(semantic_action)
            }
            GrammarSymbol::Epsilon => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct Production<V, T, A> {
    pub lhs: V,
    pub rhs: Vec<GrammarSymbol<V, T, A>>,
}

impl<V: Debug, T: Debug, A: Debug> Debug for Production<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut output = format!("{:?} -> ", self.lhs);
        for symbol in &self.rhs {
            output.push_str(&format!("{:?} ", symbol));
        }
        output.pop();
        write!(f, "{}", output)
    }
}

pub struct ContextFreeGrammar<V, T, A> {
    pub variables: HashSet<V>,
    pub terminals: HashSet<T>,
    pub start: V,
    pub productions: Vec<Production<V, T, A>>,
}

impl<V, T, A> ContextFreeGrammar<V, T, A>
where
    V: Debug + Eq + Hash + Copy,
    T: Debug + Eq + Hash + Copy,
    A: Debug + Eq + Hash + Copy,
{
    pub fn from_file(source: &str) -> Self
    where
        V: FromStr,
        T: FromStr,
        A: FromStr,
    {
        use GrammarSymbol::*;

        let mut variables: HashSet<V> = HashSet::new();
        let mut terminals: HashSet<T> = HashSet::new();
        let mut semantic_actions: HashSet<A> = HashSet::new();
        let mut productions: Vec<Production<V, T, A>> = Vec::new();

        for line in source.lines() {
            let line = line;
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
            let rhs: Vec<GrammarSymbol<V, T, A>> = words
                .iter()
                .skip(2)
                .map(|symbol| {
                    if symbol.eq_ignore_ascii_case("EPSILON") {
                        Epsilon
                    } else if let Ok(variable) = V::from_str(symbol) {
                        variables.insert(variable);
                        Variable(variable)
                    } else if let Ok(terminal) = T::from_str(symbol) {
                        terminals.insert(terminal);
                        Terminal(terminal)
                    } else if let Ok(semantic_action) = A::from_str(symbol) {
                        semantic_actions.insert(semantic_action);
                        SemanticAction(semantic_action)
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

    pub fn get_first_sets(&self) -> HashMap<V, FirstSet<T>> {
        let mut first: HashMap<V, FirstSet<T>> = HashMap::new();

        for variable in self.variables.iter().cloned() {
            first.insert(variable, HashSet::new());
        }

        let mut modified = true;
        while modified {
            modified = false;
            for production in self.productions.iter() {
                let set = self.get_first_from_rhs(&first, &production.rhs);
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

    pub fn get_follow_sets(
        &self,
        first_sets: &HashMap<V, FirstSet<T>>,
    ) -> HashMap<V, FollowSet<T>> {
        let mut follow: HashMap<V, FollowSet<T>> = HashMap::new();

        for variable in self.variables.iter().cloned() {
            if variable == self.start {
                follow.insert(variable, vec![FollowType::DollarSign].into_iter().collect());
            } else {
                follow.insert(variable, HashSet::new());
            }
        }

        let mut modified = true;
        while modified {
            modified = false;
            //Step 2
            for production in self.productions.iter() {
                let mut iterator = production
                    .rhs
                    .iter()
                    .filter(|&symbol| match symbol {
                        GrammarSymbol::SemanticAction(_) => false,
                        _ => true,
                    })
                    .peekable();
                while iterator.peek().is_some() {
                    if let GrammarSymbol::Variable(first_variable) = iterator.next().unwrap() {
                        // Check if there is another symbol after the current one.
                        let set = match iterator.peek() {
                            Some(second_symbol) => match second_symbol {
                                GrammarSymbol::Variable(second_symbol) => {
                                    // Get the FirstSet of the second variable and remove Epsilon.
                                    first_sets[second_symbol]
                                        .iter()
                                        .filter(|&symbol| *symbol != FirstType::Epsilon)
                                        .cloned()
                                        .map(FollowType::from)
                                        .collect()
                                }
                                GrammarSymbol::Terminal(terminal) => {
                                    vec![FollowType::Terminal(*terminal)].into_iter().collect()
                                }
                                GrammarSymbol::SemanticAction(_) => unreachable!(),
                                GrammarSymbol::Epsilon => unreachable!(),
                            },
                            //Case: First_variable is the last symbol
                            None => break,
                        };
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
                let mut iterator = production
                    .rhs
                    .iter()
                    .filter(|&symbol| match symbol {
                        GrammarSymbol::SemanticAction(_) => false,
                        _ => true,
                    })
                    .peekable();

                while iterator.peek().is_some() {
                    if let GrammarSymbol::Variable(first_variable) = iterator.next().unwrap() {
                        // Check if there is another symbol after the current one.
                        let set = match iterator.peek() {
                            Some(second_symbol) => match second_symbol {
                                GrammarSymbol::Variable(second_variable) => {
                                    if first_sets[second_variable].contains(&FirstType::Epsilon) {
                                        follow[&production.lhs].clone()
                                    } else {
                                        HashSet::new()
                                    }
                                }
                                GrammarSymbol::Terminal(_) => HashSet::new(),
                                GrammarSymbol::SemanticAction(_) => unreachable!(),
                                GrammarSymbol::Epsilon => panic!(
                                    "Error in grammar: Cannot have EPSILON after first symbol."
                                ),
                            },
                            //Case: First_variable is the last symbol
                            None => follow[&production.lhs].clone(),
                        };
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

    pub fn get_table(
        &self,
        first_sets: &HashMap<V, FirstSet<T>>,
        follow_sets: &HashMap<V, FollowSet<T>>,
    ) -> ParserTable<V, T> {
        //use Symbol::*;

        let mut table: ParserTable<V, T> = HashMap::new();
        let mut collisions = 0;

        //let mut production_number = 0;
        for (production_number, production) in self.productions.iter().enumerate() {
            assert!(!production.rhs.is_empty());
            assert!(match production.rhs[0] {
                GrammarSymbol::SemanticAction(_) => false,
                _ => true,
            });
            match production.rhs[0] {
                GrammarSymbol::Epsilon => {
                    for terminal in follow_sets[&production.lhs].iter() {
                        if self.add_cell(&mut table, production.lhs, *terminal, production_number) {
                            collisions += 1;
                        }
                    }
                }
                GrammarSymbol::Terminal(terminal) => {
                    if self.add_cell(
                        &mut table,
                        production.lhs,
                        FollowType::Terminal(terminal),
                        production_number,
                    ) {
                        collisions += 1;
                    }
                }
                GrammarSymbol::Variable(_) => {
                    let set = self.get_first_from_rhs(&first_sets, &production.rhs);

                    if set.contains(&FirstType::Epsilon) {
                        //println!("Adding Variable with None.");
                        for terminal in follow_sets[&production.lhs].iter() {
                            if self.add_cell(
                                &mut table,
                                production.lhs,
                                *terminal,
                                production_number,
                            ) {
                                collisions += 1;
                            }
                        }
                    }
                    //println!("Adding Variable without None.");
                    for symbol in set.iter() {
                        match symbol {
                            FirstType::Terminal(terminal) => {
                                if self.add_cell(
                                    &mut table,
                                    production.lhs,
                                    FollowType::Terminal(*terminal),
                                    production_number,
                                ) {
                                    collisions += 1;
                                }
                            }
                            FirstType::Epsilon => {}
                        }
                    }
                }
                GrammarSymbol::SemanticAction(_) => unreachable!(),
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
        table: &mut ParserTable<V, T>,
        variable: V,
        terminal: FollowType<T>,
        new_production_number: usize,
    ) -> bool {
        match table.insert((variable, terminal), new_production_number) {
            Some(old_production_number) => {
                println!(
                    "Collison at ({:?}, {:?}) between:\nOld: {:?}\nNew: {:?}",
                    variable,
                    terminal,
                    self.productions[old_production_number],
                    self.productions[new_production_number]
                );
                true
            }
            None => false,
        }
    }

    fn get_first_from_rhs(
        &self,
        first: &HashMap<V, FirstSet<T>>,
        rhs: &[GrammarSymbol<V, T, A>],
    ) -> FirstSet<T> {
        use GrammarSymbol::*;
        let mut set: FirstSet<T> = HashSet::new();
        let mut add_epsilon = true;
        for symbol in rhs.iter().filter(|&symbol| match symbol {
            GrammarSymbol::SemanticAction(_) => false,
            _ => true,
        }) {
            match symbol {
                Terminal(terminal) => {
                    set.insert(FirstType::Terminal(*terminal));
                    add_epsilon = false;
                    break;
                }
                Variable(variable) => {
                    if first[variable].contains(&FirstType::Epsilon) {
                        let mut without_epsilon = first[variable].clone();
                        without_epsilon.remove(&FirstType::Epsilon);
                        set = set.union(&without_epsilon).cloned().collect();
                    } else {
                        set = set.union(&first[variable]).cloned().collect();
                        add_epsilon = false;
                        break;
                    }
                }
                Epsilon => {
                    set.insert(FirstType::Epsilon);
                    add_epsilon = false;
                    break;
                }
                SemanticAction(_) => unreachable!(),
            }
        }
        if add_epsilon {
            set.insert(FirstType::Epsilon);
        }
        set
    }
}

fn to_parser_set<T>(grammar_set: &HashSet<FirstType<T>>) -> HashSet<FollowType<T>>
where
    T: Eq + Hash + Clone,
{
    grammar_set
        .iter()
        .cloned()
        .map(|symbol| FollowType::from(symbol))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_grammar() {
        use super::super::language::*;

        let grammar: ContextFreeGrammar<VariableType, TokenType, SemanticActionType> =
            ContextFreeGrammar::from_file("test.txt");

        dbg!(&grammar.productions);

        let first_sets = grammar.get_first_sets();
        // //dbg!(&first);
        let follow_sets = grammar.get_follow_sets(&first_sets);
        // //dbg!(&follow);
        let table = grammar.get_table(&first_sets, &follow_sets);
        // for (key, value) in table.iter() {
        //     println!("({:?}): {:?}", key, grammar.productions[*value]);
        // }

        //dbg!(&table);
        assert!(table.is_empty());
    }
}
