use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
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

impl<V: Display, T: Display, A: Display> Display for GrammarSymbol<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use GrammarSymbol::*;
        let output = match self {
            Variable(variable) => format!("{}", variable),
            Terminal(terminal) => format!("'{}'", terminal),
            SemanticAction(semantic_action) => format!("'{}'", semantic_action),
            Epsilon => "EPSILON".to_string(),
        };
        write!(f, "{}", output)
    }
}

impl<V, T, A> GrammarSymbol<V, T, A> {
    pub fn is_variable(&self) -> bool {
        use GrammarSymbol::*;
        match self {
            Variable(_) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum FirstType<T> {
    Terminal(T),
    Epsilon,
}

impl<T: Display> Display for FirstType<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use FirstType::*;
        let output = match self {
            Terminal(terminal) => format!("'{}'", terminal),
            Epsilon => "EPSILON".to_string(),
        };
        write!(f, "{}", output)
    }
}

impl<T> From<FollowType<T>> for FirstType<T> {
    fn from(grammar_symbol: FollowType<T>) -> Self {
        match grammar_symbol {
            FollowType::Terminal(terminal) => FirstType::Terminal(terminal),
            //FIXME: Not sure what to do with this.
            FollowType::DollarSign => unreachable!(),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum ParserSymbol<V, T, A> {
    Variable(V),
    Terminal(T),
    SemanticAction(A),
    DollarSign,
}

impl<V: Display, T: Display, A: Display> Display for ParserSymbol<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use ParserSymbol::*;
        let output = match self {
            Variable(variable) => format!("{}", variable),
            Terminal(terminal) => format!("'{}'", terminal),
            SemanticAction(semantic_action) => format!("'{}'", semantic_action),
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

impl<T: Display> Display for FollowType<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use FollowType::*;
        let output = match self {
            //Variable(variable) => format!("{:?}", variable),
            Terminal(terminal) => format!("'{}'", terminal),
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

impl<V: Display, T: Display, A: Display> Display for Production<V, T, A> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut output = format!("{} -> ", self.lhs);
        for symbol in &self.rhs {
            output.push_str(&format!("{} ", symbol));
        }
        output.pop();
        write!(f, "{}", output)
    }
}

pub enum GrammarError {
    CollisionsInTable(Vec<String>),
    InvalidVariable(String),
    InvalidSymbol(String),
    EpsilonNotFirstOnRHS(String),
}

impl Display for GrammarError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use GrammarError::*;
        match self {
            CollisionsInTable(collisions) => {
                let mut output = format!(
                    "ERROR: {} collision(s) where found in the parsing table:\n",
                    collisions.len()
                );
                for collision in collisions {
                    output.push_str(&format!("{}\n", collision));
                }
                write!(f, "{}", output)
            }
            InvalidVariable(string) => write!(f, "ERROR: Invalid variable on LHS: {}", string),
            InvalidSymbol(string) => write!(f, "ERROR: Invalid symbol on RHS: {}", string),
            EpsilonNotFirstOnRHS(string) => write!(
                f,
                "ERROR: Epsilon not in first position of production: {}",
                string
            ),
        }
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
    V: Display + Eq + Hash + Copy,
    T: Display + Eq + Hash + Copy,
    A: Display + Eq + Hash + Copy,
{
    pub fn from_string(source: &str) -> Result<Self, GrammarError>
    where
        V: FromStr,
        T: FromStr,
        A: FromStr,
    {
        use GrammarError::*;
        use GrammarSymbol::*;

        let mut variables: HashSet<V> = HashSet::new();
        let mut terminals: HashSet<T> = HashSet::new();
        let mut semantic_actions: HashSet<A> = HashSet::new();
        let mut productions: Vec<Production<V, T, A>> = Vec::new();

        for line in source.lines() {
            let words: Vec<&str> = line.split_whitespace().collect();
            let lhs = match V::from_str(words[0]) {
                Ok(lhs) => {
                    variables.insert(lhs);
                    lhs
                }
                Err(_) => return Err(InvalidVariable(words[0].to_string())),
            };
            let mut rhs: Vec<GrammarSymbol<V, T, A>> = Vec::new();
            for symbol in words.iter().skip(2) {
                if symbol.eq_ignore_ascii_case("EPSILON") {
                    rhs.push(Epsilon);
                } else if let Ok(variable) = V::from_str(symbol) {
                    variables.insert(variable);
                    rhs.push(Variable(variable));
                } else if let Ok(terminal) = T::from_str(symbol) {
                    terminals.insert(terminal);
                    rhs.push(Terminal(terminal));
                } else if let Ok(semantic_action) = A::from_str(symbol) {
                    semantic_actions.insert(semantic_action);
                    rhs.push(SemanticAction(semantic_action));
                } else {
                    return Err(InvalidSymbol(symbol.to_string()));
                }
            }
            productions.push(Production { lhs, rhs });
        }

        Ok(ContextFreeGrammar {
            variables,
            terminals,
            start: productions[0].lhs,
            productions,
        })
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
    ) -> Result<HashMap<V, FollowSet<T>>, GrammarError> {
        use GrammarError::*;
        use GrammarSymbol::*;

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
                        SemanticAction(_) => false,
                        _ => true,
                    })
                    .peekable();
                while iterator.peek().is_some() {
                    if let Variable(first_variable) = iterator.next().unwrap() {
                        // Check if there is another symbol after the current one.
                        let set = match iterator.peek() {
                            Some(second_symbol) => match second_symbol {
                                Variable(second_symbol) => {
                                    // Get the FirstSet of the second variable and remove Epsilon.
                                    first_sets[second_symbol]
                                        .iter()
                                        .filter(|&symbol| *symbol != FirstType::Epsilon)
                                        .cloned()
                                        .map(FollowType::from)
                                        .collect()
                                }
                                Terminal(terminal) => {
                                    vec![FollowType::Terminal(*terminal)].into_iter().collect()
                                }
                                SemanticAction(_) => {
                                    unreachable!("Semantic actions should be filtered out.")
                                }
                                Epsilon => {
                                    return Err(EpsilonNotFirstOnRHS(production.to_string()))
                                }
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
                        SemanticAction(_) => false,
                        _ => true,
                    })
                    .peekable();

                while iterator.peek().is_some() {
                    if let Variable(first_variable) = iterator.next().unwrap() {
                        // Check if there is another symbol after the current one.
                        let set = match iterator.peek() {
                            Some(second_symbol) => match second_symbol {
                                Variable(second_variable) => {
                                    if first_sets[second_variable].contains(&FirstType::Epsilon) {
                                        follow[&production.lhs].clone()
                                    } else {
                                        HashSet::new()
                                    }
                                }
                                Terminal(_) => HashSet::new(),
                                SemanticAction(_) => {
                                    unreachable!("Semantic actions should be filtered out.")
                                }
                                Epsilon => {
                                    return Err(EpsilonNotFirstOnRHS(production.to_string()))
                                }
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

        Ok(follow)
    }

    pub fn get_table(
        &self,
        first_sets: &HashMap<V, FirstSet<T>>,
        follow_sets: &HashMap<V, FollowSet<T>>,
    ) -> Result<ParserTable<V, T>, GrammarError> {
        use GrammarError::*;

        let mut table: ParserTable<V, T> = HashMap::new();
        let mut collisions = Vec::new();

        for (production_number, production) in self.productions.iter().enumerate() {
            assert!(!production.rhs.is_empty());

            let set = self.get_first_from_rhs(&first_sets, &production.rhs);

            if set.contains(&FirstType::Epsilon) {
                for terminal in follow_sets[&production.lhs].iter() {
                    match self.add_cell(&mut table, production.lhs, *terminal, production_number) {
                        Ok(_) => {}
                        Err(collision) => collisions.push(collision),
                    }
                }
            }
            for symbol in set.iter() {
                match symbol {
                    FirstType::Terminal(terminal) => {
                        match self.add_cell(
                            &mut table,
                            production.lhs,
                            FollowType::Terminal(*terminal),
                            production_number,
                        ) {
                            Ok(_) => {}
                            Err(collision) => collisions.push(collision),
                        }
                    }
                    FirstType::Epsilon => {}
                }
            }
        }

        if collisions.is_empty() {
            Ok(table)
        } else {
            Err(CollisionsInTable(collisions))
        }
    }

    fn add_cell(
        &self,
        table: &mut ParserTable<V, T>,
        variable: V,
        terminal: FollowType<T>,
        new_production_number: usize,
    ) -> Result<(), String> {
        match table.insert((variable, terminal), new_production_number) {
            Some(old_production_number) => Err(format!(
                "Collison at ({}, {}) between:\nOld: {}\nNew: {}",
                variable,
                terminal,
                self.productions[old_production_number],
                self.productions[new_production_number]
            )),
            None => Ok(()),
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
        for symbol in rhs {
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
                SemanticAction(_) => {}
            }
        }
        if add_epsilon {
            set.insert(FirstType::Epsilon);
        }
        set
    }
}
