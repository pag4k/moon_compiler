use super::grammar::*;
use super::language::*;
use super::lexical_analyzer::*;
use super::syntactic_analyzer_table::*;
use super::tree::*;

use std::fmt::Debug;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;

#[derive(Debug)]
enum SyntacticError<V> {
    WrongTerminal(Location, TokenType, TokenType),
    NotInTableButInFollow(Location, V, TokenType),
    NotInTableNorInFollow(Location, V, TokenType),
}

impl<V> Display for SyntacticError<V>
where
    V: Debug,
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SyntacticError::*;
        match self {
            WrongTerminal(location, expected_type, top_type) => write!(
                f,
                "Syntactic error at {}: Expecting {} but found {}.",
                location, expected_type, top_type
            ),
            NotInTableButInFollow(location, variable, top_type) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}. Skipping and continuing.",
                location, variable, top_type
            ),
            NotInTableNorInFollow(location, variable, top_type) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}.",
                location, variable, top_type
            ),
        }
    }
}

pub struct SyntacticAnalyzer<V, T, A> {
    pub table: SyntacticAnalyzerTable<V, T, A>,
}

impl<V> SyntacticAnalyzer<V, TokenType, NodeType>
where
    V: Debug + Eq + Hash + Copy + FromStr,
    //T: Debug + Eq + Hash + Copy + FromStr,
{
    pub fn from_file(source: &str) -> Self {
        let grammar: ContextFreeGrammar<V, TokenType, NodeType> =
            ContextFreeGrammar::from_file(source);

        let table = SyntacticAnalyzerTable::from_grammar(grammar);

        SyntacticAnalyzer { table }
    }

    pub fn parse(&self, tokens: &[Token]) {
        use SyntacticError::*;
        // Convert token stream and add '$' at the end.
        let mut parser_symbols: Vec<FollowType<TokenType>> = tokens
            .iter()
            .map(|token| FollowType::Terminal(token.token_type))
            .collect();
        parser_symbols.push(FollowType::DollarSign);
        let mut token_iter = tokens.iter();
        let mut token_type_iter = parser_symbols.iter();
        // Initialize the stack with '$' and the start non-terminal.
        let mut stack = vec![
            ParserSymbol::DollarSign,
            ParserSymbol::Variable(self.table.get_start()),
        ];
        let mut ast = Tree {
            root: None,
            nodes: Vec::new(),
        };
        let mut errors: Vec<SyntacticError<V>> = Vec::new();
        let mut semantic_stack: Vec<usize> = Vec::new();
        let mut token = token_iter.next().unwrap();
        let mut token_type = *token_type_iter.next().unwrap();
        'main: while let Some(symbol) = stack.last() {
            //dbg!(&symbol);
            match symbol {
                ParserSymbol::Terminal(terminal) => {
                    if FollowType::Terminal(*terminal) == token_type {
                        stack.pop();
                        token_type = *token_type_iter.next().unwrap();
                        if token_type == FollowType::DollarSign {
                            continue;
                        }
                        token = token_iter.next().unwrap();
                    } else {
                        println!(
                            "Syntactic error at {}: Expecting {:?} but found {:?}.",
                            token.location, terminal, token_type
                        );
                        errors.push(WrongTerminal(token.location, *terminal, token.token_type));
                        while FollowType::Terminal(*terminal) != token_type {
                            token_type = *token_type_iter.next().unwrap();
                            if token_type == FollowType::DollarSign {
                                println!(
                                    "Reached end of program while trying to recover from error."
                                );
                                break 'main;
                            }
                            token = token_iter.next().unwrap();
                        }
                        println!(
                            "Recovered from error at {} with {:?}.",
                            token.location, token_type
                        );

                        //println!("Stack: {:?}.", stack);
                    }
                }
                ParserSymbol::Variable(variable) => {
                    match self.table.get(*variable, token_type) {
                        Some(production) => {
                            stack.pop();
                            if production.rhs[0] == GrammarSymbol::Epsilon {
                                continue;
                            }
                            for grammar_symbol in production.rhs.iter().rev() {
                                stack.push(ParserSymbol::from(*grammar_symbol));
                            }
                        }
                        None => {
                            // TODO: Add $ case?
                            //let first_set = &self.table.first_sets[variable];
                            let follow_set = &self.table.follow_sets[variable];
                            //dbg!(first_set);
                            //dbg!(follow_set);
                            if token_type == FollowType::DollarSign
                                || follow_set.contains(&token_type)
                            {
                                println!(
                                    "Syntactic error at {} with {:?}: not expecting {:?}. Skipping and continuing.",
                                    token.location, variable,token_type
                                );
                                errors.push(NotInTableButInFollow(
                                    token.location,
                                    *variable,
                                    token.token_type,
                                ));
                                // dbg!(stack.clone());
                                stack.pop();
                            } else {
                                println!(
                                    "Syntactic error at {} with {:?}: not expecting {:?}.",
                                    token.location, variable, token_type
                                );
                                errors.push(NotInTableNorInFollow(
                                    token.location,
                                    *variable,
                                    token.token_type,
                                ));
                                while self.table.get(*variable, token_type).is_none()
                                // !first_set.contains(&FirstType::from(token_type))
                                //     && (first_set.contains(&FirstType::Epsilon)
                                //         && !follow_set.contains(&token_type))
                                {
                                    token_type = *token_type_iter.next().unwrap();
                                    // println!(
                                    //     "Scanning: token_type {:?} top {:?}.",
                                    //     token_type, variable
                                    // );
                                    if token_type == FollowType::DollarSign {
                                        println!("Reached end of program while trying to recover from error.");
                                        break 'main;
                                    }
                                    token = token_iter.next().unwrap();
                                }
                                println!(
                                    "Recovered from error at {} with {:?}.",
                                    token.location, token_type
                                );
                                //panic!();
                            }
                            //println!("Not in table ({:?}, {:?}).", variable, token_type);
                        }
                    }
                }
                ParserSymbol::SemanticAction(semantic_action) => {
                    if errors.is_empty() {
                        dbg!(semantic_action);
                        ast.make_node(&mut semantic_stack, *semantic_action);
                        // println!(
                        //     "Semantic stack: {:?}",
                        //     semantic_stack
                        //         .iter()
                        //         .map(|id| ast.get_element(*id).node_type)
                        //         .collect::<Vec<NodeType>>()
                        // );
                    }
                    stack.pop();
                }
                ParserSymbol::DollarSign => break,
            }
        }
        if token_type == FollowType::DollarSign && stack.len() == 1 {
            if errors.is_empty() {
                assert!(semantic_stack.len() == 1);
                ast.root = semantic_stack.pop();
                println!("Parse completed succesfully!");
                println!("Tokens: {:?}", token_type_iter.next());
                println!("Stack: {:?}.", stack);
            } else {
                println!("Parse completed but with {} errors:", errors.len());
                for error in errors.iter() {
                    println!("{}", error);
                }
            }
        } else {
            println!(
                "Parsing could not be completed because of {} errors:",
                errors.len()
            );
            for error in errors.iter() {
                println!("{}", error);
            }
        }
    }
}
