use super::grammar::*;
use super::language::*;
use super::lexical_analyzer::*;
use super::syntactic_analyzer_table::*;
use super::tree::*;

use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum SyntacticError {
    WrongTerminal(Location, TokenType, TokenType, Option<Location>),
    NotInTableButInFollow(Location, VariableType, TokenType),
    NotInTableNorInFollow(Location, VariableType, TokenType, Option<Location>),
}

impl Display for SyntacticError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use SyntacticError::*;
        match self {
            WrongTerminal(location, expected_type, top_type, recovery_location) => write!(
                f,
                "Syntactic error at {}: Expecting {} but found {}. {}.",
                location,
                expected_type,
                top_type,
                match recovery_location {
                    Some(location) => format!("Recovered at {}", location),
                    None => "Could not recover, reached end of program".to_string(),
                }
            ),
            NotInTableButInFollow(location, variable, top_type) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}. Skipping and continuing.",
                location, variable, top_type
            ),
            NotInTableNorInFollow(location, variable, top_type, recovery_location) => write!(
                f,
                "Syntactic error at {} with {:?}: not expecting {}. {}.",
                location,
                variable,
                top_type,
                match recovery_location {
                    Some(location) => format!("Recovered at {}", location),
                    None => "Could not recover, reached end of program".to_string(),
                }
            ),
        }
    }
}

pub struct SyntacticAnalyzer {
    pub table: SyntacticAnalyzerTable<VariableType, TokenType, NodeType>,
}

type Symbol = GrammarSymbol<VariableType, TokenType, NodeType>;
type OptionalProduction = Option<Production<VariableType, TokenType, NodeType>>;
type DerivationTable = Vec<(Vec<Symbol>, OptionalProduction)>;

impl SyntacticAnalyzer {
    pub fn from_grammar(
        grammar: ContextFreeGrammar<VariableType, TokenType, NodeType>,
    ) -> Result<Self, GrammarError> {
        match SyntacticAnalyzerTable::from_grammar(grammar) {
            Ok(table) => Ok(SyntacticAnalyzer { table }),
            Err(error) => Err(error),
        }
    }

    pub fn parse(
        &self,
        tokens: &[Token],
    ) -> Result<(Tree<NodeElement>, DerivationTable), Vec<SyntacticError>> {
        use SyntacticError::*;

        // Convert token stream and add '$' at the end.
        let mut parser_symbols: Vec<FollowType<TokenType>> = tokens
            .iter()
            .map(|token| FollowType::Terminal(token.token_type))
            .collect();
        parser_symbols.push(FollowType::DollarSign);

        // Get iterators for the token stream and the token type stream.
        let mut token_iter = tokens.iter();
        let mut token_type_iter = parser_symbols.iter();
        let mut token = token_iter.next().unwrap();
        let mut token_type = *token_type_iter.next().unwrap();
        let mut last_token = None;

        // Initialize the stack with '$' and the start variable.
        let mut stack = vec![
            ParserSymbol::DollarSign,
            ParserSymbol::Variable(self.table.get_start()),
        ];

        // Initialize AST, semantic stack and data stack.
        let mut ast = Tree::default();
        let mut semantic_stack: Vec<usize> = Vec::new();
        let mut data_stack: Vec<String> = Vec::new();

        // Initialize vector to accumulate syntatic errors.
        let mut errors: Vec<SyntacticError> = Vec::new();

        // Initialize vectors to keep track of derivation.
        let mut derivation: Vec<Symbol> = vec![GrammarSymbol::Variable(VariableType::Prog)];
        let mut derivation_table: DerivationTable = Vec::new();

        'main: while let Some(symbol) = stack.last() {
            //dbg!(&symbol);
            match symbol {
                ParserSymbol::Terminal(terminal) => {
                    use FollowType::*;
                    if Terminal(*terminal) == token_type {
                        // If the current token and the one on top of the stack match.
                        stack.pop();
                        token_type = *token_type_iter.next().unwrap();
                        // If the following token is $, continue since the stack should be empty.
                        if token_type == DollarSign {
                            continue;
                        }
                        last_token = Some(token);
                        token = token_iter.next().unwrap();
                    } else {
                        // If not, there is an error.
                        // println!(
                        //     "Syntactic error at {}: Expecting {:?} but found {:?}.",
                        //     token.location, terminal, token_type
                        // );
                        let (error_location, error_terminal, error_type) =
                            (token.location, *terminal, token.token_type);
                        while Terminal(*terminal) != token_type {
                            token_type = *token_type_iter.next().unwrap();
                            if token_type == DollarSign {
                                // println!(
                                //     "Reached end of program while trying to recover from error."
                                // );
                                errors.push(WrongTerminal(
                                    error_location,
                                    error_terminal,
                                    error_type,
                                    None,
                                ));
                                break 'main;
                            }
                            last_token = Some(token);
                            token = token_iter.next().unwrap();
                        }
                        errors.push(WrongTerminal(
                            error_location,
                            error_terminal,
                            error_type,
                            Some(token.location),
                        ));

                        // println!(
                        //     "Recovered from error at {} with {:?}.",
                        //     token.location, token_type
                        // );

                        //println!("Stack: {:?}.", stack);
                    }
                }
                ParserSymbol::Variable(variable) => {
                    use GrammarSymbol::*;
                    // Since a variable is on top of the stack, use the parsing table.
                    match self.table.get(*variable, token_type) {
                        Some(production) => {
                            // If a production is found.
                            stack.pop();
                            //Reverse iteration over the RHS of the production.
                            for symbol in production.rhs.iter().rev() {
                                if *symbol == Epsilon {
                                    break;
                                }
                                //Push variables, terminals and semantic actions on stack.
                                stack.push(ParserSymbol::from(*symbol));
                            }
                            // If there are no errors, also add to derivation table.
                            if errors.is_empty() {
                                derivation_table
                                    .push((derivation.clone(), Some(production.clone())));
                                //Get the first variable in derivation.
                                let (position, &variable) = derivation
                                    .iter()
                                    .enumerate()
                                    .skip_while(|(_, &symbol)| !symbol.is_variable())
                                    .next()
                                    .expect("ERROR: No variable found in derivation.");
                                //Confirm that it is the same as the LHS of the production.
                                assert!(variable == Variable(production.lhs));
                                //Remove the first variable.
                                derivation.remove(position);
                                for symbol in production.rhs.iter().rev() {
                                    match symbol {
                                        //If symbol is variable, insert at position in derivation.
                                        Variable(variable) => {
                                            derivation.insert(position, Variable(*variable))
                                        }
                                        //If symbol is terminal, insert at position in derivation.
                                        Terminal(terminal) => {
                                            derivation.insert(position, Terminal(*terminal))
                                        }
                                        //Ignore the rest epsilon and semantic actions.
                                        _ => {}
                                    }
                                }
                                //dbg!(&derivation);
                            }
                        }
                        None => {
                            // If not production is found, error.
                            //let first_set = &self.table.first_sets[variable];
                            let follow_set = &self.table.follow_sets[variable];
                            //dbg!(first_set);
                            //dbg!(follow_set);
                            if token_type == FollowType::DollarSign
                                || follow_set.contains(&token_type)
                            {
                                // println!(
                                //     "Syntactic error at {} with {:?}: not expecting {:?}. Skipping and continuing.",
                                //     token.location, variable,token_type
                                // );
                                errors.push(NotInTableButInFollow(
                                    token.location,
                                    *variable,
                                    token.token_type,
                                ));
                                // dbg!(stack.clone());
                                stack.pop();
                            } else {
                                // println!(
                                //     "Syntactic error at {} with {:?}: not expecting {:?}.",
                                //     token.location, variable, token_type
                                // );
                                let (error_location, error_variable, error_type) =
                                    (token.location, *variable, token.token_type);
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
                                        errors.push(NotInTableNorInFollow(
                                            error_location,
                                            error_variable,
                                            error_type,
                                            None,
                                        ));
                                        // dbg!(first_set);
                                        // dbg!(follow_set);
                                        break 'main;
                                    }
                                    last_token = Some(token);
                                    token = token_iter.next().unwrap();
                                }
                                errors.push(NotInTableNorInFollow(
                                    error_location,
                                    error_variable,
                                    error_type,
                                    Some(token.location),
                                ));
                                // println!(
                                //     "Recovered from error at {} with {:?}.",
                                //     token.location, token_type
                                // );
                                //panic!();
                            }
                            //println!("Not in table ({:?}, {:?}).", variable, token_type);
                        }
                    }
                }
                ParserSymbol::SemanticAction(semantic_action) => {
                    if errors.is_empty() {
                        match semantic_action {
                            NodeType::Data => {
                                data_stack.push(last_token.unwrap().lexeme.clone().unwrap());
                            }
                            _ => {
                                //dbg!(semantic_action);
                                ast.make_node(
                                    &mut semantic_stack,
                                    &mut data_stack,
                                    *semantic_action,
                                );
                                // println!(
                                //     "Semantic stack: {:?}",
                                //     semantic_stack
                                //         .iter()
                                //         .map(|id| ast.get_element(*id).node_type)
                                //         .collect::<Vec<NodeType>>()
                                // );
                            }
                        }
                    }
                    stack.pop();
                }
                ParserSymbol::DollarSign => break,
            }
        }
        // dbg!(&self.table.first_sets[&VariableType::Prog]);
        // dbg!(&self.table.follow_sets[&VariableType::Prog]);
        // dbg!(&self.table.first_sets[&VariableType::ClassDeclList]);
        // dbg!(&self.table.follow_sets[&VariableType::ClassDeclList]);
        // dbg!(&self.table.first_sets[&VariableType::FuncDefList]);
        // dbg!(&self.table.follow_sets[&VariableType::FuncDefList]);

        if errors.is_empty() {
            assert!(token_type == FollowType::DollarSign);
            assert!(stack.len() == 1);
            assert!(semantic_stack.len() == 1);
            ast.root = semantic_stack.pop();
            // dbg!(ast.get_children(ast.root.unwrap()));
            // dbg!(ast.get_element(46));
            // dbg!(ast.get_element(278));
            // dbg!(ast.get_element(454));
            //dbg!(derivation);
            // println!("Tokens: {:?}", token_type_iter.next());
            // println!("Stack: {:?}.", stack);
            //Push the final state of derivation.
            derivation_table.push((derivation.clone(), None));
            //dbg!(derivation_table);
            //Verify if derivation is equal to the token stream.

            Ok((ast, derivation_table))
        } else {
            Err(errors)
        }
    }
}
