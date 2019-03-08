use crate::ast_node::*;
use crate::grammar::*;
use crate::language::*;
use crate::lexical_analyzer::*;
use crate::symbol_table::*;
use crate::syntactic_analyzer_table::*;
use crate::tree::*;

use std::fmt::{Display, Formatter};

pub enum SyntacticError {
    WrongTerminal(Location, TokenType, TokenType, Option<Location>),
    NotInTableButInFollow(Location, VariableType, TokenType),
    NotInTableNorInFollow(Location, VariableType, TokenType, Option<Location>),
    NoNodeOnDataStack(NodeType),
    NoNodeOnStackOne(NodeType, NodeType),
    NoNodeOnStackList(NodeType, Vec<NodeType>),
    WrongNodeOnStackOne(NodeType, NodeType, NodeType),
    WrongNodeOnStackList(NodeType, Vec<NodeType>, NodeType),
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
            NoNodeOnDataStack(node_type) => write!(
                f,
                "AST error: No data found on data stack to make node: {}.",
                node_type
            ),
            NoNodeOnStackOne(node_type, child_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {}, but there is node on stack.",
                node_type, child_node_type
            ),
            NoNodeOnStackList(node_type, node_list) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {:?}, but there is node on stack.",
                node_type, node_list
            ),
            WrongNodeOnStackOne(node_type, child_node_type, actual_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {}, but found {} on stack.",
                node_type, child_node_type, actual_node_type
            ),
            WrongNodeOnStackList(node_type, node_list, actual_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {:?}, but found {} on stack.",
                node_type, node_list, actual_node_type
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
    ) -> Result<(Tree<NodeElement, SymbolTableArena>, DerivationTable), Vec<SyntacticError>> {
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
                        let (error_location, error_terminal, error_type) =
                            (token.location, *terminal, token.token_type);
                        // Skip tokens until one the one on the stack is found.
                        while Terminal(*terminal) != token_type {
                            token_type = *token_type_iter.next().unwrap();
                            // If end of progam is reached, break from main loop.
                            if token_type == DollarSign {
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
                            }
                        }
                        None => {
                            // If not production is found, error.
                            // If token is in follow set of the variable on top of the stack.
                            if token_type == FollowType::DollarSign
                                || self.table.follow_sets[variable].contains(&token_type)
                            {
                                errors.push(NotInTableButInFollow(
                                    token.location,
                                    *variable,
                                    token.token_type,
                                ));
                                stack.pop();
                            } else {
                                let (error_location, error_variable, error_type) =
                                    (token.location, *variable, token.token_type);
                                // Otherwise, skip tokens until one that leads to a production is found.
                                while self.table.get(*variable, token_type).is_none() {
                                    token_type = *token_type_iter.next().unwrap();
                                    // If end of progam is reached, break from main loop.
                                    if token_type == FollowType::DollarSign {
                                        println!("Reached end of program while trying to recover from error.");
                                        errors.push(NotInTableNorInFollow(
                                            error_location,
                                            error_variable,
                                            error_type,
                                            None,
                                        ));
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
                            }
                        }
                    }
                }
                ParserSymbol::SemanticAction(semantic_action) => {
                    // Do not handle semantic actions if an error was found.
                    if errors.is_empty() {
                        match semantic_action {
                            // If data action, push on data stack.
                            NodeType::Data => {
                                data_stack.push(last_token.unwrap().lexeme.clone().unwrap());
                            }
                            // Otherwise, try to make a node.
                            _ => {
                                if let Err(error) = ast.make_node(
                                    &mut semantic_stack,
                                    &mut data_stack,
                                    *semantic_action,
                                ) {
                                    return Err(vec![error]);
                                }
                            }
                        }
                    }
                    stack.pop();
                }
                ParserSymbol::DollarSign => break,
            }
        }

        if errors.is_empty() {
            assert!(token_type == FollowType::DollarSign);
            assert!(stack.len() == 1);
            assert!(semantic_stack.len() == 1);
            ast.root = semantic_stack.pop();
            derivation_table.push((derivation.clone(), None));
            Ok((ast, derivation_table))
        } else {
            Err(errors)
        }
    }
}
