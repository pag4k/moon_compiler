use crate::ast_node::*;
use crate::grammar::*;
use crate::language::*;
use crate::lexical_analyzer::*;
use crate::symbol_table::*;
use crate::syntactic_analyzer_table::*;
use crate::syntactic_error::*;
use crate::tree::*;

pub struct SyntacticAnalyzer {
    pub table: SyntacticAnalyzerTable<VariableType, TokenType, NodeType>,
}

type Symbol = GrammarSymbol<VariableType, TokenType, NodeType>;
type OptionalProduction = Option<Production<VariableType, TokenType, NodeType>>;
type DerivationTable = Vec<(Vec<Symbol>, OptionalProduction)>;
type ParserOutput = Result<(AST, DerivationTable, Vec<SyntacticError>), ASTError>;

impl SyntacticAnalyzer {
    pub fn from_grammar(
        grammar: ContextFreeGrammar<VariableType, TokenType, NodeType>,
    ) -> Result<Self, GrammarError> {
        match SyntacticAnalyzerTable::from_grammar(grammar) {
            Ok(table) => Ok(SyntacticAnalyzer { table }),
            Err(error) => Err(error),
        }
    }

    pub fn parse(&self, tokens: Vec<Token>) -> ParserOutput {
        use SyntacticError::*;

        // Convert token stream and add '$' at the end.
        let mut parser_symbols: Vec<FollowType<TokenType>> = tokens
            .iter()
            .map(|token| FollowType::Terminal(token.token_type))
            .collect();
        parser_symbols.push(FollowType::DollarSign);

        // Get iterators for the token stream and the token type stream.
        let mut token_iter = tokens.into_iter();
        let mut token_type_iter = parser_symbols.iter();
        let mut token = token_iter.next().unwrap();
        //         let mut token = match token_iter.next() {
        //     Some(new_token) => new_token,
        //     None =>
        // }
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
        let mut token_stack: Vec<Token> = Vec::new();

        // Initialize vector to accumulate syntatic errors.
        let mut syntactic_errors: Vec<SyntacticError> = Vec::new();

        // Initialize vectors to keep track of derivation.
        let mut derivation: Vec<Symbol> = vec![GrammarSymbol::Variable(VariableType::Prog)];
        let mut derivation_table: DerivationTable = Vec::new();

        let mut construct_ast = true;

        'main: while let Some(symbol) = stack.last() {
            match symbol {
                ParserSymbol::Terminal(terminal) => {
                    use FollowType::*;
                    if Terminal(*terminal) == token_type {
                        // If the current token and the one on top of the stack match.
                        stack.pop();
                        token_type = *token_type_iter.next().unwrap();
                        if token_type == DollarSign {
                            continue;
                        }
                        last_token = Some(token);
                        token = token_iter.next().unwrap();
                    } else {
                        // If not, there is an error.
                        let (start_token, error_terminal) = (token.clone(), *terminal);
                        // Skip tokens until the one on the stack is found.
                        while Terminal(*terminal) != token_type {
                            token_type = *token_type_iter.next().unwrap();
                            if token_type == DollarSign {
                                syntactic_errors.push(WrongTerminal(
                                    start_token,
                                    error_terminal,
                                    None,
                                ));
                                break 'main;
                            }
                            last_token = Some(token);
                            token = token_iter.next().unwrap();
                        }
                        syntactic_errors.push(WrongTerminal(
                            start_token,
                            error_terminal,
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
                            // Add to derivation table.
                            derivation_table.push((derivation.clone(), Some(production.clone())));
                            //Get the first variable in derivation.
                            let (position, &variable) = derivation
                                .iter()
                                .enumerate()
                                .skip_while(|(_, &symbol)| !symbol.is_variable())
                                .next()
                                .expect("ERROR: No variable found in derivation.");
                            //Confirm that it is the same as the LHS of the production.
                            if construct_ast {
                                assert!(variable == Variable(production.lhs));
                            }
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
                        None => {
                            // If not production is found, error.
                            // If token is in follow set of the variable on top of the stack.
                            if token_type == FollowType::DollarSign
                                || self.table.follow_sets[variable].contains(&token_type)
                            {
                                syntactic_errors
                                    .push(NotInTableButInFollow(token.clone(), *variable));
                                stack.pop();
                                // This error prevent valid construction of AST.
                                construct_ast = false;
                            } else {
                                let (start_token, error_variable) = (token.clone(), *variable);
                                // Otherwise, skip tokens until one that leads to a production is found.
                                while self.table.get(*variable, token_type).is_none() {
                                    token_type = *token_type_iter.next().unwrap();
                                    // If end of progam is reached, break from main loop.
                                    if token_type == FollowType::DollarSign {
                                        syntactic_errors.push(NotInTableNorInFollow(
                                            token.clone(),
                                            error_variable,
                                            None,
                                        ));
                                        break 'main;
                                    }
                                    last_token = Some(token);
                                    token = token_iter.next().unwrap();
                                }
                                syntactic_errors.push(NotInTableNorInFollow(
                                    start_token,
                                    error_variable,
                                    Some(token.location),
                                ));
                            }
                        }
                    }
                }
                ParserSymbol::SemanticAction(semantic_action) => {
                    // Handle semantic action.
                    if construct_ast {
                        match semantic_action {
                            // If data action, push on data stack.
                            NodeType::Data => {
                                token_stack.push(last_token.clone().unwrap());
                            }
                            // If end program, but the token is not '$'.
                            NodeType::EndProgram => {
                                stack.pop();
                                if token_type != FollowType::DollarSign {
                                    let mut token_list: Vec<Token> = token_iter.collect();
                                    token_list.push(token.clone());
                                    token_iter = token_list.into_iter();
                                }
                                break;
                            }
                            // Otherwise, try to make a node.
                            _ => {
                                ast.make_node(
                                    &mut semantic_stack,
                                    &mut token_stack,
                                    *semantic_action,
                                )?;
                            }
                        }
                    }
                    stack.pop();
                }
                ParserSymbol::DollarSign => break,
            }
        }
        for new_token in token_iter {
            syntactic_errors.push(TokenAfterMain(new_token.clone()));
        }
        //assert!(stack.len() == 1);
        //assert!(semantic_stack.len() == 1);
        ast.root = semantic_stack.pop();
        derivation_table.push((derivation.clone(), None));
        Ok((ast, derivation_table, syntactic_errors))
    }
}
