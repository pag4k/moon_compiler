use super::grammar::*;
use super::language::*;
use super::syntactic_analyzer_table::*;
use super::tree::*;

use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

pub struct SyntacticAnalyzer<V, T, A> {
    pub table: SyntacticAnalyzerTable<V, T, A>,
}

impl<V, T> SyntacticAnalyzer<V, T, NodeType>
where
    V: Debug + Eq + Hash + Copy + FromStr,
    T: Debug + Eq + Hash + Copy + FromStr,
{
    pub fn from_file(source: &str) -> Self {
        let grammar: ContextFreeGrammar<V, T, NodeType> = ContextFreeGrammar::from_file(source);

        let table = SyntacticAnalyzerTable::from_grammar(grammar);

        SyntacticAnalyzer { table }
    }

    pub fn parse(&self, tokens: &[T]) {
        // Convert token stream and add '$' at the end.
        let mut parser_symbols: Vec<FollowType<T>> = tokens
            .iter()
            .map(|token| FollowType::Terminal(*token))
            .collect();
        parser_symbols.push(FollowType::DollarSign);
        let mut token_iter = parser_symbols.iter();
        // Initialize the stack with '$' and the start non-terminal.
        let mut stack = vec![
            ParserSymbol::DollarSign,
            ParserSymbol::Variable(self.table.get_start()),
        ];
        let mut ast = Tree {
            root: 0,
            nodes: Vec::new(),
        };
        let mut semantic_stack: Vec<usize> = Vec::new();
        let mut token = *token_iter.next().unwrap();
        while let Some(symbol) = stack.last() {
            //dbg!(&symbol);
            match symbol {
                ParserSymbol::Terminal(terminal) => {
                    if FollowType::Terminal(*terminal) == token {
                        stack.pop();
                        token = *token_iter.next().unwrap();
                    } else {
                        println!(
                            "Terminals on stack ({:?}) and token ({:?}) do not match.",
                            terminal, token
                        );
                        println!("Stack: {:?}.", stack);
                        panic!();
                    }
                }
                ParserSymbol::Variable(variable) => match self.table.get(*variable, token) {
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
                        println!("Not in table ({:?}, {:?}).", variable, token);
                        println!("Stack: {:?}.", stack);
                        panic!();
                    }
                },
                ParserSymbol::SemanticAction(semantic_action) => {
                    ast.make_node(&mut semantic_stack, *semantic_action);
                    dbg!(semantic_stack
                        .iter()
                        .map(|id| ast.get_element(*id).node_type)
                        .collect::<Vec<NodeType>>());
                    stack.pop();
                }
                ParserSymbol::DollarSign => break,
            }
        }
        if token != FollowType::DollarSign {
            println!("Last token is not '$': {:?}", token);
            println!("Stack: {:?}.", stack);
            panic!();
        }
        println!("Parse completed succesfully!");
        println!("Tokens: {:?}", token_iter.next());
        println!("Stack: {:?}.", stack);
        //dbg!(semantic_stack);
    }
}
