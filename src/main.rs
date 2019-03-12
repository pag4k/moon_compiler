mod ast_node;
mod dot_generator;
mod finite_accepter;
mod grammar;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod nfa_generator;
mod semantic_analysis;
mod semantic_checking;
mod semantic_class_checker;
mod symbol_table;
mod symbol_table_generator;
mod syntactic_analyzer;
mod syntactic_analyzer_table;
mod tree;
mod tree_dot_printer;

use ast_node::*;
use grammar::*;
use language::*;
use lexical_analyzer::*;
use semantic_class_checker::*;
use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use syntactic_analyzer::*;

fn main() {
    // Get the command line arguments.
    let args: Vec<String> = env::args().collect();

    // If there was a first argument, use it as source file, otherwise default.txt.
    let source_filename = if args.len() < 2 {
        "default.txt"
    } else {
        &args[1]
    };

    let source = match fs::read_to_string(source_filename) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong reading source file: {}. Exiting...",
                source_filename
            );
            return;
        }
    };

    let atocc_filename = "atocc.txt";
    let path = Path::new(atocc_filename);
    let mut atocc_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating AtoCC file: {}. Exiting...",
                atocc_filename
            );
            return;
        }
    };

    let error_filename = "error.txt";
    let path = Path::new(error_filename);
    let mut error_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating error file: {}. Exiting...",
                error_filename
            );
            return;
        }
    };

    // Create LexicalAnalyzer and iterate over the tokens.
    let lexical_analyzer = LexicalAnalyzer::from_string(&source);
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexical_errors = Vec::new();
    for token in lexical_analyzer {
        match token {
            Ok(token) => {
                //println!("{}", token);
                match token.token_type {
                    TokenType::Comment(_) => {}
                    _ => tokens.push(token.clone()),
                }
                atocc_file
                    .write_fmt(format_args!("{} ", token.token_type))
                    .expect("Could not write to AtoCC file.");
            }
            Err(error) => {
                lexical_errors.push(error);
            }
        }
    }

    if lexical_errors.is_empty() {
        println!("Lexical analysis completed succesfully!")
    } else {
        println!(
            "ERROR: Lexical analyzer found {} errors:",
            lexical_errors.len()
        );
        for (n, error) in lexical_errors.iter().enumerate() {
            println!("{}. {}", n + 1, error);
            error_file
                .write_fmt(format_args!("{}\n", error))
                .expect("Could not write to error file.");
        }
        return;
    }

    let grammar_filename = "grammar.txt";
    let grammar_source = match fs::read_to_string("grammar.txt") {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong reading grammar file: {}. Exiting...",
                source_filename
            );
            return;
        }
    };

    let grammar = match ContextFreeGrammar::from_string(&grammar_source) {
        Ok(grammar) => {
            println!(
                "Context-free grammar generated succesfully from {}!",
                grammar_filename
            );
            grammar
        }
        Err(error) => {
            println!("{}", error);
            return;
        }
    };

    let syntactic_analyzer = match SyntacticAnalyzer::from_grammar(grammar) {
        Ok(syntactic_analyzer) => {
            println!("Parsing table generated succesfully from LL(1) grammar!");
            syntactic_analyzer
        }
        Err(error) => {
            println!("{}", error);
            return;
        }
    };

    let mut ast = match syntactic_analyzer.parse(&tokens) {
        Ok((ast, mut derivation_table)) => {
            println!("Parse completed succesfully!");
            // if tokens
            //     .iter()
            //     .map(|token| GrammarSymbol::Terminal(token.token_type))
            //     .collect::<Vec<GrammarSymbol<VariableType, TokenType, NodeType>>>()
            //     == derivation_table.pop().unwrap().0
            // {
            //     let derivation_filename = "derivation.txt";
            //     let path = Path::new(derivation_filename);
            //     let mut derivation_filename = match File::create(&path) {
            //         Ok(file) => file,
            //         Err(_) => {
            //             println!(
            //                 "ERROR: Something went wrong creating derivation file: {}. Exiting...",
            //                 error_filename
            //             );
            //             return;
            //         }
            //     };
            //     for (derivation, production) in derivation_table.iter() {
            //         for symbol in derivation.iter() {
            //             derivation_filename
            //                 .write_fmt(format_args!("{} ", symbol))
            //                 .expect("Could not write to derivation file.");
            //         }
            //         if let Some(production) = production {
            //             derivation_filename
            //                 .write_fmt(format_args!("\nPRODUCTION: {}\n", production))
            //                 .expect("Could not write to derivation file.");
            //         } else {
            //             derivation_filename
            //                 .write_fmt(format_args!("\n"))
            //                 .expect("Could not write to derivation file.");
            //         }
            //     }
            //
            //     println!("Result of derivation is equal to the token stream! Printed in 'derivation.txt.'");
            // }
            ast
        }
        Err(syntactic_errors) => {
            println!("ERROR: Parser found {} error(s):", syntactic_errors.len());
            for (n, error) in syntactic_errors.iter().enumerate() {
                println!("{}. {}", n + 1, error);
                error_file
                    .write_fmt(format_args!("{}\n", error))
                    .expect("Could not write to error file.");
            }
            return;
        }
    };

    let ast_filename = "ast.gv";
    match ast.print_tree_to(ast_filename) {
        Ok(()) => println!("Succesfully generated AST graph file: {}.", ast_filename),
        Err(error) => println!("{}", error),
    };

    if let Err(error) = ast.generate_symbol_table() {
        dbg!(error);
        return;
    }

    if let Err(error) = ast.semantic_class_checker() {
        dbg!(error);
        return;
    }

    ast.symbol_table_arena.print();
    //Second pass
    //Add inherited classes in table
    //Check inherited class existence
    //Check circular dependancy
    //Check shadow variable

    //ast.semantic_checking();
}
