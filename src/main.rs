mod ast_node;
mod dot_generator;
mod finite_accepter;
mod grammar;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod lexical_error;
mod nfa_generator;
mod semantic_analysis_common;
mod semantic_class_checker;
mod semantic_error;
mod semantic_function_checker;
mod symbol_table;
mod symbol_table_generator;
mod syntactic_analyzer;
mod syntactic_analyzer_table;
mod syntactic_error;
mod tree;
mod tree_dot_printer;
mod type_checker;

use grammar::*;
use language::*;
use lexical_analyzer::*;
use semantic_error::*;
use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use syntactic_analyzer::*;
use syntactic_error::*;

use std::fmt::{Display, Formatter};

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

    let symbol_table_filename = "symbol_table.txt";
    let path = Path::new(symbol_table_filename);
    let mut symbol_table_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating symbol table file: {}. Exiting...",
                symbol_table_filename
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

    if tokens.is_empty() {
        println!("Error: Empty program. Exiting...");
        return;
    } else {
        println!("Lexical analysis completed.");
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

    let (mut ast, syntactic_errors) = match syntactic_analyzer.parse(tokens) {
        Ok((ast, mut derivation_table, syntactic_errors)) => {
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

            (ast, syntactic_errors)
        }
        Err(ast_error) => {
            println!(
                "ERROR: Abstract syntaxt tree error: {}. Exiting...",
                ast_error
            );
            return;
        }
    };

    println!("Syntactic analysis completed.");

    if syntactic_errors
        .iter()
        .any(|syntactic_error| syntactic_error.failed_to_recover())
    {
        print_errors(
            lexical_errors,
            syntactic_errors,
            Vec::new(),
            &mut error_file,
        );
        return;
    }

    let ast_filename = "ast.gv";
    match ast.print_tree_to(ast_filename) {
        Ok(()) => println!("Succesfully generated AST graph file: {}.", ast_filename),
        Err(error) => println!("{}", error),
    };

    let semantic_errors = ast.generate_symbol_table();

    println!("Symbol table generation completed.");

    if !semantic_errors.is_empty() {
        print_errors(
            lexical_errors,
            syntactic_errors,
            semantic_errors,
            &mut error_file,
        );
        print_symbol_table(&ast, &mut symbol_table_file);
        return;
    }

    let semantic_warning_and_errors = ast.semantic_class_checker();
    let mut semantic_warnings = semantic_warning_and_errors.0;
    let semantic_errors = semantic_warning_and_errors.1;
    if !semantic_warnings.is_empty() {
        semantic_warnings.sort_by_key(TokenLocation::get_location);
        println!("Found {} warnings:", semantic_warnings.len());
        for semantic_warning in semantic_warnings {
            println!("{}", semantic_warning);
        }
    }

    println!("Semantic class checking completed.");

    if !semantic_errors.is_empty() {
        print_errors(
            lexical_errors,
            syntactic_errors,
            semantic_errors,
            &mut error_file,
        );
        print_symbol_table(&ast, &mut symbol_table_file);
        return;
    }

    let semantic_errors = ast.semantic_function_checker();

    println!("Semantic function checking completed.");

    if !semantic_errors.is_empty() {
        print_errors(
            lexical_errors,
            syntactic_errors,
            semantic_errors,
            &mut error_file,
        );
        print_symbol_table(&ast, &mut symbol_table_file);
        return;
    }

    let semantic_errors = ast.type_checker();

    println!("Type checking completed.");

    if !semantic_errors.is_empty() {
        print_errors(
            lexical_errors,
            syntactic_errors,
            semantic_errors,
            &mut error_file,
        );
        print_symbol_table(&ast, &mut symbol_table_file);
        return;
    }

    if !lexical_errors.is_empty() || !syntactic_errors.is_empty() {
        print_errors(
            lexical_errors,
            syntactic_errors,
            semantic_errors,
            &mut error_file,
        );
    } else {
        println!("Compilation completed without errors!");
        print_symbol_table(&ast, &mut symbol_table_file);
    }
}

enum Error {
    Lexical(TokenError),
    Syntactic(SyntacticError),
    Semantic(SemanticError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Error::*;
        match self {
            Lexical(token_error) => write!(f, "{}", token_error),
            Syntactic(syntactic_error) => write!(f, "{}", syntactic_error),
            Semantic(semantic_error) => write!(f, "{}", semantic_error),
        }
    }
}

impl TokenLocation for Error {
    fn get_location(&self) -> Location {
        use Error::*;
        match self {
            Lexical(token_error) => token_error.get_location(),
            Syntactic(syntactic_error) => syntactic_error.get_location(),
            Semantic(semantic_error) => semantic_error.get_location(),
        }
    }
}

fn print_errors(
    lexical_errors: Vec<TokenError>,
    syntactic_errors: Vec<SyntacticError>,
    semantic_errors: Vec<SemanticError>,
    error_file: &mut File,
) {
    use Error::*;
    let mut errors = Vec::new();
    errors.append(
        lexical_errors
            .into_iter()
            .map(Lexical)
            .collect::<Vec<Error>>()
            .as_mut(),
    );
    errors.append(
        syntactic_errors
            .into_iter()
            .map(Syntactic)
            .collect::<Vec<Error>>()
            .as_mut(),
    );
    errors.append(
        semantic_errors
            .into_iter()
            .map(Semantic)
            .collect::<Vec<Error>>()
            .as_mut(),
    );
    errors.sort_by_key(TokenLocation::get_location);
    println!("Found {} errors:", errors.len());
    for error in errors {
        println!("{}", error);
        error_file
            .write_fmt(format_args!("{}\n", error))
            .expect("Could not write to error file.");
    }
}

fn print_symbol_table(
    ast: &tree::Tree<ast_node::NodeElement, symbol_table::SymbolTableArena>,
    symbol_table_filename: &mut File,
) {
    symbol_table_filename
        .write_fmt(format_args!("{} ", ast.symbol_table_arena.print()))
        .expect("Could not write to symbol table file.");
}
