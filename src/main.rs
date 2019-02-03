mod dot_generator;
mod finite_accepter;
mod grammar;
mod grammar_generator;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod nfa_generator;
mod syntactic_analyzer;
mod syntactic_analyzer_table;

use grammar::*;
use grammar_generator::*;
use language::*;
use lexical_analyzer::*;
use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use syntactic_analyzer::*;
use syntactic_analyzer_table::*;

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

    // If there was a second argument, use it as AtoCC file, otherwise atocc.txt.
    let atocc_filename = if args.len() < 3 {
        "atocc.txt"
    } else {
        &args[2]
    };
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

    // If there was a third argument, use it as error file, otherwise error.txt.
    let error_filename = if args.len() < 4 {
        "error.txt"
    } else {
        &args[3]
    };
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

    // Print the source file content.
    //println!("File content:\n{}", source);

    // Create LexicalAnalyzer and iterate over the tokens.
    let lexical_analyzer = LexicalAnalyzer::from_string(&source);
    // for token in lexical_analyzer {
    //     match token {
    //         Ok(token) => {
    //             println!("{}", token);
    //             atocc_file
    //                 .write_fmt(format_args!("{} ", token.token_type))
    //                 .expect("Could not write to AtoCC file.");
    //         }
    //         Err(error) => {
    //             println!("{}", error);
    //             error_file
    //                 .write_fmt(format_args!("{}\n", error))
    //                 .expect("Could not write to error file.");
    //         }
    //     }
    // }
    let tokens: Vec<TokenType> = lexical_analyzer
        .into_iter()
        .filter(|element| element.is_ok())
        .map(|token| token.unwrap().token_type)
        .collect();

    let grammar: ContextFreeGrammar<VariableType, TokenType> =
        ContextFreeGrammar::from_file("test.txt");

    let table = SyntacticAnalyzerTable::new(grammar);

    let syntactic_analyzer: SyntacticAnalyzer<VariableType, TokenType> =
        SyntacticAnalyzer { tokens, table };

    syntactic_analyzer.parse();
}
