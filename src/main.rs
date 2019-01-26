mod dot_generator;
mod finite_accepter;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod nfa_generator;

use lexical_analyzer::*;
use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    // Get the command line arguments.
    let args: Vec<String> = env::args().collect();

    // If there was a first argument, use it as source file, otherwise default.txt.
    let source_filename = if args.len() < 2 {
        "default.txt"
    } else {
        &args[1]
    };
    let source =
        fs::read_to_string(source_filename).expect("Something went wrong reading source file.");

    // If there was a second argument, use it as AtoCC file, otherwise atocc.txt.
    let atocc_filename = if args.len() < 3 {
        "atocc.txt"
    } else {
        &args[2]
    };
    let path = Path::new(atocc_filename);
    let mut atocc_file = File::create(&path).expect("Something went wrong reading AtoCC file.");

    // If there was a third argument, use it as error file, otherwise error.txt.
    let error_filename = if args.len() < 4 {
        "error.txt"
    } else {
        &args[3]
    };
    let path = Path::new(error_filename);
    let mut error_file = File::create(&path).expect("Something went wrong reading error file.");

    // Print the source file content.
    //println!("File content:\n{}", source);

    // Create LexicalAnalyzer and iterate over the tokens.
    let lexical_analyzer = LexicalAnalyzer::from_string(&source);
    for token in lexical_analyzer {
        match token {
            Ok(token) => {
                println!("{}", token);
                atocc_file
                    .write_fmt(format_args!("{} ", token.token_type))
                    .expect("Could not write to AtoCC file.");
            }
            Err(error) => {
                println!("{}", error);
                error_file
                    .write_fmt(format_args!("{}\n", error))
                    .expect("Could not write to error file.");
            }
        }
    }
}
