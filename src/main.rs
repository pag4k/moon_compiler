mod ast_node;
mod ast_visitor;
mod class_checker_visitor;
mod code_generation_common;
mod code_generation_error;
mod dot_generator;
mod finite_accepter;
mod function_checker_visitor;
mod grammar;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod lexical_error;
mod nfa_generator;
mod semantic_analysis_common;
//mod semantic_class_checker;
mod semantic_error;
mod table;
mod type_checker_visitor;
//mod semantic_function_checker;
mod memory_table;
mod symbol_table;
//mod symbol_table_generator;
mod code_generator_visitor;
mod memory_table_generator_visitor;
mod register_pool;
mod syntactic_analyzer;
mod syntactic_analyzer_table;
mod syntactic_error;
mod table_generator_visitor;
mod tree;
mod tree_dot_printer;

use ast_node::*;
use class_checker_visitor::*;
use code_generator_visitor::*;
use function_checker_visitor::*;
use grammar::*;
use language::*;
use lexical_analyzer::*;
use memory_table_generator_visitor::*;
use semantic_error::*;
use std::env;
use std::fs;
use std::fs::File;
// use std::io::prelude::*;
use std::path::Path;
use syntactic_analyzer::*;
use syntactic_error::*;
use table_generator_visitor::*;
use type_checker_visitor::*;

use std::fmt::{Display, Formatter};
use std::io::{self, Write};
use std::process::Command;

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

    compile(&source);
}

#[allow(clippy::large_enum_variant)]
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

fn combine_errors(
    lexical_errors: Vec<TokenError>,
    syntactic_errors: Vec<SyntacticError>,
    semantic_errors: Vec<SemanticError>,
) -> Vec<Error> {
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
    errors
}

fn print_errors(errors: &[Error], error_file: &mut File, warning_count: usize) {
    println!(
        "Found {} warning(s) and {} error(s):",
        warning_count,
        errors.len() - warning_count
    );
    for error in errors {
        println!("{}", error);
        error_file
            .write_fmt(format_args!("{}\n", error))
            .expect("Could not write to error file.");
    }
}

fn print_symbol_table(ast: &AST, symbol_table_file: &mut File) {
    symbol_table_file
        .write_fmt(format_args!("{} ", ast.symbol_table_arena.print()))
        .expect("Could not write to symbol table file.");
}

fn print_memory_table(ast: &AST, memory_table_file: &mut File) {
    memory_table_file
        .write_fmt(format_args!("{} ", ast.memory_table_arena.print()))
        .expect("Could not write to symbol table file.");
}

fn print_moon_code(code: &[String], moon_code_file: &mut File) {
    for line in code {
        moon_code_file
            .write_fmt(format_args!("{}\n", line))
            .expect("Could not write to moon code file.");
    }
}

fn compile(source: &str) -> (Option<Vec<Error>>, Option<bool>) {
    let atocc_filename = "atocc.txt";
    let path = Path::new(atocc_filename);
    let mut atocc_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating AtoCC file: {}. Exiting...",
                atocc_filename
            );
            return (None, None);
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
            return (None, None);
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
            return (None, None);
        }
    };

    let memory_table_filename = "memory_table.txt";
    let path = Path::new(memory_table_filename);
    let mut memory_table_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating memory table file: {}. Exiting...",
                memory_table_filename
            );
            return (None, None);
        }
    };

    let moon_code_filename = "moon_code.txt";
    let path = Path::new(moon_code_filename);
    let mut moon_code_file = match File::create(&path) {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong creating moon code file: {}. Exiting...",
                moon_code_filename
            );
            return (None, None);
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
        return (None, None);
    } else {
        println!("Lexical analysis completed.");
    }

    let grammar_filename = "grammar.txt";
    let grammar_source = match fs::read_to_string("grammar.txt") {
        Ok(file) => file,
        Err(_) => {
            println!(
                "ERROR: Something went wrong reading grammar file: {}. Exiting...",
                grammar_filename
            );
            return (None, None);
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
            return (None, None);
        }
    };

    let syntactic_analyzer = match SyntacticAnalyzer::from_grammar(grammar) {
        Ok(syntactic_analyzer) => {
            println!("Parsing table generated succesfully from LL(1) grammar!");
            syntactic_analyzer
        }
        Err(error) => {
            println!("{}", error);
            return (None, None);
        }
    };

    let (mut ast, syntactic_errors) = match syntactic_analyzer.parse(tokens) {
        Ok((ast, mut _derivation_table, syntactic_errors)) => {
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
            return (None, None);
        }
    };

    println!("Syntactic analysis completed.");

    if syntactic_errors
        .iter()
        .any(|syntactic_error| syntactic_error.failed_to_recover())
    {
        let errors = combine_errors(lexical_errors, syntactic_errors, Vec::new());
        print_errors(&errors, &mut error_file, 0);
        println!("ERROR: One error could not be recovered. Exiting...",);
        return (Some(errors), None);
    }

    let ast_filename = "ast.gv";
    match ast.print_tree_to(ast_filename) {
        Ok(()) => println!("Succesfully generated AST graph file: {}.", ast_filename),
        Err(error) => println!("{}", error),
    };

    let mut semantic_errors = table_generator_visitor(&mut ast);

    println!("Symbol table generation completed.");

    semantic_errors.append(&mut class_checker_visitor(&mut ast));
    println!("Semantic class checking completed.");

    semantic_errors.append(&mut function_checker_visitor(&mut ast));

    println!("Semantic function checking completed.");

    semantic_errors.append(&mut type_checker_visitor(&mut ast));
    println!("Type checking completed.");

    if !lexical_errors.is_empty() || !syntactic_errors.is_empty() || !semantic_errors.is_empty() {
        let warning_count = semantic_errors
            .iter()
            .filter(|semantic_error| semantic_error.is_warning())
            .count();
        let errors = combine_errors(lexical_errors, syntactic_errors, semantic_errors);
        print_errors(&errors, &mut error_file, warning_count);
        print_symbol_table(&ast, &mut symbol_table_file);
        if warning_count != errors.len() {
            return (Some(errors), None);
        }
    }

    println!("Compilation completed without errors!");
    print_symbol_table(&ast, &mut symbol_table_file);

    // Traverse the AST untill all classes have been sized.
    while !memory_table_generator_visitor(&mut ast).is_empty() {}

    print_memory_table(&ast, &mut memory_table_file);

    let moon_code = code_generator_visitor(&mut ast);

    print_moon_code(&moon_code, &mut moon_code_file);

    println!("Executing program:");

    let output = Command::new("./moon")
        .arg(moon_code_filename)
        .arg("lib.m")
        .output()
        .expect("failed to execute process");
    println!("----------------------------------------");
    println!("Status: {}", output.status);
    println!("----------------------------------------");
    println!("stdout");
    println!("----------------------------------------");
    io::stdout().write_all(&output.stdout).unwrap();
    println!("----------------------------------------");
    println!("stderr");
    println!("----------------------------------------");
    io::stderr().write_all(&output.stderr).unwrap();
    println!("----------------------------------------");

    // The moon processor only uses stderr if there is a code error.
    // Not if there is a runtime error or if the output is wrong.
    (None, Some(output.status.success()))
}

#[cfg(test)]
mod tests {
    use std::fs;

    #[test]
    fn lexical_analyzer() {
        let source_files: Vec<(&str, usize)> = vec![("lexical_error1.txt", 26)];

        for (source_filename, error_count) in source_files.iter() {
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
            println!();
            println!("Compiling file: {}", source_filename);
            match crate::compile(&source) {
                (Some(errors), None) => assert!(*error_count == errors.len()),
                (None, Some(is_success)) => {
                    assert!(*error_count == 0);
                    assert!(is_success);
                }
                (Some(_), Some(_)) => unreachable!(),
                (None, None) => assert!(*error_count == 0),
            }
        }
    }

    #[test]
    fn syntactic_analyzer() {
        let source_files: Vec<(&str, usize)> =
            vec![("syntactic_error1.txt", 5), ("syntactic_error2.txt", 8)];

        for (source_filename, error_count) in source_files.iter() {
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
            println!();
            println!("Compiling file: {}", source_filename);
            match crate::compile(&source) {
                (Some(errors), None) => assert!(*error_count == errors.len()),
                (None, Some(is_success)) => {
                    assert!(*error_count == 0);
                    assert!(is_success);
                }
                (Some(_), Some(_)) => unreachable!(),
                (None, None) => assert!(*error_count == 0),
            }
        }
    }

    #[test]
    fn semantic_analyzer() {
        let source_files: Vec<(&str, usize)> = vec![
            ("table_generation_error1.txt", 4),
            ("table_generation_error2.txt", 1),
            ("table_generation_error3.txt", 2),
            ("class_checker_error1.txt", 3),
            ("class_checker_error2.txt", 1),
            ("class_checker_error3.txt", 1),
            ("class_checker_error4.txt", 2),
            ("function_checker_error1.txt", 8),
            ("type_checker_error1.txt", 11),
            ("combined_error1.txt", 9),
        ];

        for (source_filename, error_count) in source_files.iter() {
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
            println!();
            println!("Compiling file: {}", source_filename);
            match crate::compile(&source) {
                (Some(errors), None) => assert!(*error_count == errors.len()),
                (None, Some(is_success)) => {
                    assert!(*error_count == 0);
                    assert!(is_success);
                }
                (Some(_), Some(_)) => unreachable!(),
                (None, None) => assert!(*error_count == 0),
            }
        }
    }
}
