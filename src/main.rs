mod finite_accepter;
mod language;
mod lexical_analyzer;
mod lexical_analyzer_table;
mod nfa_generator;

//use std::env;
//use std::fs;

fn main() {
    //let args: Vec<String> = env::args().collect();

    // if args.len() < 2 {
    //     panic!("Need to specify filename.");
    // }
    //
    // let filename = &args[1];
    //
    // let source = fs::read_to_string(filename).expect("Something went wrong reading tokenthe file.");

    let source =
        String::from("Teést é<><= == if ][][][][]{}{}{}{][][[[[]]()()9)()()]]}\n// \n \n 000000555 \nReed//sdfsfs\nfora\n&& || & | ");
    println!("SOURCE {}", source);

    {
        let lexical_analyzer = lexical_analyzer::LexicalAnalyzer::from_string(&source);
        let tokens: Vec<Result<lexical_analyzer::Token, lexical_analyzer::Error>> =
            lexical_analyzer.into_iter().collect();
        for token in tokens {
            match token {
                Ok(token) => println!("{}", token),
                Err(error) => println!("{}", error),
            }
        }
    }
}
