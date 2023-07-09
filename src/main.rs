mod lexer;
use std::{env, fs};

use lexer::Lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Please specify source file name");
    }

    // Read source
    let source_code = fs::read_to_string(&args[1]).expect("File not found");

    // Tokenize
    let token_stream = Lexer::new(&source_code).tokenize();

    // Debug
    println!("{}", source_code);
    for token in token_stream {
        println!("{}", token.to_string());
    }
}
