mod lexer;
use std::fs;

use lexer::Lexer;

fn main() {
    let source_code = fs::read_to_string("test/test.pr").expect("File did not exist");
    let mut lexer = Lexer::new(&source_code);
    let token_steam = lexer.tokenize();
    println!("{:?}", token_steam);
}
