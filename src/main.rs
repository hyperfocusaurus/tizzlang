
use std::fs::File;
use std::env;
use std::io::prelude::*;

mod lexer;
use lexer::Lexer;
mod parser;
use parser::Parser;
mod compiler;
use compiler::Compiler;

fn print_parse_errors(errors: Vec<String>) {
    println!("parser errors:");
    for error in errors {
        println!("\t{}", error);
    }
}

fn print_compiler_errors(errors: Vec<String>) {
    println!("compiler errors:");
    for error in errors {
        println!("\t{}", error);
    }
}

fn main() -> Result<(), ()> {
    // todo: proper argument parsing
    let input_filename = env::args().nth(1).unwrap();
    let output_filename = "a.out";
    let mut input_file = File::open(input_filename).unwrap();
    let mut output_file = File::create(output_filename).unwrap();
    let mut input = String::new();
    input_file.read_to_string(&mut input).unwrap();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if parser.errors.len() != 0 {
        print_parse_errors(parser.errors);
        return Err(());
    }
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(program);
    if compiler.errors.len() != 0 {
        print_compiler_errors(compiler.errors);
        return Err(());
    }
    output_file.write_all(bytecode.as_slice()).unwrap();
    Ok(())
}
