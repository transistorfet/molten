
use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate nom;
extern crate clap;
use clap::{ App, Arg };

mod parser;
use parser::AST;

mod scope;
use scope::ScopeRef;

mod types;
mod debug;
mod interpreter;
mod compiler_c;

fn main() {
    let matches = App::new("rustytea")
                    .version("0.1")
                    .about("A toy ML compiler")
                    .arg(Arg::with_name("INPUT")
                        .help("Sets the input file to use")
                        .required(true)
                        .index(1))
                    .get_matches();

    let filename = matches.value_of("INPUT").unwrap();
    execute_file(filename);
}

fn execute_file(filename: &str) {
    let mut f = File::open(filename).expect("Error: file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");
    execute_string(contents.as_bytes())
}

fn process_input(text: &[u8]) -> (ScopeRef, Vec<AST>) {
    let result = parser::parse(text);
    println!("{:?}", result);
    let mut code = result.unwrap().1;
    let global = scope::bind_names(&mut code);
    let gtype = types::check_types(global.clone(), &mut code);

    //println!("\n{:?}\n\n{:?}", &code, global.clone());
    debug::print_types(global.clone(), &code);
    debug::print_types_scope(global.clone());
    (global, code)
}

fn execute_string(text: &[u8]) {
    let (global, code) = process_input(text);

    let fin = interpreter::execute(global.clone(), &code);
    println!("{:?}", fin);
}

fn compile_string(text: &[u8]) {
    let (global, code) = process_input(text);

    let fin = compiler_c::compile(global.clone(), &code);
    println!("{}", fin);
}

fn traverse(tree: Vec<AST>) {
    for node in tree {
        println!("{:?}", node);
        match node {
            AST::Block(x) => traverse(x),
            _ => ()
        }
    }
}


