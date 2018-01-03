
use std::fmt::Debug;
use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate nom;
extern crate clap;
use clap::{ App, Arg };

mod parser;
use parser::AST;

mod scope;
use scope::ScopeMapRef;

mod types;
mod debug;
mod utils;
mod interpreter;
//mod compiler_c;
mod compiler_llvm;
mod lib_llvm;

fn main() {
    let matches =
        App::new("rustytea")
            .version("0.1")
            .about("A toy ML compiler")
            .arg(Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1))
            .arg(Arg::with_name("compile")
                .short("c")
                .help("Compiles to C rather than executing"))
            .get_matches();

    let filename = matches.value_of("INPUT").unwrap();
    match matches.occurrences_of("compile") {
        0 => execute_file(filename),
        1 => compile_file(filename),
        _ => println!("Invalid argument: -c"),
    }
}

fn execute_file(filename: &str) {
    let mut f = File::open(filename).expect("Error: file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");
    execute_string(contents.as_bytes())
}

fn execute_string(text: &[u8]) {
    let map = interpreter::make_global();
    let code = process_input(map.clone(), text);

    let fin = interpreter::execute(map.clone(), &code);
    println!("{:?}", fin);
}

fn compile_file(filename: &str) {
    let mut f = File::open(filename).expect("Error: file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");
    compile_string(contents.as_bytes())
}

fn compile_string(text: &[u8]) {
    let map = lib_llvm::make_global();
    let code = process_input(map.clone(), text);

    let fin = compiler_llvm::compile(map.clone(), &code);
    println!("{}", fin);
}

fn process_input<V, T>(map: ScopeMapRef<V, T>, text: &[u8]) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    let result = parser::parse(text);
    println!("{:?}\n\n", result);
    let mut code = result.unwrap().1;
    traverse(&code);
    scope::bind_names(map.clone(), &mut code);
    let gtype = types::check_types(map.clone(), map.get_global(), &mut code);
    println!("\n{:?}\n", code);

    //println!("\n{:?}\n\n{:?}", &code, global.clone());
    debug::print_types(map.clone(), map.get_global(), &code);
    debug::print_types_scope(map.clone(), map.get_global());
    code
}

fn traverse(tree: &Vec<AST>) {
    for node in tree {
        println!("{:?}", node);
        match *node {
            AST::Block(ref x) => traverse(x),
            _ => ()
        }
    }
}


