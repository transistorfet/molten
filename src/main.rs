
use std::str;
use std::fs::File;
use std::fmt::Debug;
use std::io::prelude::*;

extern crate clap;
use clap::{ App, Arg };

#[macro_use]
extern crate nom;

mod parser;
use parser::AST;

//mod validator;

mod scope;
use scope::ScopeMapRef;

mod types;
mod debug;
mod utils;
mod import;
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
        0 => with_file(filename, |name, text| execute_string(name, text)),
        1 => with_file(filename, |name, text| compile_string(name, text)),
        _ => println!("Invalid argument: -c"),
    }
}

fn with_file<F>(filename: &str, with: F) where F: Fn(&str, &[u8]) -> () {
    let mut f = File::open(filename).expect("Error: file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");
    with(filename, contents.as_bytes())
}


fn execute_string(name: &str, text: &[u8]) {
    let map = interpreter::make_global();
    let code = process_input(map.clone(), name, text);

    let fin = interpreter::execute(map.clone(), &code);
    println!("{:?}", fin);
}

fn compile_string(name: &str, text: &[u8]) {
    let map = lib_llvm::make_global();
    let mut code = process_input(map.clone(), name, text);

    compiler_llvm::compile(map.clone(), name, &mut code);
}


fn process_input<V, T>(map: ScopeMapRef<V, T>, name: &str, text: &[u8]) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    let mut code = parser::parse_or_error(name, text);
    //validator::validate(&mut code);
    traverse(&code);
    //import::load_index(map.get_global(), "libcore.idx");
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


