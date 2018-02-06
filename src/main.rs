
use std::str;
use std::fs::File;
use std::fmt::Debug;
use std::io::prelude::*;

extern crate clap;
use clap::{ App, Arg };

#[macro_use]
extern crate nom;

mod config;
use config::Options;

#[macro_use]
mod parser;
use parser::AST;

mod refinery;

mod scope;
use scope::ScopeMapRef;

mod types;
mod debug;
mod utils;
mod import;
//mod interpreter;
//mod compiler_c;
mod compiler_llvm;
mod lib_llvm;

fn main() {
    let matches =
        App::new("molten")
            .version("0.1")
            .about("A toy ML compiler")
            .arg(Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1))
            .arg(Arg::with_name("compile")
                .short("c")
                .help("Compiles to C rather than executing"))
            .arg(Arg::with_name("library")
                .short("l")
                .help("Compiles as a library, without a main function"))
            .get_matches();

    //let mut options = Options::new();
    Options::init();
    Options::as_ref().is_library = matches.occurrences_of("library") > 0;

    let filename = matches.value_of("INPUT").unwrap();
    if matches.occurrences_of("compile") > 0 {
        with_file(filename, |name, text| compile_string(name, text));
    } else {
        println!("Use the -c flag to compile");
    }
}

fn with_file<F>(filename: &str, with: F) where F: Fn(&str, &[u8]) -> () {
    let name = filename.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    let mut f = File::open(filename).expect("Error: file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");
    with(name, contents.as_bytes())
}


fn compile_string(name: &str, text: &[u8]) {
    let builtins = lib_llvm::get_builtins();
    let map = lib_llvm::make_global(&builtins);
    let mut code = process_input(map.clone(), name, text);

    compiler_llvm::compile(&builtins, map.clone(), name, &mut code);
}


fn process_input<V, T>(map: ScopeMapRef<V, T>, name: &str, text: &[u8]) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    let mut code = parser::parse_or_error(name, text);
    code = refinery::refine(code);
    traverse(&code);
    //import::load_index(map.get_global(), "libcore.dec");
    scope::bind_names(map.clone(), &mut code);
    let gtype = types::check_types(map.clone(), map.get_global(), &mut code);
    println!("\n{:?}\n", code);

    //println!("\n{:?}\n\n{:?}", &code, global.clone());
    debug::print_types(map.clone(), map.get_global(), &code);
    debug::print_types_scope(map.get_global());
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


