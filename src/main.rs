
use std::str;

extern crate clap;
use clap::{ App, Arg };

#[macro_use]
extern crate nom;
#[macro_use(position)]
extern crate nom_locate;


#[macro_use]
mod debug;

#[macro_use]
mod parser;

mod abi;
mod ast;
mod scope;
mod types;
mod utils;
mod config;
mod session;
mod refinery;
mod binding;
mod typecheck;
mod precompiler;
mod export;
mod llvm;

use config::Options;
use llvm::compiler;
use llvm::lib;

fn main() {
    let matches =
        App::new("molten")
            .version("0.1")
            .about("a compiler for the molten language")
            .arg(Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1))
            .arg(Arg::with_name("output")
                .short("o")
                .value_name("OUTPUT")
                .takes_value(true)
                .help("Sets the output file"))
            .arg(Arg::with_name("compile")
                .short("c")
                .help("Compiles rather than executing"))
            .arg(Arg::with_name("library")
                .short("l")
                .help("Compiles as a library, without a main function"))
            .arg(Arg::with_name("debug")
                .short("d")
                .help("Enables debug logging"))
            .get_matches();

    Options::init();
    Options::as_ref().debug = matches.occurrences_of("debug") > 0;
    Options::as_ref().is_library = matches.occurrences_of("library") > 0;

    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("output");
    if matches.occurrences_of("compile") > 0 {
        compile_file(input, output);
    } else {
        println!("Use the -c flag to compile");
    }
}

fn compile_file(input: &str, output: Option<&str>) {
    let mut session = session::Session::new();
    let name = input.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    session.target = output.map(|s| String::from(s)).unwrap_or_else(|| format!("{}.ll", name));

    let builtins = lib::get_builtins();
    lib::make_global(&session.map, &builtins);

    let mut code = session.parse_file(input, false);
    //code = refinery::refine(code);
    binding::bind_names(&session, &mut code);
    typecheck::check_types(&session, session.map.get_global(), &mut code);

    if Options::as_ref().debug {
        let global = session.map.get_global();
        println!("\n{:?}\n", code);
        //println!("\n{:?}\n\n{:?}", &code, global);
        debug::print_types(&session.map, global, &code);
        debug::print_types_scope(global);
    }

    code = precompiler::precompile(&session, code);

    compiler::compile(&builtins, &session, name, &mut code);
}



