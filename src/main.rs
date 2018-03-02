
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

mod scope;
mod types;
mod utils;
mod config;
mod session;
mod refinery;
mod binding;
mod typecheck;
mod precompiler;
mod compiler_llvm;
mod lib_llvm;
mod export;

use config::Options;

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
            .arg(Arg::with_name("debug")
                .short("d")
                .help("Enables debug logging"))
            .get_matches();

    //let mut options = Options::new();
    Options::init();
    Options::as_ref().debug = matches.occurrences_of("debug") > 0;
    Options::as_ref().is_library = matches.occurrences_of("library") > 0;

    let filename = matches.value_of("INPUT").unwrap();
    if matches.occurrences_of("compile") > 0 {
        compile_file(filename);
    } else {
        println!("Use the -c flag to compile");
    }
}

fn compile_file(filename: &str) {
    let session = session::Session::new();
    let builtins = lib_llvm::get_builtins();
    lib_llvm::make_global(session.map.clone(), &builtins);

    let mut code = session.parse_file(filename);
    //code = refinery::refine(code);
    binding::bind_names(&session, &mut code);
    typecheck::check_types(&session, session.map.get_global(), &mut code);

    if Options::as_ref().debug {
        let global = session.map.get_global();
        println!("\n{:?}\n", code);
        //println!("\n{:?}\n\n{:?}", &code, global.clone());
        debug::print_types(session.map.clone(), global.clone(), &code);
        debug::print_types_scope(global);
    }

    code = precompiler::precompile(&session, code);

    let name = filename.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    compiler_llvm::compile(&builtins, &session, name, &mut code);
}



