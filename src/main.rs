
use std::str;

extern crate rand;
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
mod defs;
mod export;
mod llvm;
mod llvm2;

use config::Options;
use llvm::transform;
use llvm::codegen;
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
    let source = input.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    session.name = source.replace("/", ".");
    session.target = output.map(|s| String::from(s)).unwrap_or_else(|| String::from(source));

    //let builtins = lib::get_builtins();
    //lib::make_global(&session, &builtins);
    let builtins = llvm2::lib::get_builtins();
    llvm2::lib::make_global(&session, &builtins);

    let mut code = session.parse_file(input, false);
    binding::bind_names(&session, session.map.get_global(), &mut code);
    typecheck::check_types(&session, session.map.get_global(), &code);

    if Options::as_ref().debug {
        let global = session.map.get_global();
        println!("\n{:#?}\n", code);
        //println!("\n{:?}\n\n{:?}", &code, global);
        debug::print_types(&session, global.clone(), &code);
        debug::print_types_scope(&session, global);
    }

    session.resolve_types();

    export::write_exports(&session, session.map.get_global(), format!("{}.dec", session.target).as_str(), &code);

/*
    let transform = transform::Transform::new(&session);
    transform.transform_program(session.map.get_global(), &code);
    //println!("TRANSFORM:\n{:#?}", &*transform.toplevel.borrow());
    codegen::generate(&builtins, &session, &*transform.toplevel.borrow());

    if Options::as_ref().debug {
        for (ref id, ref ttype) in session.types.borrow().iter() {
            println!("{:?} -> {:?}", id, ttype);
        }
    }
*/

    use ast::{ NodeID };
    use llvm2::llcode::{ r, LLType, LLLit, LLExpr, LLGlobal, LLABI };
    use llvm2::transform::{ Transformer };
    use llvm2::codegen::{ LLVM };

    let transformer = Transformer::new(&session);
    transformer.transform_code(session.map.get_global(), &code);
    if Options::as_ref().debug {
        println!("===================");
        println!("{:#?}", &transformer.globals.borrow());
        println!("===================");
    }

    unsafe {
        let llvm = LLVM::new(&session);
        //llvm.build_expr(
        //    LLExpr::Literal(LLLit::I32(4))
        //);
        llvm2::lib::initialize_builtins(&llvm, &transformer, session.map.get_global(), &builtins);
        llvm.build_module(&transformer.globals.borrow());
        llvm.print_module();
        llvm.write_module(format!("{}.ll", session.target).as_str());
    }
}



