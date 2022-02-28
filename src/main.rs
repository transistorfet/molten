
use std::str;

extern crate regex;
extern crate clap;
#[macro_use]
extern crate nom;
#[macro_use(position)]
extern crate nom_locate;

use regex::Regex;
use clap::{ App, Arg, ArgMatches };

#[macro_use]
mod debug;
mod abi;
mod misc;
mod scope;
mod types;
mod config;
mod session;
mod defs;
mod parsing;
mod analysis;
mod transform;
mod llvm;

use crate::parsing::refinery;
use crate::analysis::binding;
use crate::analysis::typecheck;
use crate::analysis::validate;
use crate::analysis::export;
use crate::config::{ Options, EmitAs };

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
            .arg(Arg::with_name("linkfile")
                .short("b")
                .conflicts_with("assemble")
                .conflicts_with("compile")
                .help("Produces the link file and then exits"))
            .arg(Arg::with_name("assemble")
                .short("S")
                .conflicts_with("linkfile")
                .conflicts_with("compile")
                .help("Compiles to an assembly file"))
            .arg(Arg::with_name("compile")
                .short("c")
                .conflicts_with("linkfile")
                .conflicts_with("assemble")
                .help("Compiles to an object file"))
            .arg(Arg::with_name("library")
                .short("l")
                .help("Compiles as a library, without a main function"))
            .arg(Arg::with_name("opt")
                .short("O")
                .takes_value(true)
                .help("Compiles as a library, without a main function"))
            .arg(Arg::with_name("debug")
                .short("d")
                .help("Enables debug logging"))
            .arg(Arg::with_name("no-gc")
                .short("G")
                .long("no-gc")
                .help("Disables garbage collection and uses malloc directly"))
            .get_matches();

    build_options(&matches);

    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("output");
    if matches.occurrences_of("linkfile") > 0 || matches.occurrences_of("assemble") > 0 || matches.occurrences_of("compile") > 0 {
        compile_file(input, output);
    } else {
        println!("Use the -c flag to compile");
    }
}

fn build_options(matches: &ArgMatches) {
    Options::init();
    Options::as_ref().debug = matches.occurrences_of("debug") > 0;
    Options::as_ref().is_library = matches.occurrences_of("library") > 0;
    Options::as_ref().no_gc = matches.occurrences_of("no-gc") > 0;
    Options::as_ref().linkfile_only = matches.occurrences_of("linkfile") > 0;

    Options::as_ref().format = if matches.occurrences_of("assemble") > 0 {
        EmitAs::LLIR
    } else {
        EmitAs::Obj
    };

    Options::as_ref().optlevel = matches.value_of("opt").map(|s| s.parse::<u32>().unwrap()).unwrap_or(0);
}

fn compile_file(input: &str, output: Option<&str>) {
    let mut session = session::Session::new();
    let source = input.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    let re = Regex::new(r"[^A-Za-z0-9_.]").unwrap();
    session.name = re.replace_all(&source.replace("/", "."), "_").to_string();
    session.target = output.map(String::from).unwrap_or_else(|| String::from(source));

    let builtins = llvm::lib::get_builtins();
    llvm::lib::make_global(&session, &builtins);

    let code = session.parse_file(input, false);
    let mut code = refinery::Refinery::refine(&session, code);
    session.write_link_file();
    if Options::as_ref().linkfile_only {
        return;
    }
    debug!("PARSED: {:#?}", code);

    binding::NameBinder::bind_names(&session, session.map.get_global(), &code);
    typecheck::TypeChecker::check(&session, session.map.get_global(), &code);

    if Options::as_ref().debug {
        let global = session.map.get_global();
        debug::print_types(&session, global.clone(), &code);
        debug::print_types_scope(&session, global);
    }

    session.resolve_types();

    validate::Validator::validate(&session, session.map.get_global(), &code);
    export::write_exports(&session, session.map.get_global(), &format!("{}.dec", session.target), &code);

    //closures::ClosureConversion::visit(&session, session.map.get_global(), &mut code);
    crate::transform::autopack::AutoPack::visit(&session, session.map.get_global(), &mut code);

    let mut transformer = transform::transform::Transformer::new(&session);
    transformer.initialize();
    transformer.transform_code(session.map.get_global(), &code);
    if Options::as_ref().debug {
        println!("===================");
        println!("{:#?}", &transformer.globals);
        println!("===================");
    }

    let llvm = llvm::codegen::LLVM::new(&session);
    llvm.initialize();
    llvm::lib::initialize_builtins(&llvm, &mut transformer, session.map.get_global(), &builtins);
    llvm.build_module(&transformer.globals);
    llvm.optimize(Options::as_ref().optlevel);
    llvm.print_module();

    match Options::as_ref().format {
        EmitAs::LLIR => llvm.write_module(&format!("{}.ll", session.target)),
        EmitAs::Obj => llvm.write_object_file(&format!("{}.o", session.target)),
    }
}



