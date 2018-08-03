
use std::fmt::Debug;

use abi::ABI;
use session::Session;
use types::{ Type, resolve_type };
use typecheck::{ self, update_scope_variable_types };
use ast::{ Argument, ClassSpec, AST };
use scope::{ Scope, ScopeRef };
use utils::UniqueID;


pub fn precompile<'sess>(session: &'sess Session, code: Vec<AST>) -> Vec<AST> {
    update_scope_variable_types(session.map.get_global());
    precompile_vec(session, session.map.get_global(), code)
}

pub fn precompile_vec<'sess>(session: &'sess Session, scope: ScopeRef, code: Vec<AST>) -> Vec<AST> {
    let mut block = vec!();
    for node in code {
        block.push(precompile_node(session, scope.clone(), node));
    }
    block
}

pub fn precompile_node<'sess>(session: &'sess Session, scope: ScopeRef, node: AST) -> AST {
    match node {
        AST::Block(pos, code) => { AST::Block(pos, precompile_vec(session, scope, code)) },

        AST::Definition(pos, (dpos, name, ttype), code) => {
            AST::Definition(pos, (dpos, name, Some(resolve_type(scope.clone(), ttype.unwrap()))), Box::new(precompile_node(session, scope.clone(), *code)))
        },

        AST::Declare(pos, name, ttype) => {
            AST::Declare(pos, name, resolve_type(scope, ttype))
        },

        AST::Function(pos, mut name, mut args, ret, body, id, abi) => {
            let fscope = session.map.get(&id);
            update_scope_variable_types(fscope.clone());

            // Mangle names of overloaded functions
            /*
            if let Some(sname) = name.clone() {
                let dscope = Scope::target(scope);
                /*
                let mut ftype = dscope.get_variable_type(&sname).unwrap();
                if ftype.is_overloaded() {
                    ftype = types::find_variant(scope, ftype, args.iter().map(|t| t.ident.clone().unwrap()).collect(), Check::Def).unwrap();
                }
                debug!("***: {:?} {:?}", sname, ftype);
                let argtypes = ftype.get_argtypes().unwrap();
                let abi = ftype.get_abi().unwrap();
                */
                let argtypes = args.iter().map(|t| t.ident.clone().unwrap()).collect();
                let fname = abi.mangle_name(sname.as_str(), &argtypes, dscope.num_funcdefs(&sname));
                if !dscope.contains(&fname) {
                    dscope.define(fname.clone(), Some(Type::Function(argtypes.clone(), Box::new(ret.clone().unwrap()), abi))).unwrap();
                    name = Some(fname);
                }
            }
            */

            /*
            if abi == ABI::Molten {
                let mut code = vec!();

                let cid = UniqueID::generate();
                let tscope = session.map.add(cid, Some(scope.clone()));
                tscope.set_class(true);
                tscope.set_basename(format!("closure{}", cid));

                // Create class type named __closure__
                let cpair = (String::from(format!("__closure{}__", cid)), vec!());
                let classdef = scope.create_class_def(&cpair, None).unwrap();
                let ctype = Type::make_object(cpair.clone());
                tscope.define_type(String::from("Self"), ctype.clone());

                // Add closure context argument to function definition
                args.push(Argument::new(pos.clone(), Ident::from_str("__context__"), Some(ctype.clone()), None));
                let ftype = Type::Function(args.iter().map(|t| t.ident.clone().unwrap()).collect(), Box::new(ret.clone().unwrap()), abi);
                fscope.define(String::from("__context__"), Some(ctype.clone()));
                let body = precompile_node(session, fscope.clone(), *body);

                // Create the definition of the closure context class
                let mut classbody = vec!();
                classbody.push(AST::Definition(pos.clone(), (String::from("__func__"), Some(ftype.clone())), Box::new(AST::Underscore)));
                for (name, sym) in classdef.names.borrow() {
                    classbody.push(AST::Definition(pos.clone(), (name.clone(), sym.ttype.clone()), Box::new(AST::Underscore)));
                }
                code.push(AST::Class(pos.clone(), cpair.clone(), None, classbody, cid));

                // Create an instance of the class and assign the current values to its members
                let cref = format!("__closure{}__", cid);
                scope.define(cref.clone(), Some(ctype.clone()));
                code.push(AST::Definition(pos.clone(), (cref.clone(), Some(ctype.clone())), Box::new(AST::New(pos.clone(), cpair.clone()))));
                for (name, sym) in classdef.names.borrow() {
                    code.push(AST::Assignment(pos.clone(), Box::new(AST::Accessor(pos.clone(), Box::new(AST::Identifier(pos.clone(), cref.clone())), name.clone(), Some(ctype.clone()))), Box::new(AST::Identifier(pos.clone(), name.clone()))));
                }

                // Assign the function itself to the context class
                code.push(AST::Assignment(pos.clone(),
                    Box::new(AST::Accessor(pos.clone(), Box::new(AST::Identifier(pos.clone(), cref.clone())), String::from("__func__"), Some(ctype.clone()))),
                    Box::new(AST::Function(pos.clone(), name, args, ret, Box::new(body), id, abi))
                ));
                code.push(AST::Identifier(pos.clone(), cref.clone()));

                // Return the whole block of AST in place of the original function definition
                AST::Block(pos.clone(), code)
            } else {
            */
            let args = args.into_iter().map(|arg| Argument::new(arg.pos, arg.ident, arg.ttype.map(|atype| resolve_type(fscope.clone(), atype)), arg.default)).collect();
            AST::Function(pos, name, args, Some(resolve_type(fscope.clone(), ret.unwrap())), Box::new(precompile_node(session, fscope.clone(), *body)), id, abi)
            //}
        },

        AST::Invoke(pos, mut fexpr, args, stype) => {
            //if stype.as_ref().unwrap().get_abi().unwrap() == ABI::Molten {
            //    args.insert(0, precompile_node(session, scope.clone() *fexpr));
            //}

            // Mangle names of overloaded functions
            //typecheck::get_accessor_name(scope.clone(), &mut fexpr.as_mut(), stype.as_ref().unwrap()).unwrap();

            AST::Invoke(pos, Box::new(precompile_node(session, scope.clone(), *fexpr)), precompile_vec(session, scope.clone(), args), Some(resolve_type(scope.clone(), stype.unwrap())))
        },

        AST::Recall(_, _) => node,

        AST::Identifier(pos, name) => {
            /*
            if scope.is_closure_var(&name) && scope.contains(&String::from("__context__")) {
                let ctype = scope.get_variable_type(&String::from("__context__")).unwrap();
                let classdef = scope.get_class_def(&ctype.get_name().unwrap());
                let ttype = scope.get_variable_type(&name);
                classdef.define(name.clone(), ttype);
                AST::Accessor(pos.clone(), Box::new(AST::Identifier(pos.clone(), String::from("__context__"))), name, scope.get_variable_type(&String::from("__context__")))
            } else {
                AST::Identifier(pos, name)
            }
            */
            AST::Identifier(pos, name)
        },

        AST::SideEffect(pos, op, args) => {
            AST::SideEffect(pos, op, precompile_vec(session, scope, args))
        },

        AST::If(pos, cond, texpr, fexpr) => {
            AST::If(pos, Box::new(precompile_node(session, scope.clone(), *cond)), Box::new(precompile_node(session, scope.clone(), *texpr)), Box::new(precompile_node(session, scope.clone(), *fexpr)))
        },

        AST::Match(pos, cond, cases, condtype) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(session, scope.clone(), case), precompile_node(session, scope.clone(), body) )).collect();
            AST::Match(pos, Box::new(precompile_node(session, scope.clone(), *cond)), cases, condtype)
        },

        AST::Try(pos, cond, cases, condtype) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(session, scope.clone(), case), precompile_node(session, scope.clone(), body) )).collect();
            AST::Try(pos, Box::new(precompile_node(session, scope.clone(), *cond)), cases, condtype)
        },

        AST::Raise(pos, expr) => {
            AST::Raise(pos, Box::new(precompile_node(session, scope.clone(), *expr)))
        },

        AST::While(pos, cond, body) => {
            AST::While(pos, Box::new(precompile_node(session, scope.clone(), *cond)), Box::new(precompile_node(session, scope.clone(), *body)))
        },

        AST::For(pos, name, cond, body, id) => {
            let lscope = session.map.get(&id);
            AST::For(pos, name, Box::new(precompile_node(session, lscope.clone(), *cond)), Box::new(precompile_node(session, lscope.clone(), *body)), id)
        },

        AST::List(pos, code, ttype) => { AST::List(pos, precompile_vec(session, scope.clone(), code), ttype) },
        /*
        AST::List(pos, code, stype) => {
            use abi::ABI;
            use utils::UniqueID;
            let mut block = vec!();
            let itype = stype.unwrap();
            let ltype = Type::Object(format!("List"), vec!(itype.clone()));
            let id = format!("{}", UniqueID::generate());
            block.push(AST::Definition(pos.clone(), (id, Some(ltype.clone())),
                Box::new(AST::Invoke(pos.clone(),
                    Box::new(AST::Resolver(pos.clone(), Box::new(AST::Identifier(pos.clone(), format!("List"))), String::from("new"))),
                    vec!(AST::New(pos.clone(), (format!("List"), vec!(itype.clone()))) /*, AST::Integer(code.len() as isize)*/), Some(Type::Function(vec!(ltype.clone()), Box::new(ltype.clone()), ABI::Molten))))));
            for item in code {
                block.push(AST::Invoke(pos.clone(),
                    Box::new(AST::Accessor(pos.clone(), Box::new(AST::Identifier(pos.clone(), id)), String::from("push"), Some(ltype.clone()))),
                    vec!(AST::Identifier(pos.clone(), id), item), Some(Type::Function(vec!(ltype.clone()), Box::new(ltype.clone()), ABI::Molten))));
            }
            block.push(AST::Identifier(pos.clone(), id));
            AST::Block(pos.clone(), precompile_vec(session, scope.clone(), block))
        },
        */

        AST::PtrCast(_, _) => { node },
        AST::New(_, _) => { node },

        AST::Class(pos, classspec, parentspec, body, id) => {
            let tscope = session.map.get(&id);
            //update_scope_variable_types(tscope.clone());
            //let classdef = scope.get_class_def(&classspec.0);
            //update_scope_variable_types(classdef.clone());
            let mut body = precompile_vec(session, tscope.clone(), body);

            /*
            let initname = String::from("__init__");
            let objtype = Type::make_object(classspec.clone());
            if !classdef.contains_local(&initname) {
                let initid = UniqueID::generate();

                let mut initbody = vec!();
                if parentspec.is_some() {
                    //initbody.push(AST::Invoke(
                }
                for ref node in &body {
                    match **node {
                        AST::Definition((ref name, ref ttype), ref value) => {
                            initbody.push(AST::Assignment(Box::new(AST::Accessor(Box::new(AST::Identifier(String::from("object"))), name.clone(), Some(objtype.clone()))), value.clone()));
                        },
                        _ => { }
                    }
                }
                initbody.push(AST::Nil(Some(Type::Object(String::from("Nil"), vec!()))));

                let init = AST::Function(
                    Some(initname.clone()),
                    vec!((String::from("object"), Some(objtype.clone()), None)),
                    Some(Type::Object(String::from("Nil"), vec!())),
                    Box::new(AST::Block(initbody)),
                    initid
                );

                register_function(session, tscope.clone(), &init);
                body.insert(0, init);
            }
            */

            AST::Class(pos, classspec, parentspec, body, id)
        },


        AST::Resolver(pos, left, right) => {
            AST::Resolver(pos, Box::new(precompile_node(session, scope.clone(), *left)), right)
        },

        AST::Accessor(pos, left, right, stype) => {
            AST::Accessor(pos, Box::new(precompile_node(session, scope.clone(), *left)), right, Some(resolve_type(scope.clone(), stype.unwrap())))
        },

        AST::Assignment(pos, left, right) => {
            AST::Assignment(pos, Box::new(precompile_node(session, scope.clone(), *left)), Box::new(precompile_node(session, scope.clone(), *right)))
        },

        AST::Import(pos, name, decls) => {
            AST::Import(pos, name, precompile_vec(session, scope.clone(), decls))
        },

        AST::Nil(stype) => { AST::Nil(Some(resolve_type(scope.clone(), stype.unwrap()))) },

        AST::Noop => { node },
        AST::Underscore => { node },
        AST::Boolean(_) => { node },
        AST::Integer(_) => { node },
        AST::Real(_) => { node },
        AST::String(_) => { node },
        AST::Type(_, _, _) => { node },

        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    }
}

/*
pub fn register_function(session: &Session, scope: ScopeRef, function: &AST) {
    if let &AST::Function(ref name, ref args, ref ret, ref body, ref id) = function {
        let name = name.clone().unwrap();
        let fscope = session.map.add(id, Some(scope.clone()));
        fscope.set_basename(name.clone());

        for arg in args {
            fscope.define(arg.0.clone(), arg.1.clone());
        }

        let ftype = Type::Function(args.iter().map(|t| t.ident.clone().unwrap()).collect(), Box::new(ret.clone().unwrap()));
        let dscope = Scope::target(scope.clone());
        dscope.define(name, Some(ftype.clone()));
    }
}
*/

