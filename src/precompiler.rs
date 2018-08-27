

use session::Session;
use types::{ resolve_type };
use typecheck::{ update_scope_variable_types };
use ast::{ Literal, Argument, AST };
use scope::{ Scope, ScopeRef };


pub fn precompile(session: &Session, code: Vec<AST>) -> Vec<AST> {
    update_scope_variable_types(session, session.map.get_global());
    precompile_vec(session, session.map.get_global(), code)
}

pub fn precompile_vec(session: &Session, scope: ScopeRef, code: Vec<AST>) -> Vec<AST> {
    let mut block = vec!();
    for node in code {
        block.push(precompile_node(session, scope.clone(), node));
    }
    block
}

pub fn precompile_node(session: &Session, scope: ScopeRef, node: AST) -> AST {
    match node {
        AST::Block(id, pos, code) => { AST::Block(id, pos, precompile_vec(session, scope, code)) },

        AST::Definition(id, pos, name, ttype, code) => {
let dscope = Scope::target(session, scope.clone());
debug!("{} {} -> {:?} {:?}", scope.get_basename(), name.name, dscope.get_var_def(&name.name), dscope.find_var_def(session, &name.name));
            AST::Definition(id, pos, name, Some(resolve_type(session, scope.clone(), ttype.unwrap())), Box::new(precompile_node(session, scope.clone(), *code)))
        },

        AST::Declare(id, pos, name, ttype) => {
let dscope = Scope::target(session, scope.clone());
debug!("{} {} -> {:?} {:?}", scope.get_basename(), name.name, dscope.get_var_def(&name.name), dscope.find_var_def(session, &name.name));
            AST::Declare(id, pos, name, resolve_type(session, scope, ttype))
        },

        AST::Function(id, pos, name, args, _, body, abi) => {
            let fscope = session.map.get(&id);
let dscope = Scope::target(session, scope.clone());
if name.is_some() {
debug!("{} {} -> {:?} {:?}", scope.get_basename(), name.as_ref().unwrap().name, dscope.get_var_def(&name.as_ref().unwrap().name), dscope.find_var_def(session, &name.as_ref().unwrap().name));
}
            update_scope_variable_types(session, fscope.clone());

            // Mangle names of overloaded functions
            /*
            if let Some(sname) = name.clone() {
                let dscope = Scope::target(session, scope);
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
                let ctype = Type::from_spec(cpair.clone());
                tscope.define_type(String::from("Self"), ctype.clone());

                // Add closure context argument to function definition
                args.push(Argument::new(pos.clone(), Ident::from_str("__context__"), Some(ctype.clone()), None));
                let ftype = Type::Function(Box::new(Type::Tuple(args.iter().map(|t| t.ident.clone().unwrap()).collect()), Box::new(ret.clone().unwrap()), abi);
                fscope.define(String::from("__context__"), Some(ctype.clone()));
                let body = precompile_node(session, fscope.clone(), *body);

                // Create the definition of the closure context class
                let mut classbody = vec!();
                classbody.push(AST::Definition(id, pos.clone(), (String::from("__func__"), Some(ftype.clone())), Box::new(AST::Underscore)));
                for (name, sym) in classdef.names.borrow() {
                    classbody.push(AST::Definition(id, pos.clone(), (name.clone(), sym.ttype.clone()), Box::new(AST::Underscore)));
                }
                code.push(AST::Class(id, pos.clone(), cpair.clone(), None, classbody, cid));

                // Create an instance of the class and assign the current values to its members
                let cref = format!("__closure{}__", cid);
                scope.define(cref.clone(), Some(ctype.clone()));
                code.push(AST::Definition(id, pos.clone(), (cref.clone(), Some(ctype.clone())), Box::new(AST::New(id, pos.clone(), cpair.clone()))));
                for (name, sym) in classdef.names.borrow() {
                    code.push(AST::Assignment(id, pos.clone(), Box::new(AST::Accessor(id, pos.clone(), Box::new(AST::Identifier(id, pos.clone(), cref.clone())), name.clone(), Some(ctype.clone()))), Box::new(AST::Identifier(id, pos.clone(), name.clone()))));
                }

                // Assign the function itself to the context class
                code.push(AST::Assignment(id, pos.clone(),
                    Box::new(AST::Accessor(id, pos.clone(), Box::new(AST::Identifier(id, pos.clone(), cref.clone())), String::from("__func__"), Some(ctype.clone()))),
                    Box::new(AST::Function(id, pos.clone(), name, args, ret, Box::new(body), id, abi))
                ));
                code.push(AST::Identifier(id, pos.clone(), cref.clone()));

                // Return the whole block of AST in place of the original function definition
                AST::Block(id, pos.clone(), code)
            } else {
            */
            //let args = args.into_iter().map(|arg| Argument::new(arg.pos, arg.ident, arg.ttype.map(|atype| resolve_type(session, fscope.clone(), atype)), arg.default)).collect();
            AST::Function(id, pos, name, args, None, Box::new(precompile_node(session, fscope.clone(), *body)), abi)
            //}
        },

        AST::Invoke(id, pos, fexpr, args) => {
            //if stype.as_ref().unwrap().get_abi().unwrap() == ABI::Molten {
            //    args.insert(0, precompile_node(session, scope.clone() *fexpr));
            //}

            // Mangle names of overloaded functions
            //typecheck::get_accessor_name(scope.clone(), &mut fexpr.as_mut(), stype.as_ref().unwrap()).unwrap();

            AST::Invoke(id, pos, Box::new(precompile_node(session, scope.clone(), *fexpr)), precompile_vec(session, scope.clone(), args))
        },

        AST::Recall(_, _) => node,

        AST::Identifier(id, pos, name) => {
            /*
            if scope.is_closure_var(&name) && scope.contains(&String::from("__context__")) {
                let ctype = scope.get_variable_type(&String::from("__context__")).unwrap();
                let classdef = scope.get_class_def(&ctype.get_name().unwrap());
                let ttype = scope.get_variable_type(&name);
                classdef.define(name.clone(), ttype);
                AST::Accessor(id, pos.clone(), Box::new(AST::Identifier(id, pos.clone(), String::from("__context__"))), name, scope.get_variable_type(&String::from("__context__")))
            } else {
                AST::Identifier(id, pos, name)
            }
            */
            AST::Identifier(id, pos, name)
        },

        AST::SideEffect(id, pos, op, args) => {
            AST::SideEffect(id, pos, op, precompile_vec(session, scope, args))
        },

        AST::If(id, pos, cond, texpr, fexpr) => {
            AST::If(id, pos, Box::new(precompile_node(session, scope.clone(), *cond)), Box::new(precompile_node(session, scope.clone(), *texpr)), Box::new(precompile_node(session, scope.clone(), *fexpr)))
        },

        AST::Match(id, pos, cond, cases, cid) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(session, scope.clone(), case), precompile_node(session, scope.clone(), body) )).collect();
            AST::Match(id, pos, Box::new(precompile_node(session, scope.clone(), *cond)), cases, cid)
        },

        AST::Try(id, pos, cond, cases, cid) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(session, scope.clone(), case), precompile_node(session, scope.clone(), body) )).collect();
            AST::Try(id, pos, Box::new(precompile_node(session, scope.clone(), *cond)), cases, cid)
        },

        AST::Raise(id, pos, expr) => {
            AST::Raise(id, pos, Box::new(precompile_node(session, scope.clone(), *expr)))
        },

        AST::While(id, pos, cond, body) => {
            AST::While(id, pos, Box::new(precompile_node(session, scope.clone(), *cond)), Box::new(precompile_node(session, scope.clone(), *body)))
        },

        AST::For(id, pos, name, cond, body) => {
            let lscope = session.map.get(&id);
            AST::For(id, pos, name, Box::new(precompile_node(session, lscope.clone(), *cond)), Box::new(precompile_node(session, lscope.clone(), *body)))
        },

        AST::Tuple(id, pos, code) => { AST::Tuple(id, pos, precompile_vec(session, scope.clone(), code)) },

        AST::List(id, pos, code) => { AST::List(id, pos, precompile_vec(session, scope.clone(), code)) },
        /*
        AST::List(id, pos, code, stype) => {
            use abi::ABI;
            use utils::UniqueID;
            let mut block = vec!();
            let itype = stype.unwrap();
            let ltype = Type::Object(format!("List"), vec!(itype.clone()));
            let id = format!("{}", UniqueID::generate());
            block.push(AST::Definition(id, pos.clone(), (id, Some(ltype.clone())),
                Box::new(AST::Invoke(id, pos.clone(),
                    Box::new(AST::Resolver(id, pos.clone(), Box::new(AST::Identifier(id, pos.clone(), format!("List"))), String::from("new"))),
                    vec!(AST::New(id, pos.clone(), (format!("List"), vec!(itype.clone()))) /*, AST::Integer(code.len() as isize)*/), Some(Type::Function(Box::new(Type::Tuple(vec!(ltype.clone()))), Box::new(ltype.clone()), ABI::Molten))))));
            for item in code {
                block.push(AST::Invoke(id, pos.clone(),
                    Box::new(AST::Accessor(id, pos.clone(), Box::new(AST::Identifier(id, pos.clone(), id)), String::from("push"), Some(ltype.clone()))),
                    vec!(AST::Identifier(id, pos.clone(), id), item), Some(Type::Function(Box::new(Type::Tuple(vec!(ltype.clone()))), Box::new(ltype.clone()), ABI::Molten))));
            }
            block.push(AST::Identifier(id, pos.clone(), id));
            AST::Block(id, pos.clone(), precompile_vec(session, scope.clone(), block))
        },
        */

        AST::PtrCast(_, _) => { node },
        AST::New(_, _, _) => { node },

        AST::Class(id, pos, classspec, parentspec, body) => {
            let tscope = session.map.get(&id);
            //update_scope_variable_types(session, tscope.clone());
            //let classdef = scope.get_class_def(&classspec.0);
            //update_scope_variable_types(session, classdef.clone());
            let mut body = precompile_vec(session, tscope.clone(), body);

            /*
            let initname = String::from("__init__");
            let objtype = Type::from_spec(classspec.clone());
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

            AST::Class(id, pos, classspec, parentspec, body)
        },


        AST::Resolver(id, pos, left, right) => {
            AST::Resolver(id, pos, Box::new(precompile_node(session, scope.clone(), *left)), right)
        },

        AST::Accessor(id, pos, left, right, oid) => {
            AST::Accessor(id, pos, Box::new(precompile_node(session, scope.clone(), *left)), right, oid)
        },

        AST::Assignment(id, pos, left, right) => {
            AST::Assignment(id, pos, Box::new(precompile_node(session, scope.clone(), *left)), Box::new(precompile_node(session, scope.clone(), *right)))
        },

        AST::Import(id, pos, name, decls) => {
            AST::Import(id, pos, name, precompile_vec(session, scope.clone(), decls))
        },

        AST::Nil(id) => AST::Nil(id),

        AST::TypeDef(id, pos, classspec, fields) => {
            AST::TypeDef(id, pos, classspec, fields.into_iter().map(|mut f| {
                f.ttype = f.ttype.map(|t| resolve_type(session, scope.clone(), t));
                f
            }).collect())
        },

        AST::Underscore => { node },
        AST::Literal(_, _) => { node },

        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    }
}

/*
pub fn register_function(session: &Session, scope: ScopeRef, function: &AST) {
    if let &AST::Function(ref id, ref name, ref args, ref ret, ref body, ref id) = function {
        let name = name.clone().unwrap();
        let fscope = session.map.add(id, Some(scope.clone()));
        fscope.set_basename(name.clone());

        for arg in args {
            fscope.define(arg.0.clone(), arg.1.clone());
        }

        let ftype = Type::Function(Box::new(Type::Tuple(args.iter().map(|t| t.ident.clone().unwrap()).collect())), Box::new(ret.clone().unwrap()));
        let dscope = Scope::target(session, scope.clone());
        dscope.define(name, Some(ftype.clone()));
    }
}
*/

