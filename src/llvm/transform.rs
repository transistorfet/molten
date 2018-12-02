
use std::cell::RefCell;

use abi::ABI;
use defs::Def;
use types::Type;
use scope::{ Scope, ScopeRef };
use session::{ Error, Session };
use ast::{ NodeID, Pos, Ident, Literal, Argument, ClassSpec, AST };

use defs::variables::ArgDef;
use defs::functions::FuncDef;



#[derive(Clone, Debug, PartialEq)]
pub struct TopLevel {
    pub id: NodeID,
    pub pos: Pos,
    pub kind: TopKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TopKind {
    Global(String),
    Declare(String),
    Func(String, Vec<(NodeID, String)>, Expr),
    Closure(NodeID, String, String, Vec<(NodeID, String)>, Expr),
    Method(String, Vec<(NodeID, String)>, Expr),
    ClassDef(String, Vec<Expr>),
    StructDef(String),
}

impl TopLevel {
    pub fn new(id: NodeID, pos: Pos, kind: TopKind) -> Self {
        TopLevel {
            id: id,
            pos: pos,
            kind: kind,
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: NodeID,
    pub pos: Pos,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    // TODO remove this by making a special pattern enum
    Underscore,
    Nil,
    Literal(Literal),
    AccessValue(String),

    Invoke(InvokeKind, Vec<Expr>),

    DefVar(String, Box<Expr>),
    DefGlobal(String, Box<Expr>),
    AccessVar(String),
    SetClosure(Box<Expr>),

    SideEffect(String, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    For(String, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Expr, Expr)>, NodeID),

    Try(Box<Expr>, Vec<(Expr, Expr)>, NodeID),
    Raise(Box<Expr>),

    Tuple(Vec<Expr>),
    PtrCast(Type, Box<Expr>),

    AllocObject(String),
    AccessField(Box<Expr>, usize, Type),
    AccessVtable(Box<Expr>, String, NodeID),
    AccessMethod(NodeID),
    AssignField(Box<Expr>, String, Box<Expr>, NodeID),

    Block(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum InvokeKind {
    Method(Box<Expr>, String, NodeID),
    Func(Box<Expr>),
    Closure(Box<Expr>),
    CFunc(Box<Expr>),
    CByName(String),
}


impl Expr {
    pub fn new(id: NodeID, pos: Pos, kind: ExprKind) -> Self {
        Expr {
            id: id,
            pos: pos,
            kind: kind,
        }
    }

}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Func,
    ClassBody,
    Closure(NodeID),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Transform<'sess> {
    pub session: &'sess Session,
    pub toplevel: RefCell<Vec<TopLevel>>,
    pub context: RefCell<Vec<CodeContext>>,
}

impl<'sess> Transform<'sess> {
    pub fn new(session: &'sess Session) -> Self {
        Transform {
            session: session,
            toplevel: RefCell::new(vec!()),
            context: RefCell::new(vec!()),
        }
    }

    pub fn transform_program(&self, scope: ScopeRef, code: &Vec<AST>) {
        let initid = NodeID::generate();
        let fscope = self.session.map.add(initid, Some(scope.clone()));
        self.session.set_type(initid, Type::Function(Box::new(Type::Tuple(vec!())), Box::new(scope.make_obj(self.session, String::from("Bool"), vec!()).unwrap()), ABI::C));
        let mut body = self.transform_vec(scope.clone(), code);
        body.push(Expr::new(NodeID::generate(), Pos::empty(), ExprKind::Literal(Literal::Boolean(true))));
        self.add_function(initid, Pos::empty(), format!("init.{}", self.session.name), vec!(), Expr::new(NodeID::generate(), Pos::empty(), ExprKind::Block(body)));
    }

    pub fn transform_vec(&self, scope: ScopeRef, code: &Vec<AST>) -> Vec<Expr> {
        let mut exprs = vec!();
        for node in code {
            exprs.push(self.transform_expr(scope.clone(), node));
        }
        exprs
    }

    pub fn transform_expr(&self, scope: ScopeRef, node: &AST) -> Expr {
        match *node {

            /////// Functions ///////

            AST::Function(ref id, ref pos, ref ident, ref args, _, ref body, ref abi) => {
                let def = self.session.get_def(*id).unwrap();
                let fscope = self.session.map.get(id);
                let fname = self.transform_func_name(scope.clone(), ident.as_ref(), *id);
                let mut args = self.transform_args(args);

                if let Def::Closure(ref cl) = def {
                    let cid = NodeID::generate();
                    let cname = String::from("__context__");
                    args.insert(0, (cid, cname.clone()));
                    ArgDef::define(self.session, fscope.clone(), cid, false, &cname, Some(cl.contexttype.clone())).unwrap();
                    let ftype = cl.add_context_to_ftype(self.session);
                    let real_fname = fname.clone() + &"_func";
                    cl.add_field(self.session, real_fname.as_str(), ftype);
                    let index = self.toplevel.borrow().len();

                    self.set_context(CodeContext::Closure(*id));
                    let body = self.transform_expr(fscope.clone(), body);
                    self.restore_context();

                    use binding;
                    use typecheck;
                    use scope::Scope;
                    let fid = NodeID::generate();
                    //let tscope = Scope::new_ref(Some(scope.clone()));
                    let mut code = vec!();
                    code.push(AST::Definition(cl.varid, pos.clone(), true, Ident::new(fname.clone()), None, Box::new(AST::make_new(pos.clone(), ClassSpec::from_str(format!("{}_context_{}", cl.name, cl.contextid).as_str())))));
                    FuncDef::define(self.session, scope.clone(), fid, &Some(real_fname.clone()), Some(self.session.get_type(*id).unwrap()));
                    for (index, &(ref field, ref ttype)) in cl.context.fields.borrow().iter().enumerate() {
                        code.push(AST::make_assign(pos.clone(),
                            AST::make_access(pos.clone(), AST::make_ident(pos.clone(), Ident::new(fname.clone())), Ident::from_str(field.as_str())),
                            AST::make_ident_from_str(pos.clone(), field.as_str()))
                        );
                    }
                    code.push(AST::make_ident(pos.clone(), Ident::new(fname.clone())));

                    binding::bind_names(self.session, scope.clone(), &mut code);
                    typecheck::check_types(self.session, scope.clone(), &code);
                    let mut access = self.transform_vec(scope.clone(), &code);
                    let last = access.pop().unwrap();
                    access.push(Expr::new(*id, pos.clone(), ExprKind::SetClosure(Box::new(last))));
                    println!("THE THING: {:#?}", access);

                    //self.add_closure(*id, pos.clone(), fid, real_fname.clone(), fname.clone(), args, body);
                    self.toplevel.borrow_mut().insert(index, TopLevel::new(*id, pos.clone(), TopKind::Closure(fid, real_fname.clone(), fname.clone(), args, body)));
                    //Expr::new(*id, pos.clone(), ExprKind::MakeClosure)
                    Expr::new(*id, pos.clone(), ExprKind::Block(access))
                } else {
                    self.add_function(*id, pos.clone(), fname.clone(), args, self.transform_expr(fscope.clone(), body));
                    self.access_def(*id, pos.clone(), fname.clone())
                }
            },

            AST::Declare(ref id, ref pos, ref ident, _) => {
                let name = self.transform_func_name(scope.clone(), Some(&ident), *id);
                self.add_decl(*id, pos.clone(), name.clone());
                self.access_def(*id, pos.clone(), name.clone())
            },

            AST::Invoke(ref id, ref pos, ref fexpr, ref args) => {
                let mut args = self.transform_vec(scope.clone(), args);
                let invoke = match **fexpr {
                    AST::Accessor(ref aid, ref pos, ref obj, ref field, ref oid) => {
                        InvokeKind::Method(Box::new(self.transform_expr(scope.clone(), obj)), field.name.clone(), *oid)
                    },
                    _ => match self.session.get_type(*id) {
                        Some(Type::Function(_, _, ref abi)) => match *abi {
                            ABI::C => InvokeKind::CFunc(Box::new(self.transform_expr(scope.clone(), fexpr))),
                            ABI::Unknown |
                            ABI::Molten => {
                                // TODO this might cause you problems, if the ref is not always set (calculated calls).
                                if let Ok(Def::Closure(ref cl)) = self.session.get_def_from_ref(*id) {
                                    debug!("===== So a closure: {:?}", cl);
                                    InvokeKind::Closure(Box::new(self.transform_expr(scope.clone(), fexpr)))
                                } else {
                                    debug!("===== much function: {:?}", self.session.get_ref(*id));
                                    InvokeKind::Func(Box::new(self.transform_expr(scope.clone(), fexpr)))
                                }
                            },
                            _ => panic!("Unsupported ABI type {:?} for {:?}", abi, fexpr),
                        },
                        _ => panic!("FunctionError: invoking a function with no type set, {:?}", fexpr),
                    },
                };

                Expr::new(*id, pos.clone(), ExprKind::Invoke(invoke, args))
            }


            /////// Variables ///////

            AST::Definition(ref id, ref pos, ref mutable, ref ident, _, ref value) => {
                let name = self.transform_def_name(scope.clone(), ident);
                if scope.is_global() {
                    self.add_global(*id, pos.clone(), name.clone());
                    Expr::new(*id, pos.clone(), ExprKind::DefGlobal(name, Box::new(self.transform_expr(scope.clone(), value))))
                } else {
                    Expr::new(*id, pos.clone(), ExprKind::DefVar(name, Box::new(self.transform_expr(scope.clone(), value))))
                }
            },

            AST::Identifier(ref id, ref pos, ref ident) => {
                let def = self.session.get_def_from_ref(*id).unwrap();
                // TODO handle the case of accessing globals
                match def {
                    Def::Func(_) |
                    Def::CFunc(_) => Expr::new(*id, pos.clone(), ExprKind::AccessValue(ident.name.clone())),
                    _ => {
                        // TODO actually we shouldn't access scope, because that wont get us the overloaded target, but how will we tell if a reference is local???
                        if !scope.contains_local(&ident.name) && !Scope::global(scope.clone()).contains(&ident.name) {
                            match self.get_context() {
                                Some(CodeContext::Closure(ref cid)) => {
                                    let cl = self.session.get_def(*cid).unwrap().as_closure().unwrap();
                                    cl.add_field(self.session, ident.as_str(), self.session.get_type_from_ref(*id).unwrap());

                                    let oid = NodeID::generate();
                                    let objexpr = Expr::new(oid, pos.clone(), ExprKind::AccessValue(String::from("__context__")));
                                    self.session.set_type(oid, cl.contexttype.clone());

                                    let objdef = self.session.get_def(cl.contexttype.get_id().unwrap()).unwrap();
                                    if let Some((structindex, ftype)) = objdef.as_struct().unwrap().find_field(ident.as_str()) {
                                        Expr::new(*id, pos.clone(), ExprKind::AccessField(Box::new(objexpr), structindex, ftype))
                                    } else {
                                        panic!("Attempting to access a closure context member that doesn't exist: {:?}", ident.name);
                                    }
                                },
                                _ => panic!("Cannot access variable outside of scope: {:?}", node),
                            }
                        } else if let Def::Var(_) = def {
                            Expr::new(*id, pos.clone(), ExprKind::AccessVar(ident.name.clone()))
                        // TODO this doesn't work because a closure will not always be a variable, but sometimes it will...
                        //} else if let Def::Closure(_) = def {
                        //    Expr::new(*id, pos.clone(), ExprKind::AccessVar(ident.name.clone()))
                        } else {
                            Expr::new(*id, pos.clone(), ExprKind::AccessValue(ident.name.clone()))
                        }
                    }
                }
            },

            AST::Recall(ref id, ref pos) => {
                Expr::new(*id, pos.clone(), ExprKind::AccessValue(String::from("")))
            },


            /////// Objects ///////

            AST::Class(ref id, ref pos, _, _, ref body) => {
                let tscope = self.session.map.get(id);
                let index = self.toplevel.borrow().len();
                let body = self.transform_class_body(*id, tscope.clone(), body);
                self.toplevel.borrow_mut().insert(index, TopLevel::new(*id, pos.clone(), TopKind::ClassDef(tscope.get_basename(), body)));
                Expr::new(*id, pos.clone(), ExprKind::Nil)
            },

            AST::New(ref id, ref pos, ClassSpec { ref ident, .. }) => {
                // TODO do you need to get the full class name
                Expr::new(*id, pos.clone(), ExprKind::AllocObject(ident.name.clone()))
            },

            AST::Resolver(ref id, ref pos, ref tname, ref method) => {
                Expr::new(*id, pos.clone(), ExprKind::AccessMethod(self.session.get_ref(*id).unwrap()))
            },

            AST::Accessor(ref id, ref pos, ref obj, ref field, ref oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                self.transform_access(scope.clone(), *id, pos.clone(), self.transform_expr(scope.clone(), obj), field.as_str(), otype, *oid)
            },

            AST::Assignment(ref id, ref pos, ref left, ref right) => {
                match **left {
                    AST::Accessor(ref id, _, ref obj, ref field, ref oid) => {
                        Expr::new(*id, pos.clone(), ExprKind::AssignField(Box::new(self.transform_expr(scope.clone(), obj)), field.name.clone(), Box::new(self.transform_expr(scope.clone(), right)), *oid))
                    },
                    _ => panic!("UnsupportedError: attempting to assign to something that is not an object field, {:?}", left),
                }
            },

            AST::TypeDef(ref id, ref pos, _, ref fields) => {
                let structdef = self.session.get_def(*id).unwrap().as_struct().unwrap();
                for field in fields {
                    structdef.add_field(self.session, true, field.ident.as_str(), self.session.get_type(field.id).unwrap());
                }

                let tscope = self.session.map.get(id);
                self.add_top(*id, pos.clone(), TopKind::StructDef(tscope.get_basename()));
                Expr::new(*id, pos.clone(), ExprKind::Nil)
            },


            /////// Import ///////

            AST::Import(ref id, ref pos, ref ident, ref decls) => {
                let mut decls = self.transform_vec(scope.clone(), decls);

                let initid = NodeID::generate();
                let module_init_name = format!("init.{}", &ident.name);
                // TODO can we simplify type creation
                let inittype = Type::Function(Box::new(Type::Tuple(vec!())), Box::new(scope.make_obj(self.session, String::from("Bool"), vec!()).unwrap()), ABI::C);
                self.session.set_type(initid, inittype);
                self.add_decl(initid, pos.clone(), module_init_name.clone());
                decls.push(Expr::new(initid, pos.clone(), ExprKind::Invoke(
                    InvokeKind::CByName(module_init_name),
                    vec!()
                )));

                Expr::new(*id, pos.clone(), ExprKind::Block(decls))
            },


            /////// Flow Control ///////

            AST::Block(ref id, ref pos, ref code) => {
                Expr::new(*id, pos.clone(), ExprKind::Block(self.transform_vec(scope.clone(), code)))
            },

            AST::SideEffect(ref id, ref pos, ref op, ref args) => {
                let args = self.transform_vec(scope.clone(), args);
                Expr::new(*id, pos.clone(), ExprKind::SideEffect(op.name.clone(), args))
            },

            AST::If(ref id, ref pos, ref cond, ref texpr, ref fexpr) => {
                Expr::new(*id, pos.clone(), ExprKind::If(
                    Box::new(self.transform_expr(scope.clone(), cond)),
                    Box::new(self.transform_expr(scope.clone(), texpr)),
                    Box::new(self.transform_expr(scope.clone(), fexpr))
                ))
            },

            AST::Match(ref id, ref pos, ref cond, ref cases, ref cid) => {
                let cases = cases.iter().map(|(pat, body)| (self.transform_expr(scope.clone(), pat), self.transform_expr(scope.clone(), body))).collect();
                let ctype = self.session.get_type(*cid).unwrap();
                let compid = scope.get_var_def(&String::from("==")).unwrap();
                let compid = match self.session.get_def(compid) {
                    Ok(Def::Overload(ol)) => ol.find_variant(self.session, scope.clone(), Type::Tuple(vec!(ctype.clone(), ctype.clone()))).unwrap().0,
                    _ => compid,
                };
                Expr::new(*id, pos.clone(), ExprKind::Match(Box::new(self.transform_expr(scope.clone(), cond)), cases, compid))
            },

            AST::For(ref id, ref pos, ref ident, ref cond, ref body) => {
                let lscope = self.session.map.get(id);
                Expr::new(*id, pos.clone(), ExprKind::For(ident.name.clone(), Box::new(self.transform_expr(scope.clone(), cond)), Box::new(self.transform_expr(lscope.clone(), body))))
            },
            AST::While(ref id, ref pos, ref cond, ref body) => {
                Expr::new(*id, pos.clone(), ExprKind::While(Box::new(self.transform_expr(scope.clone(), cond)), Box::new(self.transform_expr(scope.clone(), body))))
            },


            /////// Exceptions ///////

            AST::Try(ref id, ref pos, ref cond, ref cases, ref cid) => {
                let cases = cases.iter().map(|(pat, body)| (self.transform_expr(scope.clone(), pat), self.transform_expr(scope.clone(), body))).collect();
                Expr::new(*id, pos.clone(), ExprKind::Try(Box::new(self.transform_expr(scope.clone(), cond)), cases, *cid))
            },
            AST::Raise(ref id, ref pos, ref value) => {
                Expr::new(*id, pos.clone(), ExprKind::Raise(Box::new(self.transform_expr(scope.clone(), value))))
            },


            /////// Literals ///////

            AST::Literal(ref id, ref lit) => Expr::new(*id, Pos::empty(), ExprKind::Literal(lit.clone())),
            AST::Nil(ref id) => Expr::new(*id, Pos::empty(), ExprKind::Nil),

            AST::Tuple(ref id, ref pos, ref items) => {
                let items = items.iter().map(|item| self.transform_expr(scope.clone(), item)).collect();
                Expr::new(*id, pos.clone(), ExprKind::Tuple(items))
            },

            AST::Record(ref id, ref pos, ref items) => {
                let items = items.iter().map(|(name, item)| self.transform_expr(scope.clone(), item)).collect();
                Expr::new(*id, pos.clone(), ExprKind::Tuple(items))
            },

            AST::PtrCast(ref ttype, ref node) => Expr::new(NodeID::generate(), Pos::empty(), ExprKind::PtrCast(ttype.clone(), Box::new(self.transform_expr(scope.clone(), node)))),

            AST::Underscore => Expr::new(NodeID::generate(), Pos::empty(), ExprKind::Underscore),

            AST::List(_, _, _) |
            AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),

        }
    }

    pub fn transform_access(&self, scope: ScopeRef, id: NodeID, pos: Pos, objexpr: Expr, field: &str, otype: Type, oid: NodeID) -> Expr {
        //let otype = self.session.get_type(*oid).unwrap();
        let objdef = self.session.get_def(otype.get_id().unwrap()).unwrap();
        let ttype = self.session.get_type_from_ref(id).unwrap();

        if let Some((structindex, ftype)) = objdef.as_struct().unwrap().find_field(field) {
            Expr::new(id, pos.clone(), ExprKind::AccessField(Box::new(objexpr), structindex, ftype))
        } else if let Some(vtableindex) = objdef.as_class().unwrap().get_vtable_index(self.session, scope.clone(), field, &ttype) {
            Expr::new(id, pos.clone(), ExprKind::AccessVtable(Box::new(objexpr), String::from(field), oid))
        } else {
            Expr::new(id, pos.clone(), ExprKind::AccessMethod(self.session.get_ref(id).unwrap()))
        }
    }

    pub fn transform_class_body(&self, cid: NodeID, scope: ScopeRef, body: &Vec<AST>) -> Vec<Expr> {
        let classdef = self.session.get_def(cid).unwrap().as_class().unwrap();

        let name = scope.get_basename();
        // TODO remove this hardcoded check before constructing
        if name.as_str() != "String" && !name.as_str().contains("closure") {
            classdef.build_vtable(self.session, scope.clone(), body);
        }
        classdef.build_structdef(self.session, scope.clone(), body);

        let mut newbody = vec!();
        // TODO set context
        for node in body {
            match *node {
                AST::Function(ref id, ref pos, ref ident, ref args, _, ref body, ref abi) => {
                    let fscope = self.session.map.get(id);
                    let name = self.transform_func_name(scope.clone(), ident.as_ref(), *id);
                    self.add_method(*id, pos.clone(), name.clone(), self.transform_args(args), self.transform_expr(fscope.clone(), body));
                    newbody.push(self.access_def(NodeID::generate(), pos.clone(), name.clone()));
                },
                AST::Declare(ref id, ref pos, ref ident, _) => {
                    let name = self.transform_func_name(scope.clone(), Some(&ident), *id);
                    self.add_decl(*id, pos.clone(), name.clone());
                    newbody.push(self.access_def(NodeID::generate(), pos.clone(), name.clone()));
                },
                AST::Definition(ref id, ref pos, ref mutable, ref ident, _, ref value) => {
                    //let name = self.transform_def_name(scope.clone(), ident);
                    //Expr::new(*id, *pos, ExprKind::AssignField(self.transform_expr(scope.clone(), obj), name.clone(), self.transform_expr(scope.clone(), value)));
                    newbody.push(self.access_def(NodeID::generate(), pos.clone(), ident.name.clone()));
                },
                _ => {
                    // TODO everything else goes into the body of the initializer class!?!?
                    newbody.push(self.transform_expr(scope.clone(), node));
                }
            }
        }
        newbody
    }

    pub fn transform_args(&self, args: &Vec<Argument>) -> Vec<(NodeID, String)> {
        args.iter().map(|arg| (arg.id, arg.ident.name.clone())).collect()
    }


    pub fn add_top(&self, id: NodeID, pos: Pos, kind: TopKind) {
        self.toplevel.borrow_mut().push(TopLevel::new(id, pos, kind));
    }

    pub fn add_function(&self, id: NodeID, pos: Pos, name: String, args: Vec<(NodeID, String)>, body: Expr) {
        self.add_top(id, pos, TopKind::Func(name, args, body));
    }

    pub fn add_closure(&self, id: NodeID, pos: Pos, fid: NodeID, fname: String, name: String, args: Vec<(NodeID, String)>, body: Expr) {
        self.add_top(id, pos, TopKind::Closure(fid, fname, name, args, body));
    }

    pub fn add_decl(&self, id: NodeID, pos: Pos, name: String) {
        self.add_top(id, pos, TopKind::Declare(name));
    }

    pub fn add_method(&self, id: NodeID, pos: Pos, name: String, args: Vec<(NodeID, String)>, body: Expr) {
        self.add_top(id, pos, TopKind::Method(name, args, body));
    }

    pub fn add_global(&self, id: NodeID, pos: Pos, name: String) {
        self.add_top(id, pos, TopKind::Global(name));
    }

    pub fn access_def(&self, defid: NodeID, pos: Pos, name: String) -> Expr {
        let id = NodeID::generate();
        self.session.set_ref(id, defid);
        Expr::new(id, pos, ExprKind::AccessValue(name))
    }

    pub fn transform_func_name(&self, scope: ScopeRef, ident: Option<&Ident>, id: NodeID) -> String {
        scope.get_full_name(ident.map(|ident| get_mangled_name(self.session, scope.clone(), &ident.name, id)), id)
    }

    pub fn transform_def_name(&self, scope: ScopeRef, ident: &Ident) -> String {
        scope.get_basename() + &ident.name
    }

    pub fn set_context(&self, context: CodeContext) {
        self.context.borrow_mut().push(context);
    }

    pub fn restore_context(&self) {
        self.context.borrow_mut().pop();
    }

    pub fn get_context(&self) -> Option<CodeContext> {
        self.context.borrow_mut().last().map(|c| *c)
    }
}

fn get_mangled_name(session: &Session, scope: ScopeRef, name: &String, id: NodeID) -> String {
    //let dscope = Scope::target(session, scope);
    //let defid = dscope.get_var_def(&name).unwrap();
    //let def = session.get_def(defid).unwrap();

    let ftype = session.get_type(id).unwrap();
    //ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), def.num_variants(session))
    ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), 2)
}


