
use types::Type;
use utils::UniqueID;
use parser::{ AST, parse_type };
use scope::{ Scope, ScopeRef, ScopeMapRef };

pub fn make_global<V, T>() -> ScopeMapRef<V, T> where V: Clone, T: Clone {
    let map = ScopeMapRef::new();
    let primatives = map.add(UniqueID(0), None);

    primatives.borrow_mut().define_type(String::from("Nil"), Type::Object(String::from("Nil")));
    primatives.borrow_mut().define_type(String::from("Int"), Type::Object(String::from("Int")));
    primatives.borrow_mut().define_type(String::from("Real"), Type::Object(String::from("Real")));
    primatives.borrow_mut().define_type(String::from("String"), Type::Object(String::from("String")));
    primatives.borrow_mut().define_type(String::from("Bool"), Type::Object(String::from("Bool")));
    primatives.borrow_mut().define_type(String::from("Class"), Type::Object(String::from("Class")));

    let intdef = Scope::new_ref(None);
    intdef.borrow_mut().set_basename(String::from("Int"));
    let bintype = parse_type("(Int, Int) -> Int");
    let booltype = parse_type("(Int, Int) -> Bool");
    intdef.borrow_mut().define(String::from("*"), bintype.clone());
    //intdef.borrow_mut().define_assign(String::from("*"), bintype.clone(), Value::Builtin(mul));
    intdef.borrow_mut().define(String::from("+"), bintype.clone());
    intdef.borrow_mut().define(String::from("add"), bintype.clone());
    intdef.borrow_mut().define(String::from("-"), bintype.clone());
    intdef.borrow_mut().define(String::from("<"), booltype.clone());
    intdef.borrow_mut().define(String::from(">"), booltype.clone());
    intdef.borrow_mut().define(String::from("/"), bintype.clone());
    intdef.borrow_mut().define(String::from("=="), booltype.clone());
    primatives.borrow_mut().set_class_def(&String::from("Int"), None, intdef);


    let bintype = parse_type("('a, 'a) -> 'a");
    let booltype = parse_type("('a, 'a) -> Bool");
    primatives.borrow_mut().define(String::from("*"), bintype.clone());
    primatives.borrow_mut().define(String::from("/"), bintype.clone());
    primatives.borrow_mut().define(String::from("^"), bintype.clone());
    primatives.borrow_mut().define(String::from("%"), bintype.clone());

    //primatives.borrow_mut().define(String::from("+"), bintype.clone());
    primatives.borrow_mut().define(String::from("+"), Some(Type::Overload(vec!(parse_type("(Real, Real) -> Real").unwrap(), parse_type("(Int, Int) -> Int").unwrap()))));
    //primatives.borrow_mut().define_func(String::from("+"), parse_type("(Int, Int) -> Int"), false);
    //primatives.borrow_mut().define_func(String::from("+"), parse_type("(Real, Real) -> Real"), false);

    primatives.borrow_mut().define(String::from("-"), bintype.clone());
    primatives.borrow_mut().define(String::from("<<"), bintype.clone());
    primatives.borrow_mut().define(String::from(">>"), bintype.clone());
    primatives.borrow_mut().define(String::from("<"), booltype.clone());
    primatives.borrow_mut().define(String::from(">"), booltype.clone());
    primatives.borrow_mut().define(String::from("<="), booltype.clone());
    primatives.borrow_mut().define(String::from(">="), booltype.clone());
    primatives.borrow_mut().define(String::from("=="), booltype.clone());
    primatives.borrow_mut().define(String::from("!="), booltype.clone());
    primatives.borrow_mut().define(String::from("&"), bintype.clone());
    primatives.borrow_mut().define(String::from("|"), bintype.clone());
    primatives.borrow_mut().define(String::from("and"), booltype.clone());
    primatives.borrow_mut().define(String::from("or"), booltype.clone());
    primatives.borrow_mut().define(String::from("~"), parse_type("('a) -> 'a"));
    primatives.borrow_mut().define(String::from("not"), parse_type("('a) -> Bool"));

    primatives.borrow_mut().define(String::from("puts"), parse_type("(String) -> Bool"));
    primatives.borrow_mut().define(String::from("malloc"), parse_type("(Int) -> 'a"));
    primatives.borrow_mut().define(String::from("realloc"), parse_type("('a, Int) -> 'a"));
    primatives.borrow_mut().define(String::from("free"), parse_type("('a) -> Nil"));
    primatives.borrow_mut().define(String::from("sprintf"), parse_type("('a, String, 'b) -> Int"));

    //primatives.borrow_mut().define(String::from("str"), parse_type("(Int) -> String"));
    primatives.borrow_mut().define(String::from("strlen"), parse_type("(String) -> Int"));

    let global = map.add(UniqueID(1), Some(primatives));
    global.borrow_mut().set_global(true);

    return map;
}
 


