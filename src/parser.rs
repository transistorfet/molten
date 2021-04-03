

extern crate nom;
use nom::{
    digit,
    hex_digit,
    oct_digit,
    line_ending,
    not_line_ending,
    anychar,
    space,
    multispace,
    is_alphanumeric,
    is_alphabetic,
    Context,
    ErrorKind,
    error_to_list
};
use nom::types::CompleteByteSlice;

extern crate nom_locate;
use nom_locate::LocatedSpan;

use std;
use std::f64;
use std::str;
use std::str::FromStr;

use abi::ABI;
use types::Type;
use misc::{ r, UniqueID };
use ast::{ Pos, AST };
use hir::{ NodeID, Mutability, Visibility, AssignType, Literal, Ident, Argument, ClassSpec, Pattern, PatKind };


///// Parsing Macros /////

const ERR_IN_FUNC: u32 = 40;
const ERR_IN_MATCH: u32 = 41;
const ERR_IN_TRY: u32 = 42;
const ERR_IN_WHILE: u32 = 43;
const ERR_IN_FOR: u32 = 44;
const ERR_IN_CLASS: u32 = 45;
const ERR_IN_LIST: u32 = 46;

pub type Span<'a> = LocatedSpan<CompleteByteSlice<'a>>;

//named!(sp, eat_separator!(&b" \t"[..]));

#[macro_export]
macro_rules! sp {
    ($i:expr, $($args:tt)*) => {
        sep!($i, sp, $($args)*)
    }
}

#[macro_export]
macro_rules! wscom {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        //terminated!($i, $submac!($($args)*), line_or_space_or_comment)
        //preceded!($i, line_or_space_or_comment, $submac!($($args)*))
        //sep!($i, line_or_space_or_comment, $submac!($($args)*))
        delimited!($i, line_or_space_or_comment, $submac!($($args)*), line_or_space_or_comment)
    });

    ($i:expr, $f:expr) => (
        wscom!($i, call!($f));
    );
}

#[macro_export]
macro_rules! wscoml {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        //terminated!($i, $submac!($($args)*), line_or_space_or_comment)
        preceded!($i, line_or_space_or_comment, $submac!($($args)*))
        //sep!($i, line_or_space_or_comment, $submac!($($args)*))
        //delimited!($i, line_or_space_or_comment, $submac!($($args)*), line_or_space_or_comment)
    });

    ($i:expr, $f:expr) => (
        wscoml!($i, call!($f));
    );
}

#[macro_export]
macro_rules! cont (
    ($i:expr, $f:expr) => {
        delimited!($i, line_continuation, call!($f), line_continuation)
    }
);

#[macro_export]
macro_rules! tag_word (
    ($i:expr, $f:expr) => {
        do_parse!($i,
            s: tag!($f) >>
            not!(peek!(alphanumeric_underscore)) >>
            ( s )
        )
    }
);

#[macro_export]
macro_rules! map_ident (
    ($i:expr, $($args:tt)*) => {
        map!($i, $($args)*, |s| ident_from_span(&s))
    }
);


///// Parser /////

pub fn parse_or_error(name: &str, text: &[u8]) -> Vec<AST> {
    let span = Span::new(CompleteByteSlice(text));
    match parse(span) {
        Ok((rem, _)) if rem.fragment != CompleteByteSlice(&[]) => panic!("InternalError: unparsed input remaining: {:?}", rem),
        Ok((_, code)) => code,
        Err(err) => { print_error(name, err); panic!("") },
    }
}


named!(pub parse(Span) -> Vec<AST>,
    complete!(do_parse!(
        e: statement_list >>
        eof!() >>
        (e)
    ))
);

named!(statement_list(Span) -> Vec<AST>,
    do_parse!(
        l: wscom!(separated_list_complete!(terminator, statement)) >>
        t: opt!(terminator) >>
        (add_terminator(l, t.unwrap_or(None)))
    )
);

fn add_terminator(mut list: Vec<AST>, term: Option<AST>) -> Vec<AST> {
    if let Some(term) = term {
        list.push(term);
    }
    list
}

named!(terminator(Span) -> Option<AST>,
    do_parse!(
        space_or_comment >>
        t: alt!(
            //map!(peek!(wscom!(tag!("}"))), |_| None) |
            map!(line_ending, |_| None) |
            map!(tag!(";"), |_| Some(AST::Literal(Literal::Unit)))
        ) >>
        line_or_space_or_comment >>
        (t)
    )
);

named!(statement(Span) -> AST,
    alt_complete!(
        import |
        class |
        typealias |
        typeenum |
        declare |
        raise |
        definition |
        assignment |
        expression
    )
);

named!(import(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("import")) >>
        e: recognize!(separated_list_complete!(tag!("."), identifier)) >>
        (AST::Import(Pos::new(pos), ident_from_span(&e), vec!()))
    )
);

named!(definition(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("let")) >>
        m: opt!(wscom!(tag_word!("mut"))) >>
        i: identifier_typed >>
        e: opt!(preceded!(
            wscom!(tag!("=")),
            expression
        )) >>
        (AST::Definition(
            Pos::new(pos),
            if m.is_some() { Mutability::Mutable } else { Mutability::Immutable },
            i.1,
            i.2,
            r(if e.is_some() { e.unwrap() } else { AST::Nil })
        ))
    )
);

named!(assignment(Span) -> AST,
    do_parse!(
        pos: position!() >>
        o: subatomic_operation >>
        wscom!(tag!("=")) >>
        e: expression >>
        (AST::Assignment(Pos::new(pos), r(o), r(e), AssignType::Update))
    )
);

named!(whileloop(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("while")) >>
        c: expression >>
        line_or_space_or_comment >>
        e: return_error!(ErrorKind::Custom(ERR_IN_WHILE), expression) >>
        (AST::While(Pos::new(pos), r(c), r(e)))
    )
);

named!(class(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("class")) >>
        i: class_spec >>
        p: opt!(preceded!(wscom!(tag_word!("extends")), class_spec)) >>
        wscom!(tag!("{")) >>
        s: many0!(wscom!(alt_complete!(
            typealias |
            definition |
            declare |
            function
        ))) >>
        return_error!(ErrorKind::Custom(ERR_IN_CLASS), tag!("}")) >>
        (AST::Class(Pos::new(pos), i, p, s))
        )
    );

named!(typealias(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("type")) >>
        c: class_spec >>
        wscom!(tag!("=")) >>
        ts: type_description >>
        (AST::TypeAlias(Pos::new(pos), c, ts))
    )
);

named!(typeenum(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("enum")) >>
        c: class_spec >>
        wscom!(tag!("=")) >>
        wscom!(opt!(tag!("|"))) >>
        ev: separated_list_complete!(wscom!(tag!("|")), enum_variant) >>
        (AST::Enum(Pos::new(pos), c, ev))
    )
);

named!(enum_variant(Span) -> (Pos, Ident, Option<Type>),
    do_parse!(
        pos: position!() >>
        i: identifier >>
        t: opt!(delimited!(tag!("("), separated_list_complete!(wscom!(tag!(",")), type_description), tag!(")"))) >>
        ((Pos::new(pos), i, t.map(|t| Type::Tuple(t))))
    )
);



named!(expression_list(Span) -> Vec<AST>,
    separated_list_complete!(wscom!(tag!(",")), expression)
);

named!(expression(Span) -> AST,
    alt_complete!(
        //underscore |
        ifexpr |
        trywith |
        matchcase |
        whileloop |
        forloop |
        newclass |
        function |
        reference |
        annotation |
        infix
    )
);



//named!(underscore(Span) -> AST,
//    value!(AST::Underscore, tag!("_"))
//);

named!(block(Span) -> AST,
    delimited!(
        alt!(tag_word!("begin") | tag!("{")),
        do_parse!(
            pos: position!() >>
            s: wscom!(statement_list) >>
            (AST::Block(Pos::new(pos), s))
        ),
        alt!(tag_word!("end") | tag!("}"))
    )
);

named!(ifexpr(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("if")) >>
        c: expression >>
        wscom!(tag_word!("then")) >>
        t: expression >>
        f: opt!(preceded!(
            wscom!(tag_word!("else")),
            expression
        )) >>
        (AST::If(Pos::new(pos), r(c), r(t), r(if f.is_some() { f.unwrap() } else { AST::Literal(Literal::Unit) })))
    )
);

named!(trywith(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("try")) >>
        c: expression >>
        line_or_space_or_comment >>
        opt!(wscom!(tag_word!("catch"))) >>
        return_error!(ErrorKind::Custom(ERR_IN_TRY),
            tag!("{")
        ) >>
        l: caselist >>
        return_error!(ErrorKind::Custom(ERR_IN_TRY),
            tag!("}")
        ) >>
        (AST::Try(Pos::new(pos), r(c), l))
    )
);

named!(raise(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("raise")) >>
        e: expression >>
        (AST::Raise(Pos::new(pos), r(e)))
    )
);

named!(matchcase(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("match")) >>
        c: expression >>
        line_or_space_or_comment >>
        //wscom!(tag_word!("with")) >>
        return_error!(ErrorKind::Custom(ERR_IN_MATCH),
            tag!("{")
        ) >>
        l: caselist >>
        return_error!(ErrorKind::Custom(ERR_IN_MATCH),
            tag!("}")
        ) >>
        (AST::Match(Pos::new(pos), r(c), l))
    )
);

named!(caselist(Span) -> Vec<(Pattern, AST)>,
    //separated_list_complete!(wscom!(tag!("|")), do_parse!(
    dbg_dmp!(many1!(wscom!(do_parse!(
        //wscom!(tag!("|")) >>
        c: pattern >>
        wscom!(tag!("=>")) >>
        e: expression >>
        //wscom!(tag!(",")) >>
        ((c, e))
    ))))
);

named!(forloop(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("for")) >>
        i: identifier >>
        wscom!(tag_word!("in")) >>
        l: expression >>
        line_or_space_or_comment >>
        e: return_error!(ErrorKind::Custom(ERR_IN_FOR), expression) >>
        (AST::For(Pos::new(pos), i, r(l), r(e)))
    )
);

named!(newclass(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("new")) >>
        cs: class_spec >>
        a: map!(
            delimited!(tag!("("), expression_list, tag!(")")),
            |mut a| { a.insert(0, AST::New(Pos::new(pos), cs.clone())); a }
        ) >>
        (AST::PtrCast(
            Type::Object(cs.ident.name.clone(), UniqueID(0), cs.types.clone()),
            r(AST::Invoke(Pos::new(pos), r(AST::Resolver(Pos::new(pos), r(AST::Identifier(Pos::new(pos), cs.ident.clone())), Ident::from_str("new"))), a))))
    )
);

named!(declare(Span) -> AST,
    do_parse!(
        pos: position!() >>
        vis: opt!(wscom!(tag_word!("pub"))) >>
        wscom!(tag_word!("decl")) >>
        n: alt_complete!(identifier | any_op) >>
        t: type_function >>
        (AST::Declare(Pos::new(pos), if vis.is_some() { Visibility::Public } else { Visibility::Private }, n, t))
    )
);

named!(function(Span) -> AST,
    do_parse!(
        pos: position!() >>
        vis: opt!(wscom!(tag_word!("pub"))) >>
        wscom!(tag_word!("fn")) >>
        //n: opt!(identifier) >>
        l: alt_complete!(
            do_parse!(
                n: opt!(alt_complete!(identifier | any_op)) >>
                //n: opt!(identifier) >>
                l: delimited!(tag!("("), argument_list, tag!(")")) >>
                ((n, l))
            ) |
            map!(argument_list, |l| (None, l))
        ) >>
        rt: opt!(preceded!(wscom!(tag!("->")), type_description)) >>
        a: abi_specifier >>
        e: alt_complete!(
            preceded!(wscom!(tag!("=>")), return_error!(ErrorKind::Custom(ERR_IN_FUNC), expression)) |
            return_error!(ErrorKind::Custom(ERR_IN_FUNC), wscoml!(block))
        ) >>
        (AST::Function(Pos::new(pos), if vis.is_some() { Visibility::Public } else { Visibility::Private }, l.0, l.1, rt, r(e), a))
    )
);

named!(reference(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("ref")) >>
        e: expression >>
        (AST::Ref(Pos::new(pos), r(e)))
    )
);

named!(annotation(Span) -> AST,
    do_parse!(
        e: atomic >>
        wscom!(tag!(":")) >>
        t: type_description >>
        (AST::PtrCast(t, r(e)))
    )
);

named!(argument_list(Span) -> Vec<Argument>,
    separated_list_complete!(wscom!(tag!(",")),
        do_parse!(
            i: identifier_typed >>
            // TODO this needs an Expr instead of AST, so probably needs an intermediate struct, but default args aren't supported yet anyways
            //d: opt!(preceded!(wscom!(tag!("=")), expression)) >>
            (Argument::new(i.0, i.1, i.2, None))
        )
    )
);


named!(infix_op(Span) -> Ident,
    map_ident!(
        alt!(
            tag!("*") |
            tag!("/") |
            tag!("^") |
            tag!("%") |
            tag!("+") |
            tag!("-") |
            tag!("<<") |
            tag!(">>") |
            tag!("<=") |
            tag!(">=") |
            tag!("<") |
            tag!(">") |
            tag!("==") |
            tag!("!=") |
            tag!("&") |
            tag!("|") |
            tag_word!("and") |
            tag_word!("or")
            //tag!("..") |
        )
        //tag!(".")
    )
);

impl AST {
    fn precedence(op: &str) -> i32 {
        match op {
            "*" | "/" | "%"         => 5,
            "+" | "-"               => 6,
            "<<" | ">>"             => 7,
            "<" | ">" | "<=" | ">=" => 8,
            "==" | "!="             => 9,
            "&"                     => 10,
            "|"                     => 12,
            "and"                   => 13,
            "or"                    => 14,
            _                       => 20,
        }
    }

    fn fold_op(left: AST, operations: Vec<(Span, Ident, AST)>) -> Self {
        let mut operands: Vec<AST> = vec!();
        let mut operators: Vec<(Pos, Ident, i32)> = vec!();
        operands.push(left);

        for (span, next_op, next_ast) in operations {
            let pos  = Pos::new(span);
            let p = AST::precedence(next_op.as_str());

            while !operators.is_empty() && operators.last().unwrap().2 <= p {
                let (pos, op, _) = operators.pop().unwrap();
                let r2 = operands.pop().unwrap();
                let r1 = operands.pop().unwrap();

                operands.push(AST::make_op(pos, op, r1, r2))
            }

            operators.push((pos, next_op, p));
            operands.push(next_ast);
        }

        while !operators.is_empty() {
            let (pos, op, _) = operators.pop().unwrap();
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            operands.push(AST::make_op(pos, op, r1, r2))
        }

        assert_eq!(operands.len(), 1);
        operands.pop().unwrap()
    }

    fn make_op(pos: Pos, op: Ident, r1: AST, r2: AST) -> AST {
        match op.as_str() {
            "and" | "or" => AST::SideEffect(pos, op, vec!(r1, r2)),
            _ =>
            //AST::Infix(pos, op, r(r1), r(r2))
            AST::Invoke(pos.clone(), r(AST::Identifier(pos, op)), vec!(r1, r2))
            //AST::Invoke(pos, AST::Access(pos, r1, op), vec!(r2))
        }
    }
}


named!(infix(Span) -> AST,
    do_parse!(
        left: atomic >>
        operations: many0!(tuple!(position!(), cont!(infix_op), atomic)) >>
        (AST::fold_op(left, operations))
    )
);

named!(atomic(Span) -> AST,
    alt_complete!(
        prefix |
        subatomic_operation
    )
);

named!(prefix_op(Span) -> Ident,
    map_ident!(alt!(
        tag_word!("not") |
        tag!("~")
    ))
);

named!(prefix(Span) -> AST,
    do_parse!(
        pos: position!() >>
        op: prefix_op >>
        opt!(space) >>
        a: atomic >>
        //(AST::Prefix(op, r(a)))
        (AST::Invoke(Pos::new(pos), r(AST::Identifier(Pos::new(pos), op)), vec!(a)))
    )
);

named!(subatomic_operation(Span) -> AST,
    do_parse!(
        left: subatomic >>
        operations: many0!(alt_complete!(
            map!(delimited!(tag!("["), tuple!(position!(), wscom!(expression)), tag!("]")), |(p, e)| SubOP::Index(Pos::new(p), e)) |
            map!(delimited!(tag!("("), tuple!(position!(), wscom!(expression_list)), tag!(")")), |(p, e)| SubOP::Invoke(Pos::new(p), e)) |
            map!(preceded!(tag!("."), tuple!(position!(), alt!(identifier | map!(digit, |s| ident_from_span(&s))))), |(p, s)| SubOP::Accessor(Pos::new(p), s)) |
            map!(preceded!(tag!("::"), tuple!(position!(), identifier)), |(p, s)| SubOP::Resolver(Pos::new(p), s))
        )) >>
        (AST::fold_access(left, operations))
    )
);

enum SubOP {
    Index(Pos, AST),
    Invoke(Pos, Vec<AST>),
    Accessor(Pos, Ident),
    Resolver(Pos, Ident),
}

impl AST {
    fn fold_access(left: AST, operations: Vec<SubOP>) -> Self {
        let mut ret = left;
        for op in operations {
            match op {
                SubOP::Index(p, e) => ret = AST::Index(p, r(ret), r(e)),
                SubOP::Invoke(p, e) => ret = AST::Invoke(p, r(ret), e),
                SubOP::Accessor(p, name) => ret = AST::Accessor(p, r(ret), name.clone()),
                SubOP::Resolver(p, name) => ret = AST::Resolver(p, r(ret), name.clone()),
            }
        }
        ret
    }
}

named!(subatomic(Span) -> AST,
    alt_complete!(
        delimited!(tag!("("), wscom!(expression), tag!(")")) |
        record_update |
        block |
        dereference |
        literal |
        identifier_node
    )
);

named!(record_update(Span) -> AST,
    delimited!(
        tag!("{"),
        do_parse!(
            pos: position!() >>
            i: wscom!(identifier_node) >>
            wscom!(tag_word!("with")) >>
            l: wscom!(record_field_assignments) >>
            (AST::RecordUpdate(Pos::new(pos), r(i), l))
        ),
        return_error!(ErrorKind::Custom(ERR_IN_LIST), tag!("}"))
    )
);

named!(dereference(Span) -> AST,
    do_parse!(
        pos: position!() >>
        tag!("*") >>
        a: subatomic >>
        (AST::Deref(Pos::new(pos), r(a)))
    )
);

named!(identifier_node(Span) -> AST,
    do_parse!(
        pos: position!() >>
        i: identifier >>
        (AST::Identifier(Pos::new(pos), i))
    )
);

named!(identifier(Span) -> Ident,
    do_parse!(
        not_reserved >>
        s: recognize!(preceded!(
            take_while1!(is_alpha_underscore),
            take_while!(is_alphanumeric_underscore)
        )) >>
        (ident_from_span(&s))
    )
);

named!(class_spec(Span) -> ClassSpec,
    do_parse!(
        pos: position!() >>
        i: identifier >>
        p: opt!(complete!(delimited!(tag!("<"), separated_list_complete!(wscom!(tag!(",")), type_description), tag!(">")))) >>
        (ClassSpec::new(Pos::new(pos), i, p.unwrap_or(vec!())))
    )
);

named!(identifier_typed(Span) -> (Pos, Ident, Option<Type>),
    do_parse!(
        pos: position!() >>
        i: identifier >>
        t: opt!(preceded!(wscom!(tag!(":")), type_description)) >>
        (Pos::new(pos), i, t)
    )
);

named!(any_op(Span) -> Ident,
    alt!(infix_op | prefix_op | map_ident!(alt!(tag!("[]") | tag!("::"))))
);

pub fn parse_type(s: &str) -> Option<Type> {
    match type_description(Span::new(CompleteByteSlice(s.as_bytes()))) {
        Ok((_, t)) => Some(t),
        e => panic!("Error Parsing Type: {:?}   [{:?}]", s, e)
    }
}

named!(type_description(Span) -> Type,
    alt_complete!(
        type_function |
        type_unit |
        type_ref |
        type_tuple |
        type_record |
        type_variable |
        type_object
    )
);

named!(type_ref(Span) -> Type,
    do_parse!(
        wscom!(tag_word!("ref")) >>
        s: type_description >>
        (Type::Ref(r(s)))
    )
);

named!(type_unit(Span) -> Type,
    map!(tag!("()"), |_| Type::Object(String::from("()"), UniqueID(0), vec!()))
);

named!(type_object(Span) -> Type,
    do_parse!(
        cs: class_spec >>
        (Type::Object(cs.ident.name.clone(), UniqueID(0), cs.types.clone()))
    )
);

named!(type_variable(Span) -> Type,
    map!(preceded!(tag!("'"), identifier), |s| Type::Variable(s.name.clone(), UniqueID(0), true))
);

named!(type_tuple(Span) -> Type,
    do_parse!(
        types: delimited!(tag!("("), separated_list_complete!(wscom!(tag!(",")), type_description), tag!(")")) >>
        (Type::Tuple(types))
    )
);

named!(type_record(Span) -> Type,
    do_parse!(
        types: delimited!(
            tag!("{"),
            wscom!(separated_list_complete!(wscom!(tag!(",")), do_parse!(
                i: identifier >>
                wscom!(tag!(":")) >>
                t: type_description >>
                ((i.name, t))
            ))),
            tag!("}")
        ) >>
        (Type::Record(types))
    )
);

named!(type_function(Span) -> Type,
    do_parse!(
        //args: delimited!(tag!("("), separated_list_complete!(wscom!(tag!(",")), type_description), tag!(")")) >>
        args: alt_complete!(type_tuple | type_variable | type_object) >>
        wscom!(tag!("->")) >>
        ret: type_description >>
        abi: abi_specifier >>
        (Type::Function(r(args), r(ret), abi))
    )
);

// TODO this used to be Overload, but it could potentially be useful as a contrained type of sorts
//named!(type_ambiguous(Span) -> Type,
//    map!(
//        delimited!(tag!("Ambiguous["), wscom!(separated_list_complete!(wscom!(tag!(",")), type_description)), tag!("]")),
//        |t| Type::Ambiguous(t)
//    )
//);

named!(abi_specifier(Span) -> ABI,
    map!(
        opt!(complete!(preceded!(wscom!(tag!("/")), identifier))),
        |s| ABI::from_ident(&s)
    )
);


named!(literal(Span) -> AST,
    alt_complete!(
        unit |
        nil |
        boolean |
        string |
        character |
        number |
        tuple |
        record |
        list
    )
);

named!(unit(Span) -> AST,
    value!(AST::Literal(Literal::Unit), tag_word!("()"))
);

named!(nil(Span) -> AST,
    value!(AST::Nil, tag_word!("nil"))
);

named!(boolean(Span) -> AST,
    alt!(
        value!(AST::Literal(Literal::Boolean(true)), tag_word!("true")) |
        value!(AST::Literal(Literal::Boolean(false)), tag_word!("false"))
    )
);

named!(escaped_character(Span) -> &[u8],
    alt!(
        tag!("\\") => { |_| &b"\\"[..] } |
        tag!("\"") => { |_| &b"\""[..] } |
        tag!("n")  => { |_| &b"\n"[..] } |
        tag!("r")  => { |_| &b"\r"[..] } |
        tag!("t")  => { |_| &b"\t"[..] } |
        tag!("0")  => { |_| &b"\0"[..] }
    )
);

named!(string_contents(Span) -> AST,
    map!(
        escaped_transform!(is_not!("\"\\"), '\\', escaped_character),
        |s| AST::Literal(Literal::String(String::from_utf8_lossy(&s).into_owned()))
    )
);

named!(string(Span) -> AST,
    delimited!(
        tag!("\""),
        alt!(
            string_contents |
            value!(AST::Literal(Literal::String(String::new())), tag!(""))
        ),
        tag!("\"")
    )
);


named!(character(Span) -> AST,
    map!(
        delimited!(
            tag!("\'"),
            alt!(
                map!(preceded!(tag!("\\"), escaped_character), |c| c[0] as char) |
                map!(anychar, |c| c as char)
            ),
            tag!("\'")
        ),
        |c| AST::Literal(Literal::Character(c))
    )
);


named!(number(Span) -> AST,
    alt_complete!(
        value!(AST::Literal(Literal::Real(std::f64::NEG_INFINITY)), tag_word!("-Inf")) |
        value!(AST::Literal(Literal::Real(std::f64::INFINITY)), tag_word!("Inf")) |
        value!(AST::Literal(Literal::Real(std::f64::NAN)), tag_word!("NaN")) |
        oct_number |
        hex_number |
        int_or_float_number
    )
);

named!(hex_number(Span) -> AST,
    map!(
        preceded!(tag!("0x"), hex_digit),
        |s| AST::Literal(Literal::Integer(isize::from_str_radix(str::from_utf8(&s.fragment).unwrap(), 16).unwrap()))
    )
);

named!(oct_number(Span) -> AST,
    map!(
        preceded!(tag!("0"), oct_digit),
        |s| AST::Literal(Literal::Integer(isize::from_str_radix(str::from_utf8(&s.fragment).unwrap(), 8).unwrap()))
    )
);

named!(int_or_float_number(Span) -> AST,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               digit,
               opt!(complete!(preceded!(tag!("."), digit)))
               //opt!(complete!(float_exponent))
            )
        ),
        |s| AST::number_from_utf8(&s.fragment)
    )
);

impl AST {
    fn number_from_utf8(s : &[u8]) -> Self {
        let n = str::from_utf8(s).unwrap();
        if let Ok(i) = isize::from_str_radix(n, 10) {
            AST::Literal(Literal::Integer(i))
        } else {
            AST::Literal(Literal::Real(f64::from_str(n).unwrap()))
        }
    }
}

named!(tuple(Span) -> AST,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("("),
            wscom!(separated_list_complete!(wscom!(tag!(",")), expression)),
            tag!(")")
        ) >>
        (AST::Tuple(Pos::new(pos), l))
    )
);

named!(record(Span) -> AST,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("{"),
            wscom!(record_field_assignments),
            return_error!(ErrorKind::Custom(ERR_IN_LIST), tag!("}"))
        ) >>
        (AST::Record(Pos::new(pos), l))
    )
);

named!(record_field_assignments(Span) -> Vec<(Ident, AST)>,
    separated_list_complete!(wscom!(tag!(",")), do_parse!(
        i: identifier >>
        wscom!(tag!("=")) >>
        e: expression >>
        ((i, e))
    ))
);

named!(list(Span) -> AST,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("["),
            wscom!(separated_list_complete!(wscom!(tag!(",")), expression)),
            tag!("]")
        ) >>
        (AST::List(Pos::new(pos), l))
    )
);


named!(not_reserved(Span) -> (),
    // TODO you need to figure out the add_error thing to raise a specific error
    //return_error!(ErrorKind::Custom(128), alt!(
    not!(alt!(
        tag_word!("do") |
        tag_word!("end") |
        tag_word!("while") |
        tag_word!("class") |
        tag_word!("import") |
        tag_word!("let") |
        tag_word!("type") |
        tag_word!("match") |
        tag_word!("with") |
        tag_word!("if") | tag_word!("then") | tag_word!("else") |
        tag_word!("try") | tag_word!("with") | tag_word!("raise") |
        tag_word!("for") | tag_word!("in") |
        tag_word!("fn") | tag_word!("decl")
    ))
);



named!(pattern(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        p: pattern_atomic >>
        a: opt!(preceded!(wscom!(tag!(":")), type_description)) >>
        (match a {
            Some(ty) => Pattern::new(Pos::new(pos), PatKind::Annotation(ty, r(p))),
            None => p,
        })
    )
);

named!(pattern_atomic(Span) -> Pattern,
    alt_complete!(
        value!(Pattern::new(Pos::empty(), PatKind::Wild), tag!("_")) |
        pattern_literal |
        pattern_enum_variant |
        pattern_tuple |
        pattern_record |
        pattern_binding
    )
);

named!(pattern_literal(Span) -> Pattern,
    map!(alt_complete!(
        unit |
        nil |
        boolean |
        string |
        character |
        number
    ), |l| Pattern::new(Pos::empty(), PatKind::Literal(l.get_literal())))
);

named!(pattern_binding(Span) -> Pattern,
    map!(identifier, |i| Pattern::new(Pos::empty(), PatKind::Binding(i)))
);

named!(pattern_resolve(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        left: identifier >>
        operations: many1!(preceded!(tag!("::"), identifier)) >>
        (operations.into_iter().fold(Pattern::new(Pos::new(pos), PatKind::Identifier(left)), |acc, i| Pattern::new(Pos::new(pos), PatKind::Resolve(r(acc), i, NodeID::generate()))))
    )
);

named!(pattern_enum_variant(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        p: pattern_resolve >>
        o: opt!(delimited!(
            tag!("("),
            separated_list_complete!(wscom!(tag!(",")), pattern),
            tag!(")")
        )) >>
        (match o {
            None => p,
            Some(l) => Pattern::new(Pos::new(pos), PatKind::EnumArgs(r(p), l))
        })
    )
);

named!(pattern_tuple(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("("),
            wscom!(separated_list_complete!(wscom!(tag!(",")), pattern)),
            tag!(")")
        ) >>
        (Pattern::new(Pos::new(pos), PatKind::Tuple(l)))
    )
);

named!(pattern_record(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("{"),
            wscom!(pattern_record_field_assignments),
            return_error!(ErrorKind::Custom(ERR_IN_LIST), tag!("}"))
        ) >>
        (Pattern::new(Pos::new(pos), PatKind::Record(l)))
    )
);

named!(pattern_record_field_assignments(Span) -> Vec<(Ident, Pattern)>,
    separated_list_complete!(wscom!(tag!(",")), do_parse!(
        i: identifier >>
        wscom!(tag!("=")) >>
        p: pattern >>
        ((i, p))
    ))
);




named!(line_comment(Span) -> Span,
    delimited!(tag!("//"), not_line_ending, peek!(line_ending))    //, |s| AST::Comment(String::from(str::from_utf8(s).unwrap())))
);

named!(block_comment(Span) -> Span,
    delimited!(
        tag!("/*"),
        recognize!(many0!(
           alt!(
                block_comment |
                recognize!(many_till!(anychar, peek!(alt!(tag!("/*") | tag!("*/")))))
            )
        )),
        tag!("*/")
    )
);


/*
named!(separator(Span) -> Span,
    recognize!(many0!(
        //alt!(take_while1!(is_ws) | comment)
        //alt!(take_while1!(is_ws) | delimited!(tag!("//"), not_line_ending, line_ending))
        //terminated!(sp!(alt!(line_comment | block_comment)), line_ending)
        //delimited!(space_or_comment, alt!(line_ending | tag!(";")), line_or_space_or_comment)
        delimited!(space_or_comment, line_ending, line_or_space_or_comment)
    ))
);
*/

named!(space_or_comment(Span) -> Span,
    recognize!(many0!(alt!(line_comment | block_comment | space)))
);

named!(line_or_space_or_comment(Span) -> Span,
    recognize!(many0!(alt!(line_comment | block_comment | multispace)))
);

named!(line_continuation(Span) -> Span,
    recognize!(tuple!(
        space_or_comment,
        many0!(opt!(tuple!(tag!("\\"), line_ending, space_or_comment)))
    ))
);



/*
macro_rules! named_method {
    ($name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => (
        fn $name<'a>( &self, i: $i ) -> IResult<$i, $o, u32> {
            $submac!(i, $($args)*)
        }
    );
}

struct Test(u32);

impl Test {
    fn test<'a>(&self, i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag!(i, "Hey")
    }

    named_method!(test2(Span<'a>) -> Span<'a>,
        tag!("Hey")
    );
}
*/

named!(alphanumeric_underscore(Span) -> Span,
    take_while1!(is_alphanumeric_underscore)
);

pub fn is_alpha_underscore(ch: u8) -> bool {
    ch == b'_' || is_alphabetic(ch)
}

pub fn is_alphanumeric_underscore(ch: u8) -> bool {
    ch == b'_' || is_alphanumeric(ch)
}

pub fn ident_from_span(span: &Span) -> Ident {
    Ident {
        name: String::from(str::from_utf8(&span.fragment).unwrap()),
    }
}



pub fn print_error(name: &str, err: nom::Err<Span, u32>) {
    match err {
        nom::Err::Incomplete(_) => println!("ParseError: incomplete input..."),
        nom::Err::Error(Context::Code(span, code)) |
        nom::Err::Failure(Context::Code(span, code)) => {
            println!("\x1B[1;31m{}:{}:{}: ParseError ({:?}) near {:?}\x1B[0m", name, span.line, span.get_utf8_column(), code, format_snippet(&span));
        },
        nom::Err::Error(list @ Context::List(_)) |
        nom::Err::Failure(list @ Context::List(_)) => {
            let errors = error_to_list(&list);
            let (span, _) = &errors[errors.len() - 1];
            println!("\x1B[1;31m{}:{}:{}: ParseError ({}) near {:?}\x1B[0m", name, span.line, span.get_utf8_column(), format_codes(&errors), format_snippet(&span));
        },
    }
}

pub fn format_snippet<'a>(span: &'a Span) -> String {
    let snippet = str::from_utf8(&span.fragment).unwrap();
    let index = snippet.find('\n').unwrap_or(snippet.len());
    String::from(snippet.get(..index).unwrap())
}

pub fn format_codes(codes: &Vec<(Span, ErrorKind)>) -> String {
    codes.iter().fold(String::new(), |acc, (_, code)| {
        let string = format!("{:?}", code);
        if &acc == "" {
            string
        } else {
            acc + " " + &string
        }
    })
}
