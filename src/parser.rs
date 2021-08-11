

use nom::{
    character::{
        is_alphabetic,
        is_alphanumeric,
        complete::{
            digit1,
            hex_digit1,
            oct_digit1,
            line_ending,
            not_line_ending,
            anychar,
            space1,
            multispace1,
        },
    },
    bytes::complete::{
        take_while,
        take_while1,
    },
    error::{
        Error as NomError,
        ErrorKind,
    },
};

use nom_locate::LocatedSpan;

use std;
use std::f64;
use std::str;
use std::str::FromStr;

use crate::abi::ABI;
use crate::types::Type;
use crate::misc::{ r, UniqueID };
use crate::ast::{ Pos, RawArgument, AST };
use crate::hir::{ NodeID, Mutability, Visibility, AssignType, Literal, WhereClause, Pattern, PatKind };


///// Parsing Macros /////

pub type Span<'a> = LocatedSpan<&'a str>;


#[macro_export]
macro_rules! wscom {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        delimited!($i, line_or_space_or_comment, $submac!($($args)*), line_or_space_or_comment)
    });

    ($i:expr, $f:expr) => (
        wscom!($i, call!($f));
    );
}

#[macro_export]
macro_rules! wscoml {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        preceded!($i, line_or_space_or_comment, $submac!($($args)*))
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

pub fn parse_or_error(name: &str, text: &str) -> Vec<AST> {
    let span = Span::new(text);
    match parse(span) {
        Ok((rem, _)) if rem.fragment() != &"" => panic!("InternalError: unparsed input remaining: {:?}", rem),
        Ok((_, code)) => code,
        Err(err) => { print_error(name, err); panic!("") },
    }
}


//pub fn parse(i: Span) -> IResult<Span, Vec<AST>, NomError<Span>> {
named!(pub parse(Span) -> Vec<AST>,
    do_parse!(
        e: statement_list >>
        eof!() >>
        (e)
    )
);

named!(statement_list(Span) -> Vec<AST>,
    do_parse!(
        l: wscom!(separated_list0!(terminator, statement)) >>
        t: opt!(complete!(terminator)) >>
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
    alt!(
        complete!(module) |
        complete!(import) |
        complete!(class) |
        complete!(typealias) |
        complete!(typeenum) |
        complete!(traitdef) |
        complete!(traitimpl) |
        complete!(declare) |
        complete!(methods) |
        complete!(definition) |
        complete!(assignment) |
        complete!(expression)
    )
);

named!(module(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("module")) >>
        e: recognize!(separated_list0!(tag!("."), identifier)) >>
        (AST::ModuleDecl(Pos::new(pos), ident_from_span(&e)))
    )
);

named!(import(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("import")) >>
        e: recognize!(separated_list0!(tag!("."), identifier)) >>
        (AST::Import(Pos::new(pos), ident_from_span(&e), vec!()))
    )
);

named!(definition(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("let")) >>
        m: mutability >>
        i: identifier_typed >>
        e: preceded!(
            wscom!(tag!("=")),
            expression
        ) >>
        (AST::Definition(
            Pos::new(pos),
            m,
            i.1,
            i.2,
            r(e)
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
        e: return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_WHILE) */, expression) >>
        (AST::While(Pos::new(pos), r(c), r(e)))
    )
);

named!(class(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("class")) >>
        c: type_object >>
        p: opt!(preceded!(wscom!(tag_word!("extends")), type_object)) >>
        w: opt_where_clause >>
        wscom!(tag!("{")) >>
        b: many0!(wscom!(alt!(
            typealias |
            classfield |
            declare |
            function
        ))) >>
        return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_CLASS) */, tag!("}")) >>
        (AST::Class(Pos::new(pos), c, p, w, b))
    )
);

named!(classfield(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("val")) >>
        m: mutability >>
        i: identifier_typed >>
        (AST::Field(
            Pos::new(pos),
            m,
            i.1,
            i.2
        ))
    )
);

named!(typealias(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("type")) >>
        d: type_object >>
        wscom!(tag!("=")) >>
        ts: type_description >>
        (AST::TypeAlias(Pos::new(pos), d, ts))
    )
);

named!(typeenum(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("enum")) >>
        t: type_object >>
        w: opt_where_clause >>
        wscom!(tag!("=")) >>
        wscom!(opt!(tag!("|"))) >>
        ev: separated_list0!(complete!(wscom!(tag!("|"))), enum_variant) >>
        (AST::Enum(Pos::new(pos), t, w, ev))
    )
);

named!(enum_variant(Span) -> (Pos, String, Option<Type>),
    do_parse!(
        pos: position!() >>
        i: identifier >>
        t: opt!(complete!(delimited!(tag!("("), separated_list0!(wscom!(tag!(",")), type_description), tag!(")")))) >>
        ((Pos::new(pos), i, t.map(|t| Type::Tuple(t))))
    )
);

named!(traitdef(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("trait")) >>
        n: identifier >>
        wscom!(tag!("{")) >>
        b: many0!(wscom!(
            declare
        )) >>
        return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_CLASS) */, tag!("}")) >>
        (AST::TraitDef(Pos::new(pos), n, b))
    )
);

named!(traitimpl(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("impl")) >>
        n: identifier >>
        wscom!(tag_word!("for")) >>
        t: type_description >>
        w: opt_where_clause >>
        wscom!(tag!("{")) >>
        b: many0!(wscom!(
            function
        )) >>
        return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_CLASS) */, tag!("}")) >>
        (AST::TraitImpl(Pos::new(pos), n, t, w, b))
    )
);

named!(methods(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("methods")) >>
        c: type_object >>
        w: opt_where_clause >>
        wscom!(tag!("{")) >>
        b: many0!(wscom!(alt!(
            declare |
            function
        ))) >>
        return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_METHODS) */, tag!("}")) >>
        (AST::Methods(Pos::new(pos), c, w, b))
    )
);


named!(expression_list(Span) -> Vec<AST>,
    separated_list0!(wscom!(tag!(",")), expression)
);

named!(expression(Span) -> AST,
    alt!(
        //underscore |
        ifexpr |
        trywith |
        matchcase |
        whileloop |
        forloop |
        newinstance |
        function |
        reference |
        raise |
        annotation |
        infix
    )
);



//named!(underscore(Span) -> AST,
//    value!(AST::Underscore, tag!("_"))
//);

named!(block_vec(Span) -> Vec<AST>,
    delimited!(
        alt!(tag_word!("begin") | tag!("{")),
        wscom!(statement_list),
        alt!(tag_word!("end") | tag!("}"))
    )
);

named!(block(Span) -> AST,
    do_parse!(
        pos: position!() >>
        v: block_vec >>
        (AST::Block(Pos::new(pos), v))
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
        return_error!(ErrorKind::Tag, //ErrorKind::Custom(ERR_IN_TRY),
            tag!("{")
        ) >>
        l: caselist >>
        return_error!(ErrorKind::Tag, //ErrorKind::Custom(ERR_IN_TRY),
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
        return_error!(ErrorKind::Tag, //ErrorKind::Custom(ERR_IN_MATCH),
            tag!("{")
        ) >>
        l: caselist >>
        return_error!(ErrorKind::Tag, //ErrorKind::Custom(ERR_IN_MATCH),
            tag!("}")
        ) >>
        (AST::Match(Pos::new(pos), r(c), l))
    )
);

named!(caselist(Span) -> Vec<(Pattern, AST)>,
    //separated_list0!(wscom!(tag!("|")), do_parse!(
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
        e: return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_FOR) */, expression) >>
        (AST::For(Pos::new(pos), i, r(l), r(e)))
    )
);

named!(newinstance(Span) -> AST,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("new")) >>
        cs: type_object >>
        a: delimited!(tag!("("), expression_list, tag!(")")) >>
        (AST::New(Pos::new(pos), cs, a))
    )
);

named!(declare(Span) -> AST,
    do_parse!(
        pos: position!() >>
        vis: visibility >>
        wscom!(tag_word!("decl")) >>
        n: alt!(identifier | operator_func_identifier) >>
        t: type_function >>
        w: opt_where_clause >>
        (AST::Declare(Pos::new(pos), vis, n, t, w))
    )
);

named!(function(Span) -> AST,
    do_parse!(
        pos: position!() >>
        vis: visibility >>
        wscom!(tag_word!("fn")) >>
        l: alt!(
            do_parse!(
                n: opt!(alt!(identifier | operator_func_identifier)) >>
                l: delimited!(tag!("("), argument_list, tag!(")")) >>
                ((n, l))
            ) |
            map!(argument_list, |l| (None, l))
        ) >>
        rt: opt!(preceded!(wscom!(tag!("->")), type_description)) >>
        a: abi_specifier >>
        w: opt_where_clause >>
        e: alt!(
            preceded!(wscom!(tag!("=>")), return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_FUNC) */,
                map!(expression, |s| vec!(s)))) |
            return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_FUNC) */,
                wscoml!(block_vec))
        ) >>
        (AST::Function(Pos::new(pos), vis, l.0, l.1, rt, e, a, w))
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
        wscom!(complete!(tag!(":"))) >>
        t: type_description >>
        (AST::Annotation(t, r(e)))
    )
);

named!(argument_list(Span) -> Vec<RawArgument>,
    separated_list0!(wscom!(tag!(",")),
        do_parse!(
            i: identifier_typed >>
            // TODO add default arguments
            //d: opt!(preceded!(wscom!(tag!("=")), expression)) >>
            (RawArgument::new(i.0, i.1, i.2))
        )
    )
);

named!(where_clause(Span) -> WhereClause,
    do_parse!(
        pos: position!() >>
        wscom!(tag_word!("where")) >>
        c: separated_list1!(wscom!(tag!(",")),
            do_parse!(
                v: identifier >>
                wscom!(tag!(":")) >>
                c: identifier >>
                ((v, c))
            )
        ) >>
        (WhereClause::new(Pos::new(pos), c))
    )
);

named!(opt_where_clause(Span) -> WhereClause,
    map!(opt!(where_clause), |w| w.unwrap_or(WhereClause::empty()))
);


named!(infix_op(Span) -> String,
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

    fn fold_op(left: AST, operations: Vec<(Span, String, AST)>) -> Self {
        let mut operands: Vec<AST> = vec!();
        let mut operators: Vec<(Pos, String, i32)> = vec!();
        operands.push(left);

        for (span, next_op, next_ast) in operations {
            let pos  = Pos::new(span);
            let p = AST::precedence(&next_op);

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

    fn make_op(pos: Pos, op: String, r1: AST, r2: AST) -> AST {
        match op.as_str() {
            "and" | "or" => AST::SideEffect(pos, op, vec!(r1, r2)),
            _ =>
            //AST::Infix(pos, op, r(r1), r(r2))
            AST::Invoke(pos, r(AST::Identifier(pos, op)), vec!(r1, r2))
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
    alt!(
        prefix |
        subatomic_operation
    )
);

named!(prefix_op(Span) -> String,
    map_ident!(alt!(
        tag_word!("not") |
        tag!("~")
    ))
);

named!(prefix(Span) -> AST,
    do_parse!(
        pos: position!() >>
        op: prefix_op >>
        opt!(space1) >>
        a: atomic >>
        //(AST::Prefix(op, r(a)))
        (AST::Invoke(Pos::new(pos), r(AST::Identifier(Pos::new(pos), op)), vec!(a)))
    )
);

named!(subatomic_operation(Span) -> AST,
    do_parse!(
        left: subatomic >>
        operations: many0!(alt!(
            map!(delimited!(tag!("["), tuple!(position!(), wscom!(expression)), tag!("]")), |(p, e)| SubOP::Index(Pos::new(p), e)) |
            map!(delimited!(tag!("("), tuple!(position!(), wscom!(expression_list)), tag!(")")), |(p, e)| SubOP::Invoke(Pos::new(p), e)) |
            map!(preceded!(tag!("."), tuple!(position!(), alt!(identifier | map!(digit1, |s| ident_from_span(&s))))), |(p, s)| SubOP::Accessor(Pos::new(p), s)) |
            map!(preceded!(tag!("::"), tuple!(position!(), identifier)), |(p, s)| SubOP::Resolver(Pos::new(p), s))
        )) >>
        (AST::fold_access(left, operations))
    )
);

enum SubOP {
    Index(Pos, AST),
    Invoke(Pos, Vec<AST>),
    Accessor(Pos, String),
    Resolver(Pos, String),
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
    alt!(
        bracketed_expression |
        record_update |
        block |
        dereference |
        literal |
        identifier_node
    )
);

named!(bracketed_expression(Span) -> AST,
    // This extra AST element is how we distinguish between an intentional method call vs an access followed by a regular call
    // If the expression is surrounded in brackets, it will not be treated as a method call when it's converted by the refinery
    map!(
        delimited!(tag!("("), wscom!(expression), tag!(")")),
        |e| AST::Bracketed(r(e))
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
        return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_LIST) */, tag!("}"))
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

named!(identifier(Span) -> String,
    do_parse!(
        not_reserved >>
        s: recognize!(preceded!(
            call!(take_while1(is_alpha_underscore)),
            call!(take_while(is_alphanumeric_underscore))
        )) >>
        (ident_from_span(&s))
    )
);

named!(identifier_typed(Span) -> (Pos, String, Option<Type>),
    do_parse!(
        pos: position!() >>
        i: identifier >>
        t: opt!(preceded!(wscom!(tag!(":")), type_description)) >>
        (Pos::new(pos), i, t)
    )
);

named!(operator_func_identifier(Span) -> String,
    alt!(infix_op | prefix_op | map_ident!(alt!(tag!("[]") | tag!("::"))))
);

named!(mutability(Span) -> Mutability,
    map!(
        opt!(wscom!(tag_word!("mut"))),
        |m| if m.is_some() { Mutability::Mutable } else { Mutability::Immutable }
    )
);

named!(visibility(Span) -> Visibility,
    map!(
        opt!(wscom!(tag_word!("pub"))),
        |vis| if vis.is_some() { Visibility::Public } else { Visibility::Private }
    )
);


pub fn parse_type(s: &str) -> Option<Type> {
    match complete_type_description(Span::new(s)) {
        Ok((_, t)) => Some(t),
        e => panic!("Error Parsing Type: {:?}   [{:?}]", s, e)
    }
}

named!(complete_type_description(Span) -> Type,
    do_parse!(
        t: type_description >>
        eof!() >>
        (t)
    )
);

named!(type_description(Span) -> Type,
    alt!(
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
    map!(complete!(tag!("()")), |_| Type::Object(String::from("()"), UniqueID(0), vec!()))
);

named!(type_object(Span) -> Type,
    do_parse!(
        i: identifier >>
        p: opt!(complete!(delimited!(tag!("<"), separated_list0!(wscom!(tag!(",")), type_description), tag!(">")))) >>
        (Type::Object(i, UniqueID(0), p.unwrap_or(vec!())))
    )
);

named!(type_variable(Span) -> Type,
    map!(preceded!(tag!("'"), identifier), move |s| Type::Universal(s, UniqueID(0)))
);

named!(type_tuple(Span) -> Type,
    do_parse!(
        types: delimited!(tag!("("), separated_list0!(wscom!(tag!(",")), type_description), tag!(")")) >>
        (Type::Tuple(types))
    )
);

named!(type_record(Span) -> Type,
    do_parse!(
        types: delimited!(
            tag!("{"),
            wscom!(separated_list0!(wscom!(tag!(",")), do_parse!(
                i: identifier >>
                wscom!(tag!(":")) >>
                t: type_description >>
                ((i, t))
            ))),
            tag!("}")
        ) >>
        (Type::Record(types))
    )
);

named!(type_function(Span) -> Type,
    complete!(do_parse!(
        //args: delimited!(tag!("("), separated_list0!(wscom!(tag!(",")), type_description), tag!(")")) >>
        args: alt!(type_tuple | type_variable | type_object) >>
        wscom!(tag!("->")) >>
        ret: type_description >>
        abi: abi_specifier >>
        (Type::Function(r(args), r(ret), abi))
    ))
);

// TODO this used to be Overload, but it could potentially be useful as a contrained type of sorts
//named!(type_ambiguous(Span) -> Type,
//    map!(
//        delimited!(tag!("Ambiguous["), wscom!(separated_list0!(wscom!(tag!(",")), type_description)), tag!("]")),
//        |t| Type::Ambiguous(t)
//    )
//);

named!(abi_specifier(Span) -> ABI,
    map!(
        opt!(complete!(preceded!(wscom!(tag!("/")), identifier))),
        |s| ABI::from_str(&s)
    )
);


named!(literal(Span) -> AST,
    alt!(
        unit |
        nil |
        boolean |
        string |
        character |
        number |
        tuple |
        record |
        array
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

named!(escaped_character(Span) -> &str,
    alt!(
        tag!("\\") => { |_| &"\\"[..] } |
        tag!("\"") => { |_| &"\""[..] } |
        tag!("n")  => { |_| &"\n"[..] } |
        tag!("r")  => { |_| &"\r"[..] } |
        tag!("t")  => { |_| &"\t"[..] } |
        tag!("0")  => { |_| &"\0"[..] }
    )
);

named!(string_contents(Span) -> AST,
    map!(
        escaped_transform!(is_not!("\"\\"), '\\', escaped_character),
        |s| AST::Literal(Literal::String(String::from(s)))
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
                map!(preceded!(tag!("\\"), escaped_character), |c| c[0..1].chars().next().unwrap()) |
                map!(anychar, |c| c)
            ),
            tag!("\'")
        ),
        |c| AST::Literal(Literal::Character(c))
    )
);


named!(number(Span) -> AST,
    alt!(
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
        preceded!(tag!("0x"), hex_digit1),
        |s| AST::Literal(Literal::Integer(isize::from_str_radix(s.fragment(), 16).unwrap()))
    )
);

named!(oct_number(Span) -> AST,
    map!(
        preceded!(tag!("0"), oct_digit1),
        |s| AST::Literal(Literal::Integer(isize::from_str_radix(s.fragment(), 8).unwrap()))
    )
);

named!(int_or_float_number(Span) -> AST,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               digit1,
               opt!(complete!(preceded!(tag!("."), digit1)))
               //opt!(complete!(float_exponent))
            )
        ),
        |s| AST::number_from_utf8(s.fragment())
    )
);

impl AST {
    fn number_from_utf8(s: &str) -> Self {
        if let Ok(i) = isize::from_str_radix(s, 10) {
            AST::Literal(Literal::Integer(i))
        } else {
            AST::Literal(Literal::Real(f64::from_str(s).unwrap()))
        }
    }
}

named!(tuple(Span) -> AST,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("("),
            wscom!(separated_list0!(wscom!(tag!(",")), expression)),
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
            return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_LIST)*/, tag!("}"))
        ) >>
        (AST::Record(Pos::new(pos), l))
    )
);

named!(record_field_assignments(Span) -> Vec<(String, AST)>,
    separated_list0!(wscom!(tag!(",")), do_parse!(
        i: identifier >>
        wscom!(tag!("=")) >>
        e: expression >>
        ((i, e))
    ))
);

named!(array(Span) -> AST,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("["),
            wscom!(separated_list0!(wscom!(tag!(",")), expression)),
            tag!("]")
        ) >>
        (AST::Array(Pos::new(pos), l))
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
    alt!(
        value!(Pattern::new(Pos::empty(), PatKind::Wild), tag!("_")) |
        pattern_literal |
        pattern_enum_variant |
        pattern_tuple |
        pattern_record |
        pattern_binding
    )
);

named!(pattern_literal(Span) -> Pattern,
    map!(alt!(
        unit |
        nil |
        boolean |
        string |
        character |
        number
    ), |l| Pattern::new(Pos::empty(), PatKind::Literal(l.get_literal(), NodeID::generate())))
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
            separated_list0!(wscom!(tag!(",")), pattern),
            tag!(")")
        )) >>
        (match o {
            None => p,
            Some(l) => Pattern::new(Pos::new(pos), PatKind::EnumArgs(r(p), l, NodeID::generate()))
        })
    )
);

named!(pattern_tuple(Span) -> Pattern,
    do_parse!(
        pos: position!() >>
        l: delimited!(
            tag!("("),
            wscom!(separated_list0!(wscom!(tag!(",")), pattern)),
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
            return_error!(ErrorKind::Tag /*ErrorKind::Custom(ERR_IN_LIST) */, tag!("}"))
        ) >>
        (Pattern::new(Pos::new(pos), PatKind::Record(l)))
    )
);

named!(pattern_record_field_assignments(Span) -> Vec<(String, Pattern)>,
    separated_list0!(wscom!(tag!(",")), do_parse!(
        i: identifier >>
        wscom!(tag!("=")) >>
        p: pattern >>
        ((i, p))
    ))
);




named!(line_comment(Span) -> Span,
    complete!(delimited!(tag!("//"), not_line_ending, peek!(line_ending)))    //, |s| AST::Comment(String::from(str::from_utf8(s).unwrap())))
);

named!(block_comment(Span) -> Span,
    complete!(delimited!(
        tag!("/*"),
        recognize!(tuple!(
            recognize!(many0!(tuple!(non_block_comment_chars, block_comment))),
            non_block_comment_chars
        )),
        tag!("*/")
    ))
);

named!(non_block_comment_chars(Span) -> Span,
    recognize!(many_till!(anychar, peek!(alt!(tag!("/*") | tag!("*/")))))
);

named!(space_or_comment(Span) -> Span,
    recognize!(many0!(alt!(line_comment | block_comment | space1)))
);

named!(line_or_space_or_comment(Span) -> Span,
    recognize!(many0!(alt!(line_comment | block_comment | multispace1)))
);

named!(line_continuation(Span) -> Span,
    recognize!(tuple!(many0!(recognize!(tuple!(space_or_comment, tag!("\\"), line_ending))), space_or_comment))
);




named!(alphanumeric_underscore(Span) -> Span,
    call!(take_while1(is_alphanumeric_underscore))
);

pub fn is_alpha_underscore(ch: char) -> bool {
    ch == '_' || is_alphabetic(ch as u8)
}

pub fn is_alphanumeric_underscore(ch: char) -> bool {
    ch == '_' || is_alphanumeric(ch as u8)
}

pub fn ident_from_span(span: &Span) -> String {
    span.fragment().to_string()
}



pub fn print_error<'a>(name: &str, err: nom::Err<NomError<Span<'a>>>) {
    match err {
        nom::Err::Error(error) =>
            println!("\x1B[1;31m{}:{}:{}: ParseError ({:?}) near {:?}\x1B[0m", name, error.input.location_line(), error.input.get_utf8_column(), error.code, format_snippet(&error.input)),
        _ =>
            println!("\x1B[1;31mParseError: {:?}", err),
    }
}

pub fn format_snippet<'a>(span: &'a Span) -> String {
    let snippet = span.fragment();
    let index = snippet.find('\n').unwrap_or(snippet.len());
    String::from(snippet.get(..index).unwrap())
}

