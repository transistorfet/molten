

extern crate nom;
use nom::{ digit, hex_digit, oct_digit, line_ending, not_line_ending, space, multispace, is_alphanumeric, is_alphabetic, is_space, IResult };

use std;
use std::f64;
use std::str;
use std::str::FromStr;

use types::Type;
use utils::UniqueID;


pub fn parse_or_error(name: &str, text: &[u8]) -> Vec<AST> {
    match parse(text) {
        IResult::Done(rem, _) if rem != [] => panic!("InternalError: unparsed input remaining: {:?}", rem),
        IResult::Done(rem, code) => code,
        res @ IResult::Error(_) => { print_error(name, text, nom::prepare_errors(text, res).unwrap()); panic!(""); },
        res @ _ => panic!("UnknownError: the parser returned an unknown result; {:?}", res),
    }
}

pub fn parse_index_or_error(name: &str, text: &[u8]) -> Vec<Decl> {
    match parse_index(text) {
        IResult::Done(rem, _) if rem != [] => panic!("InternalError: unparsed input remaining: {:?}", rem),
        IResult::Done(rem, decls) => decls,
        res @ IResult::Error(_) => { print_error(name, text, nom::prepare_errors(text, res).unwrap()); panic!(""); },
        res @ _ => panic!("UnknownError: the parser returned an unknown result; {:?}", res),
    }
}

///// Parser /////

named!(sp, eat_separator!(&b" \t"[..]));

#[macro_export]
macro_rules! sp (
    ($i:expr, $($args:tt)*) => (
        {
            sep!($i, sp, $($args)*)
        }
    )
);

#[macro_export]
macro_rules! wscom {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        use parser::multispace_comment;
        //terminated!($i, $submac!($($args)*), multispace_comment)
        //preceded!($i, multispace_comment, $submac!($($args)*))
        //sep!($i, multispace_comment, $submac!($($args)*))
        delimited!($i, multispace_comment, $submac!($($args)*), multispace_comment)
    });
    ($i:expr, $f:expr) => (
        wscom!($i, call!($f));
    );
}

#[macro_export]
macro_rules! wscoml {
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        use parser::multispace_comment;
        //terminated!($i, $submac!($($args)*), multispace_comment)
        preceded!($i, multispace_comment, $submac!($($args)*))
        //sep!($i, multispace_comment, $submac!($($args)*))
        //delimited!($i, multispace_comment, $submac!($($args)*), multispace_comment)
    });
    ($i:expr, $f:expr) => (
        wscom!($i, call!($f));
    );
}

#[macro_export]
macro_rules! tag_word (
    ($i:expr, $f:expr) => (
        {
            do_parse!($i,
                s: tag!($f) >>
                not!(peek!(alphanumeric_underscore)) >>
                ( s )
            )
        }
    )
);

#[macro_export]
macro_rules! map_str (
    ($i:expr, $($args:tt)*) => (
        {
            map!($i, $($args)*, |s| String::from(str::from_utf8(s).unwrap()))
        }
    )
);


#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Noop,
    //Comment(String),

    Underscore,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    Nil(Option<Type>),
    List(Vec<AST>),

    Identifier(String),
    Index(Box<AST>, Box<AST>, Option<Type>),
    Resolver(Box<AST>, String),
    Accessor(Box<AST>, String, Option<Type>),
    Invoke(Box<AST>, Vec<AST>, Option<Type>),
    SideEffect(String, Vec<AST>),
    //MethodIdentifier(String),
    //InvokeMethod(String, Vec<AST>, Option<Type>),
    //Prefix(String, Box<AST>),
    //Infix(String, Box<AST>, Box<AST>),
    Block(Vec<AST>),
    If(Box<AST>, Box<AST>, Box<AST>),
    Raise(Box<AST>),
    Try(Box<AST>, Vec<(AST, AST)>),
    Match(Box<AST>, Vec<(AST, AST)>),
    For(String, Box<AST>, Box<AST>, UniqueID),
    Function(Option<String>, Vec<(String, Option<Type>, Option<AST>)>, Option<Type>, Box<AST>, UniqueID),
    Class((String, Vec<Type>), Option<(String, Vec<Type>)>, Vec<AST>, UniqueID),

    Import(String, Vec<Decl>),
    Definition((String, Option<Type>), Box<AST>),
    Assignment(Box<AST>, Box<AST>),
    While(Box<AST>, Box<AST>),
    Type(String, Vec<(String, Option<Type>)>),
}


named!(pub parse<Vec<AST>>,
    complete!(do_parse!(
        e: many0!(statement) >>
        eof!() >>
        (e)
    ))
);

named!(statement<AST>,
    //separated_list!(ws!(tag!(",")), do_parse!(
    do_parse!(
        s: wscom!(alt_complete!(
            import |
            definition |
            assignment |
            whileloop |
            class |
            typedef |
            expression
            // TODO should class be here too?
            //value!(AST::Noop, multispace_comment)
        )) >>
        //eat_separator!("\n") >>
        separator >>
        (s)
    )
);

named!(import<AST>,
    do_parse!(
        wscom!(tag_word!("import")) >>
        e: map_str!(recognize!(separated_list!(tag!("."), identifier))) >>
        (AST::Import(e, vec!()))
    )
);

named!(definition<AST>,
    do_parse!(
        wscom!(tag_word!("let")) >>
        i: identifier_typed >>
        wscom!(tag!("=")) >>
        e: expression >>
        (AST::Definition(i, Box::new(e)))
    )
);

named!(assignment<AST>,
    do_parse!(
        o: subatomic_operation >>
        wscom!(tag!("=")) >>
        e: expression >>
        (AST::Assignment(Box::new(o), Box::new(e)))
    )
);

named!(whileloop<AST>,
    do_parse!(
        wscom!(tag_word!("while")) >>
        c: expression >>
        opt!(multispace_comment) >>
        e: expression >>
        (AST::While(Box::new(c), Box::new(e)))
    )
);

named!(class<AST>,
    do_parse!(
        wscom!(tag_word!("class")) >>
        i: class_identifier >>
        p: opt!(preceded!(wscom!(tag_word!("extends")), class_identifier)) >>
        wscom!(tag!("{")) >>
        s: many0!(statement) >>
        wscom!(tag!("}")) >>
        (AST::Class(i, p, s, UniqueID::generate()))
    )
);

named!(typedef<AST>,
    do_parse!(
        wscom!(tag_word!("type")) >>
        i: identifier >>
        wscom!(tag!("=")) >>
        s: alt!(
            map!(identifier_typed, |i| vec!(i)) |
            delimited!(wscom!(tag!("{")), separated_list!(wscom!(tag!(",")), identifier_typed), wscom!(tag!("}")))
        ) >>
        (AST::Type(i, s))
    )
);



named!(expression<AST>,
    alt_complete!(
        noop |
        block |
        ifexpr |
        trywith |
        raise |
        matchcase |
        forloop |
        function |
        infix
    )
);

named!(noop<AST>,
    value!(AST::Noop, tag_word!("noop"))
);

named!(block<AST>,
    delimited!(
        wscom!(alt!(tag_word!("begin") | tag!("{"))),
        do_parse!(
            s: many0!(statement) >>
            (AST::Block(s))
        ),
        wscom!(alt!(tag_word!("end") | tag!("}")))
    )
);

named!(ifexpr<AST>,
    do_parse!(
        wscom!(tag_word!("if")) >>
        c: expression >>
        wscom!(tag_word!("then")) >>
        t: expression >>
        f: opt!(preceded!(
            wscom!(tag_word!("else")),
            expression
        )) >>
        (AST::If(Box::new(c), Box::new(t), Box::new(if f.is_some() { f.unwrap() } else { AST::Noop })))
    )
);

named!(trywith<AST>,
    do_parse!(
        wscom!(tag_word!("try")) >>
        c: expression >>
        wscom!(tag_word!("with")) >>
        l: caselist >>
        (AST::Try(Box::new(c), l))
    )
);

named!(raise<AST>,
    do_parse!(
        wscom!(tag_word!("raise")) >>
        e: expression >>
        (AST::Raise(Box::new(e)))
    )
);

named!(matchcase<AST>,
    do_parse!(
        wscom!(tag_word!("match")) >>
        c: expression >>
        //wscom!(tag_word!("with")) >>
        //l: caselist >>
        l: delimited!(wscom!(tag!("{")), caselist, wscom!(tag!("}"))) >>
        (AST::Match(Box::new(c), l))
    )
);

named!(caselist<Vec<(AST, AST)>>,
    //separated_list!(wscom!(tag!(",")), do_parse!(
    many1!(do_parse!(
        //wscom!(tag!("|")) >>
        c: alt_complete!(value!(AST::Underscore, tag!("_")) | literal) >>
        wscom!(tag!("=>")) >>
        e: expression >>
        //wscom!(tag!(",")) >>
        (c, e)
    ))
);

named!(forloop<AST>,
    do_parse!(
        wscom!(tag_word!("for")) >>
        i: identifier >>
        wscom!(tag_word!("in")) >>
        l: expression >>
        opt!(multispace_comment) >>
        e: expression >>
        (AST::For(i, Box::new(l), Box::new(e), UniqueID::generate()))
    )
);

named!(function<AST>,
    do_parse!(
        wscom!(tag_word!("fn")) >>
        //n: opt!(identifier) >>
        l: alt_complete!(
            do_parse!(
                n: opt!(alt_complete!(identifier | any_op)) >>
                //n: opt!(identifier) >>
                l: delimited!(tag!("("), identifier_list_defaults, tag!(")")) >>
                ((n, l))
            ) |
            map!(identifier_list_defaults, |l| (None, l))
        ) >>
        r: opt!(preceded!(wscom!(tag!("->")), type_description)) >>
        e: alt_complete!(block | preceded!(wscom!(tag!("=>")), expression)) >>
        (AST::Function(l.0, l.1, r, Box::new(e), UniqueID::generate()))
    )
);
/*
named!(function<AST>,
    do_parse!(
        wscom!(tag_word!("fn")) >>
        l: identifier_list_defaults >>
        wscom!(tag!("=>")) >>
        e: expression >>
        (AST::Function(l, Box::new(e), UniqueID::generate(), None))
    )
);
*/
named!(identifier_list<Vec<(String, Option<Type>)>>,
    separated_list!(tag!(","), identifier_typed)
);

named!(identifier_list_defaults<Vec<(String, Option<Type>, Option<AST>)>>,
    separated_list!(tag!(","),
        do_parse!(
            i: identifier_typed >>
            d: opt!(preceded!(tag!("="), expression)) >>
            ((i.0, i.1, d))
        )
    )
);


named!(infix_op<String>,
    map_str!(
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

    fn fold_op(left: AST, operations: Vec<(String, AST)>) -> Self {
        let mut operands: Vec<AST> = vec!();
        let mut operators: Vec<(String, i32)> = vec!();
        operands.push(left);

        for (next_op, next_ast) in operations {
            let p = AST::precedence(next_op.as_str());

            while !operators.is_empty() && operators.last().unwrap().1 <= p {
                let op = operators.pop().unwrap().0;
                let r2 = operands.pop().unwrap();
                let r1 = operands.pop().unwrap();

                operands.push(AST::make_op(op, r1, r2))
            }

            operators.push((next_op, p));
            operands.push(next_ast);
        }

        while !operators.is_empty() {
            let op = operators.pop().unwrap().0;
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            operands.push(AST::make_op(op, r1, r2))
        }

        assert_eq!(operands.len(), 1);
        operands.pop().unwrap()
    }

    fn make_op(op: String, r1: AST, r2: AST) -> AST {
        match op.as_str() {
            "and" | "or" => AST::SideEffect(op, vec!(r1, r2)),
            _ => 
            //AST::Infix(op, Box::new(r1), Box::new(r2))
            AST::Invoke(Box::new(AST::Identifier(op)), vec!(r1, r2), None)
            //AST::Invoke(Box::new(AST::Accessor(Box::new(r1), op, None)), vec!(r2), None)
        }
    }
}

/*
#[macro_export]
macro_rules! infixer (
    ($i:expr, $op:expr, $($sub:tt)*) => (
        {
            do_parse!($i,
                left: $($sub)* >>
                operations: many0!(do_parse!(
                    op: call!($op) >>
                    right: $($sub)* >>
                    (op, right)
                )) >>
                (AST::fold_op(left, operations))
            )
        }
    )
);

named!(infix<AST>,
    infixer!(infix_op, alt!(atomic))
);
*/

named!(infix<AST>,
    do_parse!(
        left: atomic >>
        operations: many0!(tuple!(infix_op, atomic)) >>
        (AST::fold_op(left, operations))
    )
);

named!(atomic<AST>,
    wscom!(alt_complete!(
        prefix |
        subatomic_operation
        //index |
        //accessor |
        // TODO (expr), identifier, invoke(), literal
        //literal |
        //invoke |
        //subatomic
    ))
);

named!(prefix_op<String>,
    map_str!(alt!(
        tag_word!("not") |
        tag!("~")
    ))
);

named!(prefix<AST>,
    do_parse!(
        op: prefix_op >>
        a: atomic >>
        //(AST::Prefix(op, Box::new(a)))
        (AST::Invoke(Box::new(AST::Identifier(op)), vec!(a), None))
        //(AST::Invoke(Box::new(AST::Accessor(Box::new(a), op, None)), vec!(), None))
    )
);

named!(subatomic_operation<AST>,
    do_parse!(
        left: subatomic >>
        operations: many0!(alt_complete!(
            map!(delimited!(tag!("["), expression, tag!("]")), |e| SubOP::Index(e)) |
            map!(delimited!(tag!("("), expression_list, tag!(")")), |e| SubOP::Invoke(e)) |
            map!(preceded!(tag!("."), identifier), |s| SubOP::Accessor(s)) |
            map!(preceded!(tag!("::"), identifier), |s| SubOP::Resolver(s))
        )) >>
        (AST::fold_access(left, operations))
    )
);

enum SubOP {
    Index(AST),
    Invoke(Vec<AST>),
    Accessor(String),
    Resolver(String),
}

impl AST {
    fn fold_access(left: AST, operations: Vec<SubOP>) -> Self {
        let mut ret = left;
        for op in operations {
            match op {
                SubOP::Index(e) => ret = AST::Index(Box::new(ret), Box::new(e), None),
                SubOP::Invoke(e) => ret = AST::Invoke(Box::new(ret), e, None),
                SubOP::Accessor(name) => ret = AST::Accessor(Box::new(ret), name.clone(), None),
                SubOP::Resolver(name) => ret = AST::Resolver(Box::new(ret), name.clone()),
            }
        }
        ret
    }
}

named!(subatomic<AST>,
    alt_complete!(
        literal |
        map!(identifier, |s| AST::Identifier(s)) |
        delimited!(tag!("("), wscom!(expression), tag!(")"))
    )
);

named!(expression_list<Vec<AST>>,
    separated_list!(tag!(","), expression)
);

named!(identifier<String>,
    map_str!(
        do_parse!(
            not!(reserved) >>
            s: recognize!(preceded!(
                take_while1!(is_alpha_underscore),
                take_while!(is_alphanumeric_underscore)
            )) >>
            (s)
        )
    )
);

named!(class_identifier<(String, Vec<Type>)>,
    do_parse!(
        i: identifier >>
        p: opt!(complete!(delimited!(tag!("["), separated_list!(wscom!(tag!(",")), type_description), tag!("]")))) >>
        ((i, p.unwrap_or(vec!())))
    )
);

named!(identifier_typed<(String, Option<Type>)>,
    wscom!(do_parse!(
        i: identifier >>
        t: opt!(preceded!(wscom!(tag!(":")), type_description)) >>
        (i, t)
    ))
);

named!(any_op<String>,
    alt!(infix_op | prefix_op | map_str!(alt!(tag!("[]") | tag!("::"))))
);

pub fn parse_type(s: &str) -> Option<Type> {
    match type_description(s.as_bytes()) {
        IResult::Done(_, t) => Some(t),
        _ => panic!("Error Parsing Type: {:?}", s)
    }
}

named!(type_description<Type>,
    alt_complete!(
        type_function |
        //type_list |
        type_variable |
        type_object
    )
);

named!(type_object<Type>,
    do_parse!(
        c: class_identifier >>
        (Type::Object(c.0, c.1))
    )
);

named!(type_variable<Type>,
    map!(preceded!(tag!("'"), identifier), |s| Type::Variable(s))
);

named!(type_list<Type>,
    map!(
        delimited!(tag!("List["), wscom!(type_description), tag!("]")),
        |t| Type::List(Box::new(t))
    )
);

named!(type_function<Type>,
    wscom!(do_parse!(
        args: delimited!(tag!("("), separated_list!(wscom!(tag!(",")), type_description), tag!(")")) >>
        wscom!(tag!("->")) >>
        ret: type_description >>
        (Type::Function(args, Box::new(ret)))
    ))
);

named!(type_overload<Type>,
    map!(
        delimited!(tag!("Overload["), wscom!(separated_list!(wscom!(tag!(",")), type_description)), tag!("]")),
        |t| Type::Overload(t)
    )
);

named!(reserved,
    alt!(
        tag_word!("do") | tag_word!("end") | tag_word!("while")
    )
);



named!(literal<AST>,
    alt_complete!(
        nil |
        boolean |
        string |
        number |
        list
    )
);

named!(nil<AST>,
    value!(AST::Nil(None), tag_word!("nil"))
);

named!(boolean<AST>,
    alt!(
        value!(AST::Boolean(true), tag_word!("true")) |
        value!(AST::Boolean(false), tag_word!("false"))
    )
);

named!(string<AST>,
    map!(
        delimited!(
            tag!("\""),
            escaped_transform!(is_not!("\"\\"), '\\',
                alt!(
                    tag!("\\") => { |_| &b"\\"[..] } |
                    tag!("\"") => { |_| &b"\""[..] } |
                    tag!("n")  => { |_| &b"\n"[..] } |
                    tag!("r")  => { |_| &b"\r"[..] } |
                    tag!("t")  => { |_| &b"\t"[..] }
                )
            ),
            tag!("\"")
        ),
        |s| AST::String(String::from_utf8_lossy(&s).into_owned())
    )
);

named!(number<AST>,
    alt_complete!(
        value!(AST::Real(std::f64::NEG_INFINITY), tag_word!("-Inf")) |
        value!(AST::Real(std::f64::INFINITY), tag_word!("Inf")) |
        value!(AST::Real(std::f64::NAN), tag_word!("NaN")) |
        oct_number |
        hex_number |
        int_or_float_number
    )
);

named!(hex_number<AST>,
    map!(
        preceded!(tag!("0x"), hex_digit),
        |s| AST::Integer(isize::from_str_radix(str::from_utf8(s).unwrap(), 16).unwrap())
    )
);

named!(oct_number<AST>,
    map!(
        preceded!(tag!("0"), oct_digit),
        |s| AST::Integer(isize::from_str_radix(str::from_utf8(s).unwrap(), 8).unwrap())
    )
);

named!(int_or_float_number<AST>,
    map!(
        recognize!(
            tuple!(
               opt!(tag!("-")),
               digit,
               opt!(complete!(preceded!(tag!("."), digit)))
               //opt!(complete!(float_exponent))
            )
        ),
        AST::number_from_utf8
    )
);

impl AST {
    fn number_from_utf8(s : &[u8]) -> Self {
        let n = str::from_utf8(s).unwrap();
        if let Ok(i) = isize::from_str_radix(n, 10) {
            AST::Integer(i)
        } else {
            AST::Real(f64::from_str(n).unwrap())
        }
    }
}

named!(list<AST>,
    map!(
        delimited!(
            wscom!(tag!("[")),
            separated_list!(wscom!(tag!(",")), expression),
            wscom!(tag!("]"))
        ),
        |e| AST::List(e)
    )
);




named!(separator,
    recognize!(many0!(
        //alt!(take_while1!(is_ws) | comment)
        //alt!(take_while1!(is_ws) | delimited!(tag!("//"), not_line_ending, line_ending))
        //terminated!(sp!(alt!(line_comment | block_comment)), line_ending)
        delimited!(space_comment, alt!(line_ending | tag!(";")), multispace_comment)
    ))
);



named!(space_comment,
    recognize!(many0!(alt!(line_comment | block_comment | space)))
);

named!(multispace_comment,
map!(
    recognize!(many0!(alt!(line_comment | block_comment | multispace))),
    |s| { count_lines(s); s }
)
);

named!(line_comment,
    delimited!(tag!("//"), not_line_ending, peek!(line_ending))    //, |s| AST::Comment(String::from(str::from_utf8(s).unwrap())))
);

// TODO allow for nested comments
named!(block_comment,
    delimited!(tag!("/*"), take_until!("*/"), tag!("*/"))              //, |s| AST::Comment(String::from(str::from_utf8(s).unwrap())))
);



named!(alphanumeric_underscore,
    take_while1!(is_alphanumeric_underscore)
);

pub fn is_alpha_underscore(ch: u8) -> bool {
    ch == b'_' || is_alphabetic(ch)
}

pub fn is_alphanumeric_underscore(ch: u8) -> bool {
    ch == b'_' || is_alphanumeric(ch)
}

pub fn is_not_colon(ch: u8) -> bool {
    ch != b':' && !is_space(ch)
}


static mut _lines: usize = 0;

pub fn count_lines(text: &[u8]) {
    for ch in text {
        if *ch == '\n' as u8 {
            //*lines.get_mut() += 1;
            unsafe { _lines += 1; }
        }
    }
}



pub fn print_error(name: &str, text: &[u8], errors: Vec<(nom::ErrorKind, usize, usize)>) {
    for error in errors {
        print_error_info(name, text, error);
    }
}

pub fn print_error_info(name: &str, text: &[u8], err: (nom::ErrorKind, usize, usize)) {
    let mut lines = 0;
    let mut start = 0;
    for i in 0 .. err.1 {
        if text[i] == b'\n' {
            lines += 1;
            start = i + 1;
        }
    }

    let mut end = err.2 - 1;
    for i in err.1 .. err.2 {
        if text[i] == b'\n' {
            end = i;
            break;
        }
    }

    println!("{}:{}:{}: ParseError: error with \"{:?}\" near {:?}", name, lines, err.1 - start, err.0, String::from(str::from_utf8(&text[start .. end]).unwrap()));
}


///// Index Summaries /////

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Symbol(String, Type),
    Class((String, Vec<Type>), Option<(String, Vec<Type>)>, Vec<Decl>),
}

named!(pub parse_index<Vec<Decl>>,
    complete!(do_parse!(
        e: many0!(declarations) >>
        eof!() >>
        (e)
    ))
);

named!(declarations<Decl>,
    do_parse!(
        s: wscom!(alt_complete!(
            symbol_decl |
            class_decl
        )) >>
        separator >>
        (s)
    )
);

named!(symbol_name<String>,
    map_str!(recognize!(take_while!(is_not_colon)))
);

named!(symbol_decl<Decl>,
    do_parse!(
        wscom!(tag_word!("decl")) >>
        n: symbol_name >>
        wscom!(tag!(":")) >>
        t: alt_complete!(type_overload | type_description) >>
        (Decl::Symbol(n, t))
    )
);

named!(class_decl<Decl>,
    do_parse!(
        wscom!(tag_word!("class")) >>
        i: class_identifier >>
        p: opt!(preceded!(wscom!(tag_word!("extends")), class_identifier)) >>
        wscom!(tag!("{")) >>
        s: many0!(symbol_decl) >>
        wscom!(tag!("}")) >>
        (Decl::Class(i, p, s))
    )
); 

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("
                5 * 3 + 8 / 100
            ".as_bytes()),
            IResult::Done(&b""[..], vec!(
                AST::Invoke(Box::new(AST::Identifier(String::from("+"))), vec!(
                    AST::Invoke(Box::new(AST::Identifier(String::from("*"))), vec!(AST::Integer(5), AST::Integer(3)), None),
                    AST::Invoke(Box::new(AST::Identifier(String::from("/"))), vec!(AST::Integer(8), AST::Integer(100)), None)
                ), None)
            ))
        );
    }
}


