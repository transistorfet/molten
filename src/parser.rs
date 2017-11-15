

extern crate nom;
use nom::{ digit, hex_digit, oct_digit, line_ending, not_line_ending, space, multispace, is_alphanumeric, is_alphabetic, IResult, IError, ErrorKind };

extern crate std;
use std::f64;
use std::str;
use std::str::FromStr;


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
    Comment(String),

    Nil,
    Underscore,
    Boolean(bool),
    Integer(isize),
    Float(f64),
    String(String),
    List(Vec<AST>),

    Identifier(String),
    //Access(Box<AST>, Box<AST>),
    Invoke(String, Vec<AST>),
    Prefix(String, Box<AST>),
    Infix(String, Box<AST>, Box<AST>),
    If(Box<AST>, Box<AST>, Box<AST>),
    Match(Box<AST>, Vec<(AST, AST)>),
    For(String, Box<AST>, Box<AST>),
    Block(Vec<AST>),
    Function(Vec<(String, Option<String>, Option<AST>)>, Box<AST>),
    Class(String, Vec<AST>),

    Import(String),
    Definition((String, Option<String>), Box<AST>),
    While(Box<AST>, Box<AST>),
    Type(String, Vec<(String, Option<String>)>),
}


named!(pub parse<Vec<AST>>,
    complete!(ws!(do_parse!(
        e: many0!(statement) >>
        eof!() >>
        (e)
    )))
);

named!(statement<AST>,
    //separated_list!(ws!(tag!(",")), do_parse!(
    ws!(do_parse!(
        s: alt!(
            import |
            definition |
            whileloop |
            typedef |
            expression
        ) >>
        //eat_separator!("\n") >>
        separator >>
        (s)
    ))
);

named!(import<AST>,
    do_parse!(
        tag!("import") >>
        space >>
        e: map_str!(recognize!(do_parse!(
            identifier >>
            many0!(preceded!(tag!("."), identifier)) >>
            ()
        ))) >>
        (AST::Import(e))
    )
);

named!(definition<AST>,
    do_parse!(
        ws!(tag_word!("let")) >>
        i: identifier_typed >>
        t: opt!(preceded!(tag!(":"), identifier)) >>
        ws!(tag!("=")) >>
        e: expression >>
        (AST::Definition(i, Box::new(e)))
    )
);

named!(whileloop<AST>,
    do_parse!(
        ws!(tag_word!("while")) >>
        c: expression >>
        opt!(multispace) >>
        e: expression >>
        (AST::While(Box::new(c), Box::new(e)))
    )
);

named!(typedef<AST>,
    do_parse!(
        ws!(tag_word!("type")) >>
        i: identifier >>
        ws!(tag!("=")) >>
        s: alt!(
            map!(identifier_typed, |i| vec!(i)) |
            delimited!(ws!(tag!("{")), separated_list!(ws!(tag!(",")), identifier_typed), ws!(tag!("}")))
        ) >>
        (AST::Type(i, s))
    )
);



named!(expression<AST>,
    alt_complete!(
        noop |
        block |
        ifexpr |
        matchcase |
        forloop |
        function |
        class |
        infix
    )
);

named!(noop<AST>,
    value!(AST::Noop, tag_word!("noop"))
);

named!(block<AST>,
    delimited!(
        alt!(tag_word!("begin") | tag_word!("do") | tag!("{")),
        //ws!(tag!("do")),
        do_parse!(
            s: many0!(statement) >>
            (AST::Block(s))
        ),
        alt!(tag_word!("end") | tag!("}"))
        //ws!(tag!("end"))
    )
);

named!(ifexpr<AST>,
    do_parse!(
        ws!(tag_word!("if")) >>
        c: expression >>
        ws!(tag_word!("then")) >>
        t: expression >>
        ws!(tag_word!("else")) >>
        f: expression >>
        (AST::If(Box::new(c), Box::new(t), Box::new(f)))
    )
);

named!(matchcase<AST>,
    do_parse!(
        ws!(tag_word!("match")) >>
        c: expression >>
        ws!(tag_word!("with")) >>
        l: many1!(do_parse!(
            c: alt_complete!(value!(AST::Underscore, tag!("_")) | literal) >>
            ws!(tag!("->")) >>
            e: expression >>
            (c, e)
        )) >>
        (AST::Match(Box::new(c), l))
    )
);

named!(forloop<AST>,
    do_parse!(
        ws!(tag_word!("for")) >>
        i: identifier >>
        ws!(tag_word!("in")) >>
        l: expression >>
        opt!(multispace) >>
        e: expression >>
        (AST::For(i, Box::new(l), Box::new(e)))
    )
);

named!(function<AST>,
    do_parse!(
        ws!(tag_word!("fn")) >>
        l: identifier_list_defaults >>
        ws!(tag!("->")) >>
        e: expression >>
        (AST::Function(l, Box::new(e)))
    )
);

named!(identifier_list<Vec<(String, Option<String>)>>,
    separated_list!(ws!(tag!(",")), identifier_typed)
);

named!(identifier_list_defaults<Vec<(String, Option<String>, Option<AST>)>>,
    separated_list!(ws!(tag!(",")),
        do_parse!(
            i: identifier_typed >>
            d: opt!(preceded!(ws!(tag!("=")), expression)) >>
            ((i.0, i.1, d))
        )
    )
);

named!(class<AST>,
    do_parse!(
        ws!(tag_word!("class")) >>
        i: identifier >>
        ws!(tag!("{")) >>
        s: many0!(statement) >>
        ws!(tag!("}")) >>
        (AST::Class(i, s))
    )
);


named!(infix_op<String>,
    map_str!(
        sp!(alt!(
            tag!("+") |
            tag!("-") |
            tag!("*") |
            tag!("/") |
            tag!("^") |
            tag!("%") |
            tag!("==") |
            tag!("!=") |
            tag!("<") |
            tag!(">") |
            tag!("<=") |
            tag!(">=") |
            tag_word!("and") |
            tag_word!("or") |
            tag!("&") |
            tag!("|") |
            tag!("~") |
            tag!("<<") |
            tag!(">>")
            //tag!("..") |
        ))
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

    fn fold_op_old(left: AST, operations: Vec<(String, AST)>) -> Self {
        operations.into_iter().fold(left, |acc, pair| {
            AST::Infix(pair.0, Box::new(acc), Box::new(pair.1))
        })
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
                operands.push(AST::Infix(op, Box::new(r1), Box::new(r2)));
            }

            operators.push((next_op, p));
            operands.push(next_ast);
        }

        while !operators.is_empty() {
            let op = operators.pop().unwrap().0;
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            operands.push(AST::Infix(op, Box::new(r1), Box::new(r2)));
        }

        assert_eq!(operands.len(), 1);
        operands.pop().unwrap()
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
    alt_complete!(
        prefix |
        accessor
    )
);

named!(prefix_op<String>,
    map_str!(sp!(alt!(
        tag_word!("not") |
        tag!("~")
    )))
);

named!(prefix<AST>,
    do_parse!(
        op: prefix_op >>
        a: atomic >>
        (AST::Prefix(op, Box::new(a)))
    )
);

named!(accessor<AST>,
    do_parse!(
        left: subatomic >>
        operations: many0!(tuple!(map_str!(tag!(".")), subatomic)) >>
        (AST::fold_op(left, operations))
    )
);

named!(subatomic<AST>,
    alt_complete!(
        literal |
        invoke |
        map!(identifier, |s| AST::Identifier(s)) |
        delimited!(tag!("("), ws!(expression), tag!(")"))
    )
);

named!(invoke<AST>,
    do_parse!(
        s: identifier >>
        opt!(space) >>
        tag!("(") >>
        l: expression_list >>
        tag!(")") >>
        (AST::Invoke(s, l))
    )
);

named!(expression_list<Vec<AST>>,
    separated_list!(ws!(tag!(",")), expression)
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

named!(identifier_typed<(String, Option<String>)>,
    do_parse!(
        i: identifier >>
        t: opt!(preceded!(ws!(tag!(":")), identifier)) >>
        (i, t)
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
    value!(AST::Nil, tag_word!("nil"))
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
            is_not!("\""),
            tag!("\"")
        ),
        |s| AST::String(String::from(str::from_utf8(s).unwrap()))
    )
);

named!(number<AST>,
    alt_complete!(
        value!(AST::Float(std::f64::NEG_INFINITY), tag_word!("-Inf")) |
        value!(AST::Float(std::f64::INFINITY), tag_word!("Inf")) |
        value!(AST::Float(std::f64::NAN), tag_word!("NaN")) |
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
        }
        else {
            AST::Float(f64::from_str(n).unwrap())
        }
    }
}

named!(list<AST>,
    map!(
        delimited!(
            ws!(tag!("[")),
            separated_list!(ws!(tag!(",")), expression),
            ws!(tag!("]"))
        ),
        |e| AST::List(e)
    )
);




named!(separator,
    recognize!(many0!(
        //alt!(take_while1!(is_ws) | comment)
        //alt!(take_while1!(is_ws) | delimited!(tag!("//"), not_line_ending, line_ending))
        terminated!(sp!(line_comment), line_ending)
    ))
);

/*
named!(separator<AST>,
    do_parse!(
        take_while!(is_ws) >>
        opt!(preceded!(tag!("//"), not_line_ending)) >>
        line_ending >>
        (AST::Noop)
    )
);
*/


named!(line_comment<AST>,
    map!(delimited!(tag!("//"), not_line_ending, peek!(line_ending)), |s| AST::Comment(String::from(str::from_utf8(s).unwrap())))
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

 
