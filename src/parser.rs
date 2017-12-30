

extern crate nom;
use nom::{ digit, hex_digit, oct_digit, line_ending, not_line_ending, space, multispace, is_alphanumeric, is_alphabetic, IResult };

extern crate std;
use std::f64;
use std::str;
use std::str::FromStr;

use types::Type;
use utils::{ UniqueID };


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
macro_rules! wscomp {
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

    Nil,
    Underscore,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    List(Vec<AST>),

    Identifier(String),
    Index(Box<AST>, Box<AST>),
    Resolver(Box<AST>, String),
    Accessor(Box<AST>, String, Option<Type>),
    Invoke(Box<AST>, Vec<AST>, Option<Type>),
    //Prefix(String, Box<AST>),
    //Infix(String, Box<AST>, Box<AST>),
    Block(Vec<AST>),
    If(Box<AST>, Box<AST>, Box<AST>),
    Raise(Box<AST>),
    Try(Box<AST>, Vec<(AST, AST)>),
    Match(Box<AST>, Vec<(AST, AST)>),
    For(String, Box<AST>, Box<AST>, UniqueID),
    Function(Option<String>, Vec<(String, Option<Type>, Option<AST>)>, Option<Type>, Box<AST>, UniqueID),
    Class(String, Option<String>, Vec<AST>, UniqueID),

    Import(String),
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
        s: wscom!(alt!(
            import |
            definition |
            assignment |
            whileloop |
            typedef |
            // TODO should class be here too?
            expression
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
        (AST::Import(e))
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
        class |
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
        wscom!(tag_word!("else")) >>
        f: expression >>
        (AST::If(Box::new(c), Box::new(t), Box::new(f)))
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
        wscom!(tag_word!("with")) >>
        l: caselist >>
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
        l: alt!(
            do_parse!(
                n: opt!(identifier) >>
                l: delimited!(tag!("("), identifier_list_defaults, tag!(")")) >>
                ((n, l))
            ) |
            map!(identifier_list_defaults, |l| (None, l))
        ) >>
        r: opt!(preceded!(wscom!(tag!("->")), type_description)) >>
        e: alt!(block | preceded!(wscom!(tag!("=>")), expression)) >>
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

named!(class<AST>,
    do_parse!(
        wscom!(tag_word!("class")) >>
        i: identifier >>
        p: opt!(preceded!(wscom!(tag_word!("extends")),
            identifier)) >>
        wscom!(tag!("{")) >>
        s: many0!(statement) >>
        wscom!(tag!("}")) >>
        (AST::Class(i, p, s, UniqueID::generate()))
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
            tag!("<") |
            tag!(">") |
            tag!("<=") |
            tag!(">=") |
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
                //operands.push(AST::Infix(op, Box::new(r1), Box::new(r2)));
                operands.push(AST::Invoke(Box::new(AST::Identifier(op)), vec!(r1, r2), None));
            }

            operators.push((next_op, p));
            operands.push(next_ast);
        }

        while !operators.is_empty() {
            let op = operators.pop().unwrap().0;
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            //operands.push(AST::Infix(op, Box::new(r1), Box::new(r2)));
            operands.push(AST::Invoke(Box::new(AST::Identifier(op)), vec!(r1, r2), None));
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
    )
);

named!(subatomic_operation<AST>,
    do_parse!(
        left: subatomic >>
        operations: many0!(alt!(
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
                SubOP::Index(e) => ret = AST::Index(Box::new(ret), Box::new(e)),
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

named!(identifier_typed<(String, Option<Type>)>,
    wscom!(do_parse!(
        i: identifier >>
        t: opt!(preceded!(wscom!(tag!(":")), type_description)) >>
        (i, t)
    ))
);

pub fn parse_type(s: &str) -> Option<Type> {
    match type_description(s.as_bytes()) {
        IResult::Done(_, t) => Some(t),
        _ => panic!("Error Parsing Type: {:?}", s)
    }
}

named!(type_description<Type>,
    alt!(
        type_function |
        type_variable |
        type_concrete
    )
);

named!(type_concrete<Type>,
    map!(identifier, |s| Type::Concrete(s))
);

named!(type_variable<Type>,
    map!(preceded!(tag!("'"), identifier), |s| Type::Variable(s))
);

named!(type_function<Type>,
    wscom!(do_parse!(
        args: delimited!(tag!("("), separated_list!(wscom!(tag!(",")), type_description), tag!(")")) >>
        wscom!(tag!("->")) >>
        ret: type_description >>
        (Type::Function(args, Box::new(ret)))
    ))
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
        }
        else {
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


static mut lines: usize = 0;

pub fn count_lines(text: &[u8]) {
    for ch in text {
        if *ch == '\n' as u8 {
            //*lines.get_mut() += 1;
            unsafe { lines += 1; }
        }
    }
}

/*
pub type UniqueID = usize;
static mut _next_id: UniqueID = 10;
pub fn generate_id() -> UniqueID {
    unsafe {
        _next_id += 1;
        //format!("anon{}", _next_id)
        _next_id
    }
}
*/
 
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


