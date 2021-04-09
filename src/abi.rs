

use std::fmt;
use std::str;

//extern crate nom;
//use nom::{ digit };
//use nom::types::CompleteByteSlice;
//use parser;

use hir::Ident;
use types::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ABI {
    Unknown,
    Molten,
    C,
    Cpp,
    Rust,
}


impl ABI {
    pub fn from_ident(name: &Option<Ident>) -> ABI {
        match *name {
            None => ABI::Molten,
            Some(ref name) => match name.as_str() {
                "" | "Molten" | "M" => ABI::Molten,
                "C" => ABI::C,
                "C++" => ABI::Cpp,
                "Rust" => ABI::Rust,
                _ => panic!("Unknown ABI: {:?}", name)
            }
        }
    }

    pub fn as_str(&self) -> &str {
        match *self {
            ABI::Unknown => "unknown",
            ABI::Molten => "Molten",
            ABI::C => "C",
            ABI::Cpp => "C++",
            ABI::Rust => "Rust",
        }
    }

    pub fn compare(&self, other: &ABI) -> Option<ABI> {
        if self == &ABI::Unknown {
            Some(other.clone())
        } else if other == &ABI::Unknown || self == other {
            Some(self.clone())
        } else {
            None
        }
    }

    pub fn can_overload(&self) -> bool {
        match *self {
            ABI::Cpp |
            ABI::Molten => true,
            ABI::C |
            ABI::Rust |
            ABI::Unknown => false,
        }
    }

    pub fn mangle_name(&self, name: &str, argtypes: &Type, funcdefs: i32) -> String {
        match *self {
            ABI::Molten => molten_mangle_name(name, argtypes, funcdefs),
            // TODO C++, etc
            _ => String::from(name),
        }
    }
}

impl fmt::Display for ABI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ABI::Molten => write!(f, ""),
            _ => write!(f, " / {}", self.as_str()),
        }
    }
}


pub fn molten_mangle_name(name: &str, argtypes: &Type, funcdefs: i32) -> String {
    if funcdefs >= 2 {
        format!("_Z{}{}{}", name.len(), name, molten_mangle_type(argtypes))
    } else {
        String::from(name)
    }
}

pub fn molten_mangle_type(ttype: &Type) -> String {
    match ttype {
        // TODO add type paramaters into name
        Type::Variable(_, _, _) => format!("V"),
        Type::Object(name, _, _types) => format!("N{}{}", name.len(), name),
        Type::Tuple(types) => {
            let mut tuple = String::from("");
            for ttype in types {
                tuple = tuple + &molten_mangle_type(ttype);
            }
            format!("T{}{}", tuple.len(), tuple)
        },
        // TODO this isn't complete, you have to deal with other types
        _ => String::from("")
    }
}

/*
pub fn molten_unmangle_name(name: &str) -> Option<String> {
    named!(unmangle(parser::Span) -> String,
        preceded!(tag!("_Z"),
            map!(
                length_bytes!(map!(digit, |s| usize::from_str_radix(str::from_utf8(&s.fragment).unwrap(), 10).unwrap())),
                |s| parser::span_to_string(s)
            )
        )
    );
    match unmangle(parser::Span::new(CompleteByteSlice(name.as_bytes()))) {
        Ok((_, value)) => Some(value),
        _ => None,
    }
}
*/

