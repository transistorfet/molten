 

use std::fmt;
use std::str;

extern crate nom;
use nom::{ digit };

use parser;
use types::Type;

//pub struct ABI(i32)

#[derive(Clone, Debug, PartialEq)]
pub enum ABI {
    Unknown,
    Molten,
    C,
    Cpp,
    Rust,
}


impl ABI {
    pub fn get(name: &Option<String>) -> ABI {
        match *name {
            None => ABI::Molten,
            Some(ref name) =>  match name.as_str() {
                "" | "molten" => ABI::Molten,
                "C" => ABI::C,
                "C++" => ABI::Cpp,
                "rust" => ABI::Rust,
                _ => panic!("Unknown ABI: {:?}", name)
            }
        }
    }

    // TODO there must be an easier way to do this
    pub fn as_str(&self) -> &str {
        match *self {
            ABI::Unknown => "unknown",
            ABI::Molten => "molten",
            ABI::C => "C",
            ABI::Cpp => "C++",
            ABI::Rust => "rust",
        }
    }

    pub fn mangle_name(&self, name: &str, argtypes: &Vec<Type>, funcdefs: i32) -> String {
        match *self {
            ABI::Molten => molten_mangle_name(name, argtypes, funcdefs),
            // TODO C++, etc
            _ => String::from(name),
        }
    }

    pub fn unmangle_name(&self, name: &str) -> Option<String> {
        match *self {
            ABI::Molten => molten_unmangle_name(name),
            // TODO C++, etc
            _ => None,
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


pub fn molten_mangle_name(name: &str, argtypes: &Vec<Type>, funcdefs: i32) -> String {
    if funcdefs >= 2 {
        let mut args = String::from("");
        for ttype in argtypes {
            args = args + &match *ttype {
                // TODO add type paramaters into name
                Type::Variable(_, _) => format!("V"),
                Type::Object(ref name, ref types) => format!("N{}{}", name.len(), name),
                // TODO this isn't complete, you have to deal with other types
                _ => String::from(""),
            };
        }
        format!("_Z{}{}{}", name.len(), name, args)
    } else {
        String::from(name)
    }
}

pub fn molten_unmangle_name(name: &str) -> Option<String> {
    named!(unmangle(parser::Span) -> String,
        preceded!(tag!("_Z"),
            map_str!(length_bytes!(map!(digit, |s| usize::from_str_radix(str::from_utf8(s.fragment).unwrap(), 10).unwrap())))
        )
    );
    match unmangle(parser::Span::new(name.as_bytes())) {
        nom::IResult::Done(_, value) => Some(value),
        _ => None,
    }
}


