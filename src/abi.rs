

use std::fmt;
use std::str;

use crate::types::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ABI {
    Unknown,
    Molten,
    C,
    Cpp,
    Rust,
}


impl ABI {
    pub fn from_str(name: &Option<String>) -> ABI {
        match name.as_deref() {
            None => ABI::Molten,
            Some(name) => match name {
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
        Type::Variable(_) => panic!("Placeholder type variables shouldn't appear here"),
        Type::Universal(_, _) => format!("V"),
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

