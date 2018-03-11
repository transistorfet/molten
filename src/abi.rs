 

use std::fmt;

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
}

impl fmt::Display for ABI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ABI::Molten => write!(f, ""),
            _ => write!(f, " / {}", self.as_str()),
        }
    }
}

