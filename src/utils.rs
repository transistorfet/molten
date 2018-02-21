
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UniqueID(pub usize);

static mut _next_id: usize = 10;

impl UniqueID {
    pub fn generate() -> UniqueID {
        unsafe {
            _next_id += 1;
            //format!("anon{}", _next_id)
            UniqueID(_next_id)
        }
    }

    pub fn to_string(&self) -> String {
        (self.0 as i64).to_string()
    }
}

impl fmt::Display for UniqueID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
     
