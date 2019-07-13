
use std::fmt;

// Boxed References

pub type R<T> = Box<T>;

pub fn r<T>(t: T) -> R<T> {
    R::new(t)
}


// UniqueID

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct UniqueID(pub usize);

static mut _NEXT_ID: usize = 10;

impl UniqueID {
    pub fn generate() -> UniqueID {
        unsafe {
            _NEXT_ID += 1;
            //format!("anon{}", _next_id)
            UniqueID(_NEXT_ID)
        }
    }

    pub fn to_string(&self) -> String {
        (self.0 as u64).to_string()
    }
}

impl fmt::Display for UniqueID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

