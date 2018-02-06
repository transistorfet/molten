

//use std::rc::Rc;
//use std::cell::RefCell;
use std::sync::Mutex;

#[derive(Clone, Debug, PartialEq)]
pub struct Options<'a> {
    pub is_library: bool,
    pub libpath: Vec<&'a str>,
}


//lazy_static! {
//    static ref _options: Mutex<Options<'static>> = Mutex::new(Options { is_library: false, libpath: vec!() });
//}

static mut _OPTIONS: Option<Options<'static>> = None;

impl Options<'static> {
    pub fn init() {
        unsafe {
            _OPTIONS = Some(Options { is_library: false, libpath: vec!(".", "lib") });
        }
    }

    pub fn as_ref() -> &'static mut Options<'static> {
        unsafe {
            _OPTIONS.as_mut().unwrap()
        }
    }

    pub fn add_lib_path(&mut self, path: &'static str) {
        self.libpath.push(path);
    }
}


