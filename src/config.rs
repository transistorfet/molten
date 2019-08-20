
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EmitAs {
    LLIR,
    Obj,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Options<'a> {
    pub debug: bool,
    pub is_library: bool,
    pub libpath: Vec<&'a str>,
    pub format: EmitAs,
    pub optlevel: u32,
    pub no_gc: bool,
    pub linkfile_only: bool,
}


//lazy_static! {
//    static ref _options: Mutex<Options<'static>> = Mutex::new(Options { is_library: false, libpath: vec!() });
//}

static mut _OPTIONS: Option<Options<'static>> = None;

impl Options<'static> {
    pub fn init() {
        unsafe {
            _OPTIONS = Some(Options {
                debug: false,
                is_library: false,
                libpath: vec!(".", "lib"),
                format: EmitAs::LLIR,
                optlevel: 0,
                no_gc: false,
                linkfile_only: false,
            });
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


