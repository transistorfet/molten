 

#[derive(Clone, Debug, PartialEq)]
pub struct Options<'a> {
    pub is_library: bool,
    pub libpath: Vec<&'a str>,
}


impl<'a> Options<'a> {
    pub fn new() -> Options<'a> {
        Options { is_library: false, libpath: vec!() }
    }

    pub fn add_lib_path(&mut self, path: &'a str) {
        self.libpath.push(path);
    }
}


