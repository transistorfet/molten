
pub struct ModuleDef;

impl ModuleDef {
    pub fn get_module_name(name: &str) -> String {
        name.replace(".", "_")
    }

    pub fn get_run_name(name: &str) -> String {
        format!("run_{}", name)
    }

    pub fn get_memo_name(name: &str) -> String {
        format!("memo.{}", name)
    }
}

