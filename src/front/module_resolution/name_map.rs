use crate::front::syntax::ast_types::{FnDef, GlobalResolvedName, StructDef, VarDecl};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct NameMap {
    pub path: String,
    pub function_definitions: HashMap<Rc<GlobalResolvedName>, FnDef>,
    pub struct_definitions: HashMap<Rc<GlobalResolvedName>, StructDef>,
    pub global_var_definitions: HashMap<Rc<GlobalResolvedName>, VarDecl>,
}

impl NameMap {
    pub fn new() -> NameMap {
        NameMap {
            path: String::new(),
            function_definitions: Default::default(),
            struct_definitions: Default::default(),
            global_var_definitions: Default::default(),
        }
    }

    pub fn set_path(&mut self, path: &String) {
        self.path = path.clone();
    }
}
