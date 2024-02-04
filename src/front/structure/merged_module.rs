use crate::front::module_resolution::name_map::NameMap;
use crate::front::syntax::ast_types::{FnDef, GlobalResolvedName, StructDef, VarDecl, VarDef};
use std::collections::HashMap;

#[derive(Debug)]
pub struct MergedModule {
    pub name_map: NameMap,
}

impl MergedModule {
    pub fn new() -> MergedModule {
        MergedModule {
            name_map: NameMap::new(),
        }
    }
}
