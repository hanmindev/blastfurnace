use crate::front::module_resolution::name_map::NameMap;
use crate::front::syntax::ast_types::{
    FnDef, GlobalResolvedName, ResolvedName, StructDef, VarDecl, VarDef,
};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct ModuleMerger {
    pub mp: HashMap<Rc<ResolvedName>, Rc<GlobalResolvedName>>,
    pub name_map: NameMap,
}

impl ModuleMerger {
    pub fn new() -> ModuleMerger {
        ModuleMerger {
            mp: HashMap::new(),
            name_map: NameMap::new(),
        }
    }
}
