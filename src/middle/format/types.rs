use crate::front::ast_types::{FnDef, StructDef, VarDecl};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub type GlobalName = String;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub public_functions: HashSet<GlobalName>,
    pub function_definitions: HashMap<GlobalName, FnDef>,
    pub struct_definitions: HashMap<GlobalName, StructDef>,
    pub global_var_definitions: HashMap<GlobalName, VarDecl>,
}
