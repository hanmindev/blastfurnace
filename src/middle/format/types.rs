use crate::front::ast_types::{FnDef, StructDef, VarDecl};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GlobalName {
    pub module: Rc<str>,
    pub name: Rc<str>,
}
#[derive(Debug, PartialEq)]
pub struct Program {
    pub public_functions: HashSet<Rc<GlobalName>>,
    pub function_definitions: HashMap<Rc<GlobalName>, FnDef>,
    pub struct_definitions: HashMap<Rc<GlobalName>, StructDef>,
    pub global_var_definitions: HashMap<Rc<GlobalName>, VarDecl>,
}
