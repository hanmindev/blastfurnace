use crate::front::ast_types::{Block, Definition};
use crate::front::ast_types::Reference;

#[derive(Debug, PartialEq)]
pub struct Module {
    pub mods: Vec<ModuleImport>,
    pub uses: Vec<Use>,
    pub public_definitions: Vec<Definition>,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct ModuleImport {
    pub public: bool,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct UseElement {
    pub origin_name: String,
    pub imported_name: Reference,
}

#[derive(Debug, PartialEq)]
pub struct Use {
    pub path: Vec<String>,
    pub elements: Vec<UseElement>,
}
