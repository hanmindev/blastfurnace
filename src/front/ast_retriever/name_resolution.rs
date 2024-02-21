use crate::front::ast_retriever::name_resolution::scope_table::ScopeTable;
use crate::front::ast_types::visitor::Visitable;
use crate::front::ast_types::Module;

mod lib;
mod resolver;
mod scope_table;

pub fn resolve_module(module: &mut Module) {
    let mut scope_table = ScopeTable::new();
    module.visit(&mut scope_table).unwrap();
}
