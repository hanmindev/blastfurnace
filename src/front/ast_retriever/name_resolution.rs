use crate::front::ast_retriever::name_resolution::scope_table::ScopeTable;
use crate::front::ast_types::Module;
use crate::front::ast_types::visitor::Visitable;

mod lib;
mod resolvers;
mod scope_table;

pub fn resolve_module(module: &mut Module) {
    let mut scope_table = ScopeTable::new();
    module.visit(&mut scope_table).unwrap();
}
