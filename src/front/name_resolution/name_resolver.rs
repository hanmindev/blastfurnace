use crate::front::name_resolution::resolvers::Resolvable;
use crate::front::name_resolution::scope_table::ScopeTable;
use crate::front::ast_retriever::syntax::ast_types::Module;

pub fn resolve_module(module: &mut Module) {
    let mut scope_table = ScopeTable::new();
    module.resolve_name(&mut scope_table).unwrap();
}
