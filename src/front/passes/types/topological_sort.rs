use crate::front::ast_types::GlobalResolvedName;
use crate::front::passes::types::var_def_table::VarDefTable;
use either::Either;
use std::collections::HashSet;
use std::rc::Rc;

fn topological_sort_visit(
    name: &Rc<GlobalResolvedName>,
    table: &VarDefTable,
    visited: &mut HashSet<Rc<GlobalResolvedName>>,
    result: &mut Vec<Rc<GlobalResolvedName>>,
) {
    visited.insert(Rc::clone(name));

    if let Some(var_type) = table.var_types.get(name) {
        if let Either::Right(type_dep) = &var_type.types_ {
            for dep in &type_dep.deps {
                if !visited.contains(dep) {
                    topological_sort_visit(dep, table, visited, result);
                }
            }
        }
    }

    result.push(Rc::clone(name));
}

pub fn topological_sort(table: &VarDefTable) -> Vec<Rc<GlobalResolvedName>> {
    let mut result = Vec::new();
    let mut visited = HashSet::new();

    for (name, _) in &table.var_types {
        if !visited.contains(name) {
            topological_sort_visit(name, table, &mut visited, &mut result);
        }
    }

    result
}
