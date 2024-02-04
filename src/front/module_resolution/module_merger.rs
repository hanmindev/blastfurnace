use crate::front::module_resolution::definition_table::DefinitionTable;
use crate::front::module_resolution::resolvers::Resolvable;
use crate::front::syntax::ast_types::{
    FnDef, GlobalResolvedName, Module, ResolvedName, StructDef, VarDecl,
};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct ModuleMerger {
    pub package_name: String,
    module_path: String,
    global_name_table: HashMap<Rc<ResolvedName>, Rc<GlobalResolvedName>>,
    pub definition_table: DefinitionTable,
}

impl ModuleMerger {
    pub fn new(package_name: &str) -> ModuleMerger {
        ModuleMerger {
            package_name: package_name.to_string(),
            module_path: String::new(),
            global_name_table: HashMap::new(),
            definition_table: DefinitionTable::new(),
        }
    }

    fn switch_module(&mut self, path: &str) {
        self.module_path = path.to_string();
        self.global_name_table.clear()
    }

    pub fn get_path(&self) -> &String {
        &self.module_path
    }

    pub fn register_global_name(
        &mut self,
        local_name: Rc<ResolvedName>,
        global_name: Rc<GlobalResolvedName>,
        should_be_0: bool,
    ) {
        if should_be_0 && !local_name.starts_with("0_") {
            panic!("Local name should start with 0_");
        }

        self.global_name_table.insert(local_name, global_name);
    }

    pub fn resolve_global_name(
        &mut self,
        resolved_name: &Rc<ResolvedName>,
    ) -> Rc<GlobalResolvedName> {
        return if let Some(s) = self.global_name_table.get(resolved_name) {
            Rc::clone(s)
        } else {
            let g = Rc::from(GlobalResolvedName {
                module: self.get_path().clone(),
                name: resolved_name.clone(),
            });
            self.register_global_name(Rc::clone(resolved_name), Rc::clone(&g), false);
            g
        };
    }

    pub fn merge_module(&mut self, path: &str, module: &mut Module) {
        self.switch_module(path);
        module
            .resolve_module(self)
            .expect("Failed to resolve module");
    }

    pub fn insert_fn_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: FnDef,
    ) {
        self.definition_table
            .function_definitions
            .insert(global_resolved_name, definition);
    }

    pub fn insert_struct_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: StructDef,
    ) {
        self.definition_table
            .struct_definitions
            .insert(global_resolved_name, definition);
    }

    pub fn insert_global_var_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: VarDecl,
    ) {
        self.definition_table
            .global_var_definitions
            .insert(global_resolved_name, definition);
    }
}
