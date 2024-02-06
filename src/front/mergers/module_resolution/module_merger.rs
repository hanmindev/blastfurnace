use crate::front::ast_retriever::retriever::{ModuleNode, ModuleSource};
use crate::front::mergers::module_resolution::merged_module::MergedModule;
use crate::front::mergers::module_resolution::resolvers::Resolvable;
use crate::front::ast_retriever::ast_types::{
    FnDef, GlobalResolvedName, Module, ResolvedName, StructDef, VarDecl,
};
use std::collections::{HashMap, LinkedList};
use std::rc::Rc;

#[derive(Debug)]
pub enum ModuleMergeError {
    ModuleNotAttached(ModuleSource, String),
}

pub type ModuleMergeResult<T> = Result<T, ModuleMergeError>;

#[derive(Debug)]
pub struct ModuleMerger {
    pub package_name: String,
    module_source: ModuleSource,
    global_name_table: HashMap<Rc<ResolvedName>, Rc<GlobalResolvedName>>,
    merged_module: Option<MergedModule>,
    visibility_rules: HashMap<ModuleSource, ModuleSource>, // to call public methods in module of path (key), module_path needs prefix (value)
}

impl ModuleMerger {
    pub fn new(package_name: &str) -> ModuleMerger {
        ModuleMerger {
            package_name: package_name.to_string(),
            module_source: String::new(),
            global_name_table: HashMap::new(),
            merged_module: None,
            visibility_rules: HashMap::new(),
        }
    }

    fn switch_module(&mut self, module_source: &str) {
        self.module_source = module_source.to_string();
        self.global_name_table.clear()
    }

    pub fn get_path(&self) -> &String {
        &self.module_source
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

    fn rec_create_visibility_rules(
        &mut self,
        module_source: &str,
        modules: &HashMap<ModuleSource, ModuleNode>,
    ) -> ModuleMergeResult<LinkedList<ModuleSource>> {
        let module_node = modules.get(module_source).unwrap();

        let mut visible_above: LinkedList<ModuleSource> =
            LinkedList::from([module_source.to_string()]);
        let mut visible_below: LinkedList<ModuleSource> = LinkedList::new();

        for (name, mut module) in &module_node.submodules {
            let mut visible = self.rec_create_visibility_rules(&name, modules)?;
            if let Some(mod_) = &mut module {
                if *mod_ {
                    visible_above.append(&mut visible)
                } else {
                    visible_below.append(&mut visible)
                }
            } else {
                Err(ModuleMergeError::ModuleNotAttached(
                    name.to_string(),
                    "Module is not attached to any parent module!".to_string(),
                ))?;
            }
        }

        for name in visible_below {
            self.visibility_rules
                .insert(name, module_source.to_string());
        }

        return Ok(visible_above);
    }

    pub fn can_call(&self, target_module_source: &str) -> bool {
        if target_module_source.starts_with("/root") {
            if let Some(module_source) = self.visibility_rules.get(target_module_source) {
                return self.get_path().starts_with(module_source);
            }
            return false;
        }
        return true;
    }

    pub fn merge_modules(
        &mut self,
        mut modules: HashMap<ModuleSource, ModuleNode>,
    ) -> ModuleMergeResult<MergedModule> {
        for name in self.rec_create_visibility_rules("/root", &mut modules)? {
            self.visibility_rules.insert(name, "/root".to_string());
        }
        self.merged_module = Some(MergedModule::new());

        for (module_source, mut module) in modules {
            self.switch_module(&module_source);
            module
                .module
                .unwrap()
                .resolve_module(self)
                .expect("Expected Module, got None");
        }

        Ok(self.merged_module.take().unwrap())
    }

    pub fn insert_fn_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: FnDef,
        is_public: bool,
    ) {
        let mut definitions = if is_public && self.module_source == "/root" {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .public_definitions
                .function_definitions
        } else {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .private_definitions
                .function_definitions
        };

        definitions.insert(global_resolved_name, definition);
    }

    pub fn insert_struct_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: StructDef,
        is_public: bool,
    ) {
        let mut definitions = if is_public && self.module_source == "/root" {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .public_definitions
                .struct_definitions
        } else {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .private_definitions
                .struct_definitions
        };
        definitions.insert(global_resolved_name, definition);
    }

    pub fn insert_global_var_definition(
        &mut self,
        global_resolved_name: Rc<GlobalResolvedName>,
        definition: VarDecl,
        is_public: bool,
    ) {
        let mut definitions = if is_public && self.module_source == "/root" {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .public_definitions
                .global_var_definitions
        } else {
            &mut self
                .merged_module
                .as_mut()
                .unwrap()
                .private_definitions
                .global_var_definitions
        };
        definitions.insert(global_resolved_name, definition);
    }
}
