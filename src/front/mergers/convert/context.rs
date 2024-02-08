use std::rc::Rc;
use crate::front::ast_types::{GlobalResolvedName, NamePath, Reference};
use crate::front::mergers::convert::global_name_updater;
use crate::front::mergers::definition_table::DefinitionTable;
use crate::middle::format::ir_types::{Address, AddressOrigin};

pub struct Context<'a> {
    pub fn_name: String,
    block_count: u32,
    var_count: u32,
    definition_table: &'a DefinitionTable<Rc<GlobalResolvedName>>,
}

impl Context<'_> {
    pub fn new<'a>(fn_name: &str, definition_table: &'a DefinitionTable<Rc<GlobalResolvedName>>) -> Context<'a> {
        Context {
            fn_name: fn_name.to_string(),
            block_count: 0,
            var_count: 0,
            definition_table,
        }
    }

    pub fn use_block(&mut self) -> u32 {
        let block = self.block_count;
        self.block_count += 1;
        block
    }

    pub fn get_parameter_variable(&self, function_name: &Rc<GlobalResolvedName>, index: u32) -> Address {
        let fn_ = self.definition_table.function_definitions.get(function_name).unwrap();
        self.convert_var_name(&fn_.args[index as usize].name)
    }

    pub fn create_variable(&mut self) -> Address {
        let a = Address {
            name: AddressOrigin::CtxGenerated(self.fn_name.clone(), self.var_count),
            offset: 0,
        };
        self.var_count += 1;
        a
    }

    pub fn get_if_variable(&mut self) -> Address {
        Address {
            name: AddressOrigin::If,
            offset: 0,
        }
    }

    pub fn get_return_variable(&mut self) -> Address {
        Address {
            name: AddressOrigin::Return,
            offset: 0,
        }
    }

    pub fn convert_var_name(&self, var_name: &Reference) -> Address {
        let global_resolved_name = var_name.global_resolved.as_ref().unwrap();

        Address {
            name: AddressOrigin::User(global_name_updater(&global_resolved_name)),
            offset: 0,
        }
    }

    pub fn convert_name_path(&self, name_path: &NamePath) -> Address {
        // TODO: does not work for structs yet
        self.convert_var_name(&name_path.name)
    }
}