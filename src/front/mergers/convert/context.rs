use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use crate::front::ast_types::{GlobalResolvedName, NamePath, Reference};
use crate::front::mergers::convert::global_name_updater;
use crate::front::mergers::definition_table::DefinitionTable;
use crate::middle::format::ir_types::{Address, AddressOrigin};
use std::rc::Rc;

struct VarGenerator {
    spare_vars: BinaryHeap<Reverse<u32>>,
    next_var: u32,
}

impl VarGenerator {
    fn new() -> VarGenerator {
        VarGenerator {
            spare_vars: BinaryHeap::new(),
            next_var: 0,
        }
    }

    fn get_var(&mut self) -> u32 {
        if let Some(var) = self.spare_vars.pop() {
            var.0
        } else {
            let var = self.next_var;
            self.next_var += 1;
            var
        }
    }

    fn forfeit_var(&mut self, var: u32) {
        self.spare_vars.push(Reverse(var));

        if self.spare_vars.len() == self.next_var as usize {
            self.spare_vars.clear();
            self.next_var = 0;
        }
    }
}

pub struct ConstGenerator {
    consts: HashSet<i32>,
}

impl ConstGenerator {
    pub(crate) fn new() -> ConstGenerator {
        ConstGenerator {
            consts: HashSet::new(),
        }
    }

    pub fn get_const(&mut self, value: i32) -> Address {
        self.consts.insert(value);
        Address {
            name: AddressOrigin::Const(value),
            offset: 0,
        }
    }
}


pub struct Context<'a> {
    pub fn_name: String,
    block_count: u32,
    var_generator: VarGenerator,
    definition_table: &'a DefinitionTable<Rc<GlobalResolvedName>>,
    pub const_generator: &'a mut ConstGenerator,
}

impl Context<'_> {
    pub fn new<'a>(
        fn_name: &str,
        definition_table: &'a DefinitionTable<Rc<GlobalResolvedName>>,
        const_generator: &'a mut ConstGenerator
    ) -> Context<'a> {
        Context {
            fn_name: fn_name.to_string(),
            block_count: 0,
            var_generator: VarGenerator::new(),
            definition_table,
            const_generator,
        }
    }

    pub fn use_block(&mut self) -> u32 {
        let block = self.block_count;
        self.block_count += 1;
        block
    }

    pub fn get_parameter_variable(
        &self,
        function_name: &Rc<GlobalResolvedName>,
        index: u32,
    ) -> Address {
        let fn_ = self
            .definition_table
            .function_definitions
            .get(function_name)
            .unwrap();
        self.convert_var_name(&fn_.args[index as usize].name)
    }

    pub fn get_variable(&mut self) -> Address {
        Address {
            name: AddressOrigin::CtxGenerated(self.fn_name.clone(), self.var_generator.get_var()),
            offset: 0,
        }
    }

    pub fn forfeit_variable(&mut self, address: &Address) {
        if let AddressOrigin::CtxGenerated(fn_name, var) = &address.name {
            if *fn_name != self.fn_name {
                panic!("Cannot forfeit variable from another function"); // TODO: make this not panic
            }

            self.var_generator.forfeit_var(*var);
        }
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
