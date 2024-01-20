use crate::front::semantic::name_resolution::resolver::ResolveResult;
use crate::front::semantic::name_resolution::resolver::ResolverError::Redefinition;
use crate::front::syntax::ast_types::{RawName, ResolvedName};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum SymbolType {
    Var,
    Fn,
    Struct,
}

#[derive(Debug)]
pub struct ScopeTableNode {
    symbols: HashMap<(RawName, SymbolType), Rc<ResolvedName>>,
}

#[derive(Debug)]
pub struct ScopeTable {
    stack: Vec<ScopeTableNode>,
    scope_level: u32,

    unresolved: HashMap<(RawName, SymbolType), Rc<ResolvedName>>,
    count: HashMap<(RawName, SymbolType), i32>,
}

pub fn name_format(name: &String, count: i32) -> String {
    format!("{count}_{name}")
}

impl ScopeTable {
    pub fn new() -> ScopeTable {
        ScopeTable {
            stack: vec![ScopeTableNode {
                symbols: HashMap::new(),
            }],
            scope_level: 0,
            unresolved: HashMap::new(),
            count: HashMap::new(),
        }
    }

    pub fn scope_enter(&mut self) {
        self.stack.push(ScopeTableNode {
            symbols: HashMap::new(),
        });
        self.scope_level += 1;
    }

    pub fn scope_exit(&mut self) {
        self.stack.pop();
        self.scope_level -= 1;
    }

    pub fn scope_level(&self) -> u32 {
        self.scope_level
    }

    pub fn scope_bind(
        &mut self,
        name: &String,
        symbol_type: SymbolType,
    ) -> ResolveResult<Rc<ResolvedName>> {
        let symbols = &mut self.stack.last_mut().unwrap().symbols;
        let key = (name.clone(), symbol_type);

        let resolved = Rc::new(match self.count.get_mut(&key) {
            Some(count) => {
                *count += 1;
                name_format(name, *count)
            }
            None => {
                if self.scope_level == 0 {
                    if let Some(rn) = self.unresolved.get(&key) {
                        return Ok(rn.clone());
                    }
                }

                self.count.insert((name.clone(), symbol_type), 0);
                name_format(name, 0)
            }
        });

        match symbols.get_mut(&key) {
            Some(_) => {
                return Err(Redefinition(name.clone()));
            }
            None => {
                symbols.insert(key, resolved.clone());
            }
        }

        Ok(resolved)
    }

    pub fn scope_lookup_current(
        &self,
        name: &String,
        symbol_type: SymbolType,
    ) -> Option<Rc<ResolvedName>> {
        if let Some(sym) = self
            .stack
            .last()
            .unwrap()
            .symbols
            .get(&(name.to_string(), symbol_type))
        {
            return Some(sym.clone());
        }
        None
    }

    pub fn scope_lookup(&self, name: &String, symbol_type: SymbolType) -> Option<Rc<ResolvedName>> {
        for node in self.stack.iter().rev() {
            if let Some(sym) = node.symbols.get(&(name.to_string(), symbol_type)) {
                return Some(sym.clone());
            }
        }
        None
    }

    pub fn scope_lookup_force(
        &mut self,
        name: &String,
        symbol_type: SymbolType,
    ) -> Rc<ResolvedName> {
        for node in self.stack.iter().rev() {
            if let Some(sym) = node.symbols.get(&(name.to_string(), symbol_type)) {
                return sym.clone();
            }
        }
        let key = (name.clone(), symbol_type);

        let resolved = Rc::new(name_format(name, -1));

        self.stack
            .last_mut()
            .unwrap()
            .symbols
            .insert(key, resolved.clone());

        resolved
    }
}
