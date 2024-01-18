use crate::front::semantic::name_resolution::resolver::ResolveResult;
use crate::front::semantic::name_resolution::resolver::ResolverError::Redefinition;
use crate::front::syntax::ast_types::{FnMod, RawName, ResolvedName, VarMod};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum SymbolInfo {
    None,
    Var(Rc<Vec<VarMod>>),
    Fn(Rc<Vec<FnMod>>),
}

#[derive(Debug)]
pub struct Symbol {
    resolved: ResolvedName,
    symbol_info: SymbolInfo,
}

impl Symbol {
    pub fn resolved(&self) -> &ResolvedName {
        &self.resolved as &String
    }

    pub fn symbol_info(&self) -> &SymbolInfo {
        &self.symbol_info
    }
}

#[derive(Debug)]
pub struct ScopeTableNode {
    symbols: HashMap<RawName, Rc<Symbol>>,
}

#[derive(Debug)]
pub struct ScopeTable {
    stack: Vec<ScopeTableNode>,
    scope_level: u32,

    global: HashMap<ResolvedName, Rc<Symbol>>,
    count: HashMap<RawName, i32>,
}

impl ScopeTable {
    pub fn new() -> ScopeTable {
        ScopeTable {
            stack: vec![ScopeTableNode {
                symbols: HashMap::new(),
            }],
            scope_level: 0,
            global: HashMap::new(),
            count: HashMap::new(),
        }
    }

    pub fn get_global(&self) -> &HashMap<ResolvedName, Rc<Symbol>> {
        &self.global
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

    pub fn scope_bind(&mut self, name: &String, symbol_info: SymbolInfo) -> ResolveResult<String> {
        let symbols = &mut self.stack.last_mut().unwrap().symbols;

        let resolved = match self.count.get_mut(name) {
            Some(count) => {
                *count += 1;
                format!("{count}_{name}")
            }
            None => {
                self.count.insert(name.clone(), 0);
                format!("0_{name}")
            }
        };

        match symbols.get_mut(name) {
            Some(_) => {
                return Err(Redefinition(name.clone()));
            }
            None => {
                symbols.insert(
                    name.to_string(),
                    Rc::new(Symbol {
                        resolved: String::from(&resolved),
                        symbol_info,
                    }),
                );
            }
        }

        Ok(resolved)
    }

    pub fn scope_lookup_current(&self, name: &String) -> Option<&Symbol> {
        if let Some(sym) = self.stack.last().unwrap().symbols.get(name) {
            return Some(sym);
        }
        None
    }

    pub fn scope_lookup(&self, name: &String) -> Option<&Symbol> {
        for node in self.stack.iter().rev() {
            if let Some(sym) = node.symbols.get(name) {
                return Some(sym);
            }
        }
        None
    }
}
