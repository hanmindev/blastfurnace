use crate::front::syntax::ast_types::{FnMod, VarMod};
use std::collections::HashMap;
use std::rc::Rc;

type Raw = String;
type Resolved = String;

pub enum SymbolInfo {
    None,
    Var(Rc<Vec<VarMod>>),
    Fn(Rc<Vec<FnMod>>),
}

pub struct Symbol {
    resolved: Resolved,
    symbol_info: SymbolInfo,
}

impl Symbol {
    pub fn resolved(&self) -> &Resolved {
        &self.resolved as &String
    }

    pub fn symbol_info(&self) -> &SymbolInfo {
        &self.symbol_info
    }
}

pub struct ScopeTableNode {
    symbols: HashMap<Raw, Vec<Rc<Symbol>>>,
}

pub struct ScopeTable {
    stack: Vec<ScopeTableNode>,
    scope_level: u32,

    global: HashMap<Resolved, Rc<Symbol>>,
    count: HashMap<Raw, i32>,
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

    pub fn scope_bind(&mut self, name: &String, symbol_info: SymbolInfo) -> String {
        let mut symbols = &mut self.stack.last_mut().unwrap().symbols;

        let prefix_value = match self.count.get_mut(name) {
            Some(count) => {
                *count += 1;
                count.to_string()
            }
            None => {
                self.count.insert(name.clone(), 0);
                "".to_string()
            }
        };

        let resolved = format!("{prefix_value}_{name}");

        match symbols.get_mut(name) {
            Some(vec) => {
                vec.push(Rc::new(Symbol {
                    resolved: String::from(&resolved),
                    symbol_info,
                }));
            }
            None => {
                symbols.insert(
                    name.to_string(),
                    vec![Rc::new(Symbol {
                        resolved: String::from(&resolved),
                        symbol_info,
                    })],
                );
            }
        }

        return resolved;
    }

    pub fn scope_lookup_current(&self, name: &String) -> Option<&Symbol> {
        if let Some(vec) = self.stack.last().unwrap().symbols.get(name) {
            return Some(vec.last().unwrap());
        }
        None
    }

    pub fn scope_lookup(&self, name: &String) -> Option<&Symbol> {
        for node in self.stack.iter().rev() {
            if let Some(vec) = node.symbols.get(name) {
                return Some(vec.last().unwrap());
            }
        }
        None
    }
}
