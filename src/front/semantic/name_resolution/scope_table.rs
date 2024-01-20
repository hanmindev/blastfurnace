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
    unresolved: HashMap<(RawName, SymbolType), Rc<ResolvedName>>,
}

#[derive(Debug)]
pub struct ScopeTable {
    stack: Vec<ScopeTableNode>,
    scope_level: i32,

    global_count: HashMap<(RawName, SymbolType), i32>,
}

pub fn name_format(name: &String, count: i32) -> String {
    format!("{count}_{name}")
}

impl ScopeTable {
    pub fn new() -> ScopeTable {
        ScopeTable {
            stack: vec![ScopeTableNode {
                symbols: HashMap::new(),
                unresolved: HashMap::new(),
            }],
            scope_level: 0,
            global_count: HashMap::new(),
        }
    }

    pub fn scope_enter(&mut self) {
        self.stack.push(ScopeTableNode {
            symbols: HashMap::new(),
            unresolved: HashMap::new(),
        });
        self.scope_level += 1;
    }

    pub fn scope_exit(&mut self) {
        self.stack.pop();
        self.scope_level -= 1;
    }

    pub fn scope_level(&self) -> i32 {
        self.scope_level
    }

    pub fn scope_bind(
        &mut self,
        name: &String,
        symbol_type: SymbolType,
    ) -> ResolveResult<Rc<ResolvedName>> {
        let key = (name.clone(), symbol_type);

        let node = &mut self.stack.last_mut().unwrap();

        // first see if it is unresolved in the current scope. If so, remove it from unresolved and resolve it
        if let Some(resolved) = node.unresolved.remove(&key) {
            // resolve it
            node.symbols.insert(key, resolved.clone());
            Ok(resolved)
        } else {
            let resolved = Rc::new(match self.global_count.get_mut(&key) {
                Some(count) => {
                    *count += 1;
                    name_format(name, *count)
                }
                None => {
                    self.global_count.insert((name.clone(), symbol_type), 0);
                    name_format(name, 0)
                }
            });

            match node.symbols.get_mut(&key) {
                Some(_) => {
                    return Err(Redefinition(name.clone()));
                }
                None => {
                    node.symbols.insert(key, resolved.clone());
                }
            }
            Ok(resolved)
        }
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

    /*
    Warning: This works with the assumption that all scope-based names are defined at the start of the scope.
    Some preprocessing may have to be done to the AST to ensure this is true.
     */
    pub fn scope_lookup_force(
        &mut self,
        name: &String,
        symbol_type: SymbolType,
    ) -> Rc<ResolvedName> {
        if let Some(rn) = self.scope_lookup(name, symbol_type) {
            return rn;
        }

        let key = (name.clone(), symbol_type);
        let node = &mut self.stack.last_mut().unwrap();
        if let Some(resolved) = node.unresolved.get(&key) {
            return resolved.clone();
        }

        // symbol is not resolved yet, bind it to the current scope so future lookups will be equal

        let resolved = Rc::new(match self.global_count.get_mut(&key) {
            Some(count) => {
                *count += 1;
                name_format(name, *count)
            }
            None => {
                self.global_count.insert(key.clone(), 0);
                name_format(name, 0)
            }
        });

        self.stack
            .last_mut()
            .unwrap()
            .unresolved
            .insert(key.clone(), resolved.clone());

        resolved
    }
}
