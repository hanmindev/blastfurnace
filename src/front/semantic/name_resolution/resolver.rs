use crate::front::semantic::name_resolution::scope_table::{ScopeTable, SymbolInfo};
use crate::front::syntax::ast_types::{
    AtomicExpression, Block, Compound, CompoundValue, Expression, FnCall, FnDef, LiteralValue,
    NamePath, Reference, Statement, StatementBlock, StructAssign, StructDecl, StructDef, VarAssign,
    VarDecl,
};
use std::rc::Rc;

pub trait Resolvable {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub enum ResolverError {
    UndefinedVariable(String),
    Redefinition(String),
}

pub type ResolveResult<T> = Result<T, ResolverError>;

impl Resolvable for Block {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        scope_table.scope_enter();
        for statement in &mut self.statements {
            statement.resolve(scope_table)?;
        }
        scope_table.scope_exit();
        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            s => s.resolve(scope_table)?,
        }
        Ok(())
    }
}

impl Resolvable for Statement {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        return Ok(match self {
            s => s.resolve(scope_table)?,
        });
    }
}

impl Resolvable for VarDecl {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name.resolve(scope_table)?;

        let raw = &self.name.raw.as_ref().unwrap();
        self.name.resolved =
            Some(scope_table.scope_bind(raw, SymbolInfo::Var(Rc::clone(&self.mods)))?);

        match self.expr {
            Some(ref mut expr) => expr.resolve(scope_table)?,
            None => (),
        }

        Ok(())
    }
}

impl Resolvable for StructDecl {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        let raw = &self.name.raw.as_ref().unwrap();
        self.name.resolved =
            Some(scope_table.scope_bind(raw, SymbolInfo::Var(Rc::clone(&self.mods)))?);

        match self.expr {
            Some(ref mut compound) => compound.resolve(scope_table)?,
            None => (),
        }
        Ok(())
    }
}

impl Resolvable for VarAssign {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name_path.resolve(scope_table)?;
        self.expr.resolve(scope_table)?;

        Ok(())
    }
}

impl Resolvable for StructAssign {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name_path.resolve(scope_table)?;
        self.compound.resolve(scope_table)?;

        Ok(())
    }
}

impl Resolvable for StructDef {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name.resolve(scope_table)?;

        Ok(())
    }
}

impl Resolvable for FnDef {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name.resolve(scope_table)?;
        self.body.resolve(scope_table)?;

        Ok(())
    }
}

impl Resolvable for Expression {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            Expression::AtomicExpression(atomic) => {
                atomic.resolve(scope_table)?;
            }
            Expression::Unary(_, expression) => {
                expression.resolve(scope_table)?;
            }
            Expression::Binary(e0, _, e1) => {
                e0.resolve(scope_table)?;
                e1.resolve(scope_table)?;
            }
        }

        Ok(())
    }
}

impl Resolvable for AtomicExpression {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            AtomicExpression::Literal(lit) => match lit {
                LiteralValue::Compound(compound) => {
                    compound.resolve(scope_table)?;
                }
                _ => (),
            },
            AtomicExpression::Variable(var) => {
                var.resolve(scope_table)?;
            }
            AtomicExpression::FnCall(fn_call) => {
                fn_call.resolve(scope_table)?;
            }
        }
        Ok(())
    }
}

impl Resolvable for NamePath {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name.resolve(scope_table)?;
        Ok(())
    }
}

impl Resolvable for Reference<String, String> {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match scope_table.scope_lookup(&self.raw.as_ref().unwrap()) {
            Some(symbol) => Ok(self.resolved = Some(symbol.resolved().clone())),
            None => Err(ResolverError::UndefinedVariable(self.raw.clone().unwrap())),
        }
    }
}

impl Resolvable for Compound {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        for (_, value) in self.iter_mut() {
            match value {
                CompoundValue::Expression(expr) => expr.resolve(scope_table)?,
                CompoundValue::Compound(compound) => compound.resolve(scope_table)?,
            }
        }
        Ok(())
    }
}

impl Resolvable for FnCall {
    fn resolve(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name_path.resolve(scope_table)?;
        for arg in &mut self.args {
            arg.resolve(scope_table)?;
        }
        Ok(())
    }
}
