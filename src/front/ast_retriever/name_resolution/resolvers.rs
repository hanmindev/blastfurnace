use crate::front::ast_retriever::name_resolution::scope_table::{ScopeTable, SymbolType};
use crate::front::ast_types::{AtomicExpression, Block, Compound, CompoundValue, Definition, Expression, ExpressionEnum, FnCall, FnDef, For, If, LiteralValue, Module, NamePath, Statement, StatementBlock, StructDef, Type, Use, VarAssign, VarDecl, VarDef, While};

pub trait Resolvable {
    fn resolve_name(&mut self, _scope_table: &mut ScopeTable) -> ResolveResult<()> {
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    UndefinedVariable(String),
    Redefinition(String),
}

pub type ResolveResult<T> = Result<T, ResolverError>;

impl Resolvable for Block {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        scope_table.scope_enter();
        for definitions in &mut self.definitions {
            definitions.resolve_name(scope_table)?;
        }

        for statement in &mut self.statements {
            statement.resolve_name(scope_table)?;
        }
        scope_table.scope_exit();
        Ok(())
    }
}

impl Resolvable for Module {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        scope_table.scope_enter();
        for use_ in &mut self.uses {
            use_.resolve_name(scope_table)?;
        }

        for definitions in &mut self.public_definitions {
            definitions.resolve_name(scope_table)?;
        }

        self.block.resolve_name(scope_table)?;
        scope_table.scope_exit();
        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            StatementBlock::Statement(statement) => statement.resolve_name(scope_table)?,
            StatementBlock::Block(block) => block.resolve_name(scope_table)?,
        }
        Ok(())
    }
}

impl Resolvable for Definition {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            Definition::VarDecl(statement) => statement.resolve_name(scope_table)?,
            Definition::StructDef(statement) => statement.resolve_name(scope_table)?,
            Definition::FnDef(statement) => statement.resolve_name(scope_table)?,
        }
        Ok(())
    }
}

impl Resolvable for Statement {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            Statement::VarDecl(statement) => statement.resolve_name(scope_table)?,
            Statement::VarAssign(statement) => statement.resolve_name(scope_table)?,
            Statement::If(statement) => statement.resolve_name(scope_table)?,
            Statement::While(statement) => statement.resolve_name(scope_table)?,
            Statement::For(statement) => statement.resolve_name(scope_table)?,
            Statement::Return(statement) => statement.resolve_name(scope_table)?,
            Statement::Expression(statement) => statement.resolve_name(scope_table)?,

            _ => {}
        };
        Ok(())
    }
}

impl Resolvable for VarDef {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        if let Some(Type::Struct(struct_name)) = &mut self.type_ {
            struct_name.module_resolved =
                Some(scope_table.scope_lookup_force(&struct_name.raw, SymbolType::Struct));
        }

        self.name.module_resolved = Some(scope_table.scope_bind(&self.name.raw, SymbolType::Var)?);
        Ok(())
    }
}

impl Resolvable for VarDecl {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        if let Some(ref mut expr) = self.expr {
            expr.resolve_name(scope_table)?
        }
        self.var_def.resolve_name(scope_table)?;

        Ok(())
    }
}

impl Resolvable for VarAssign {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.name_path.resolve_name(scope_table)?;
        self.expr.resolve_name(scope_table)?;

        Ok(())
    }
}

impl Resolvable for StructDef {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.type_name.module_resolved =
            Some(scope_table.scope_bind(&self.type_name.raw, SymbolType::Struct)?);

        for v in &mut self.map.values_mut() {
            if let Type::Struct(struct_name) = v {
                struct_name.module_resolved =
                    Some(scope_table.scope_lookup_force(&struct_name.raw, SymbolType::Struct));
            }
        }
        Ok(())
    }
}

impl Resolvable for FnDef {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        scope_table.scope_enter();

        self.name.module_resolved = Some(scope_table.scope_bind(&self.name.raw, SymbolType::Fn)?);
        for arg in &mut self.args {
            arg.resolve_name(scope_table)?;
        }

        if let Some(body) = &mut self.body {
            body.resolve_name(scope_table)?;
        }
        scope_table.scope_exit();

        Ok(())
    }
}

impl Resolvable for Expression {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match &mut self.expr {
            ExpressionEnum::AtomicExpression(atomic) => {
                atomic.resolve_name(scope_table)?;
            }
            ExpressionEnum::Unary(_, expression) => {
                expression.resolve_name(scope_table)?;
            }
            ExpressionEnum::Binary(e0, _, e1) => {
                e0.resolve_name(scope_table)?;
                e1.resolve_name(scope_table)?;
            }
        }

        Ok(())
    }
}

impl Resolvable for AtomicExpression {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match self {
            AtomicExpression::Literal(lit) => {
                if let LiteralValue::Compound(compound) = lit {
                    compound.resolve_name(scope_table)?;
                }
            }
            AtomicExpression::Variable(var) => {
                var.resolve_name(scope_table)?;
            }
            AtomicExpression::FnCall(fn_call) => {
                fn_call.resolve_name(scope_table)?;
            }
        }
        Ok(())
    }
}

impl Resolvable for NamePath {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match scope_table.scope_lookup(&self.name.raw, SymbolType::Var) {
            Some(name) => {
                self.name.module_resolved = Some(name.clone());
                Ok(())
            }
            None => Err(ResolverError::UndefinedVariable(self.name.raw.clone())),
        }
    }
}

impl Resolvable for Compound {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        for (_, value) in self.iter_mut() {
            match value {
                CompoundValue::Expression(expr) => expr.resolve_name(scope_table)?,
                CompoundValue::Compound(compound) => compound.resolve_name(scope_table)?,
            }
        }
        Ok(())
    }
}

impl Resolvable for FnCall {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        match scope_table.scope_lookup(&self.name.raw, SymbolType::Fn) {
            Some(name) => {
                self.name.module_resolved = Some(name.clone());
            }
            None => return Err(ResolverError::UndefinedVariable(self.name.raw.clone())),
        }

        for arg in &mut self.args {
            arg.resolve_name(scope_table)?;
        }
        Ok(())
    }
}

impl Resolvable for If {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.cond.resolve_name(scope_table)?;
        self.body.resolve_name(scope_table)?;
        if let Some(ref mut else_body) = self.else_ {
            else_body.resolve_name(scope_table)?;
        }
        Ok(())
    }
}

impl Resolvable for While {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        self.cond.resolve_name(scope_table)?;
        self.body.resolve_name(scope_table)?;
        Ok(())
    }
}

impl Resolvable for For {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        if let Some(ref mut init) = self.init {
            init.resolve_name(scope_table)?;
        }
        if let Some(ref mut cond) = self.cond {
            cond.resolve_name(scope_table)?;
        }
        if let Some(ref mut step) = self.step {
            step.resolve_name(scope_table)?;
        }
        self.body.resolve_name(scope_table)?;
        Ok(())
    }
}

impl Resolvable for Use {
    fn resolve_name(&mut self, scope_table: &mut ScopeTable) -> ResolveResult<()> {
        for element in &mut self.elements {
            let struct_name =
                scope_table.scope_bind(&element.imported_name.raw, SymbolType::Struct)?;
            let fn_name = scope_table.scope_bind(&element.imported_name.raw, SymbolType::Fn)?;
            let var_name = scope_table.scope_bind(&element.imported_name.raw, SymbolType::Var)?;

            if struct_name == fn_name && fn_name == var_name {
                element.imported_name.module_resolved = Some(struct_name);
            } else {
                return Err(ResolverError::Redefinition(
                    element.imported_name.raw.clone(),
                ));
            }
        }

        Ok(())
    }
}
