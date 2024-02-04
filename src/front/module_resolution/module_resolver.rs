use crate::front::module_resolution::name_map::NameMap;
use crate::front::syntax::ast_types::{
    AtomicExpression, Block, Definition, Expression, GlobalResolvedName, If, Module, Reference,
    Statement, StatementBlock,
};
use std::rc::Rc;

pub trait Resolvable {
    fn resolve_module(&mut self, _name_map: &mut NameMap) -> ResolveResult<()> {
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ResolverError {}

pub type ResolveResult<T> = Result<T, ResolverError>;

impl Resolvable for Block {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        while let Some(definition) = self.definitions.pop() {
            resolve_definition(definition, name_map)?;
        }

        for statement in &mut self.statements {
            statement.resolve_module(name_map)?;
        }
        Ok(())
    }
}

impl Resolvable for Module {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        while let Some(definition) = self.public_definitions.pop() {
            resolve_definition(definition, name_map)?;
        }

        self.block.resolve_module(name_map)?;
        Ok(())
    }
}

impl Resolvable for Reference {
    fn resolve_module(&mut self, _name_map: &mut NameMap) -> ResolveResult<()> {
        self.global_resolved = Some(Rc::from(GlobalResolvedName {
            module: _name_map.path.clone(),
            name: self.module_resolved.clone().unwrap(),
        }));

        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        match self {
            StatementBlock::Statement(statement) => statement.resolve_module(name_map)?,
            StatementBlock::Block(block) => block.resolve_module(name_map)?,
        }
        Ok(())
    }
}

impl Resolvable for If {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        self.cond.resolve_module(name_map)?;
        self.body.resolve_module(name_map)?;
        if let Some(else_block) = &mut self.else_ {
            else_block.resolve_module(name_map)?;
        }
        Ok(())
    }
}

impl Resolvable for Statement {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        match self {
            Statement::VarDecl(var_decl) => {
                var_decl.var_def.name.resolve_module(name_map)?;
                if let Some(expr) = &mut var_decl.expr {
                    expr.resolve_module(name_map)?;
                }
            }
            Statement::VarAssign(var_assign) => {
                var_assign.name_path.name.resolve_module(name_map)?;
                var_assign.expr.resolve_module(name_map)?;
            }
            Statement::If(if_) => {
                if_.resolve_module(name_map)?;
            }
            Statement::While(while_) => {
                while_.cond.resolve_module(name_map)?;
                while_.body.resolve_module(name_map)?;
            }
            Statement::For(for_) => {
                if let Some(init) = &mut for_.init {
                    init.resolve_module(name_map)?;
                }
                if let Some(cond) = &mut for_.cond {
                    cond.resolve_module(name_map)?;
                }
                if let Some(step) = &mut for_.step {
                    step.resolve_module(name_map)?;
                }
                for_.body.resolve_module(name_map)?;
            }
            Statement::Expression(expr) => {
                expr.resolve_module(name_map)?;
            }
            _ => {}
        }
        Ok(())
    }
}

impl Resolvable for Expression {
    fn resolve_module(&mut self, name_map: &mut NameMap) -> ResolveResult<()> {
        match self {
            Expression::AtomicExpression(atomic) => match atomic {
                AtomicExpression::Literal(_) => {}
                AtomicExpression::Variable(var) => {
                    var.name.resolve_module(name_map)?;
                }
                AtomicExpression::FnCall(fn_call) => {
                    fn_call.name.resolve_module(name_map)?;
                }
            },
            Expression::Unary(_, expr) => {
                expr.resolve_module(name_map)?;
            }
            Expression::Binary(e0, _, e1) => {
                e0.resolve_module(name_map)?;
                e1.resolve_module(name_map)?;
            }
        }
        Ok(())
    }
}

fn resolve_definition(definition: Definition, name_map: &mut NameMap) -> ResolveResult<()> {
    match definition {
        Definition::FnDef(mut fn_def) => {
            fn_def.name.resolve_module(name_map)?;
            fn_def.body.as_mut().unwrap().resolve_module(name_map)?;

            name_map
                .function_definitions
                .insert(fn_def.name.global_resolved.clone().unwrap(), fn_def);
        }
        Definition::StructDef(mut struct_def) => {
            struct_def.type_name.resolve_module(name_map)?;

            name_map.struct_definitions.insert(
                struct_def.type_name.global_resolved.clone().unwrap(),
                struct_def,
            );
        }
        Definition::VarDecl(mut var_decl) => {
            var_decl.var_def.name.resolve_module(name_map)?;
            if let Some(expr) = &mut var_decl.expr {
                expr.resolve_module(name_map)?;
            }

            name_map.global_var_definitions.insert(
                var_decl.var_def.name.global_resolved.clone().unwrap(),
                var_decl,
            );
        }
        _ => {}
    }
    Ok(())
}
