use crate::front::ast_types::{
    AtomicExpression, Block, Definition, Expression, If, Reference, ResolvedName, Statement,
    StatementBlock,
};
use crate::front::internal_ast_types::{Module, Use};
use crate::front::mergers::package::module_resolution::module_merger::ModuleMerger;
use std::rc::Rc;

pub trait Resolvable {
    fn resolve_module(&mut self, _module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    ImportVisibilityError(String, String, String),
}

pub type ResolveResult<T> = Result<T, ResolverError>;

impl Resolvable for Block {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        while let Some(definition) = self.definitions.pop() {
            resolve_definition(definition, module_merger, false)?;
        }

        for statement in &mut self.statements {
            statement.resolve_module(module_merger)?;
        }
        Ok(())
    }
}

impl Resolvable for Module {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        while let Some(mut use_) = self.uses.pop() {
            use_.resolve_module(module_merger)?;
        }

        while let Some(definition) = self.public_definitions.pop() {
            resolve_definition(definition, module_merger, true)?;
        }

        self.block.resolve_module(module_merger)?;
        Ok(())
    }
}

impl Resolvable for Use {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        for element in &mut self.elements {
            let original_name = element.origin_name.clone();
            let local_name: Rc<ResolvedName> = element
                .imported_name
                .module_resolved
                .as_ref()
                .unwrap()
                .clone();

            let module_name = if self.path[0] == "root" {
                let mut s = "/".to_string();
                s.push_str(&self.path[1..].join("/"));
                s
            } else {
                let s = self.path.join("/");

                if !module_merger.can_call(&s) {
                    return Err(ResolverError::ImportVisibilityError(
                        s.clone(),
                        module_merger.get_path().clone(),
                        "Cannot call module from this path".to_string(),
                    ));
                }
                s
            };

            let global_resolved_name =
                module_merger.create_or_get_global_name(module_name, format!("0_{original_name}"));

            module_merger.register_global_name(local_name, Rc::from(global_resolved_name), false);
            element.imported_name.resolve_module(module_merger)?;
        }

        Ok(())
    }
}

impl Resolvable for Reference {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        self.global_resolved =
            Some(module_merger.resolve_global_name(self.module_resolved.as_ref().unwrap()));

        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        match self {
            StatementBlock::Statement(statement) => statement.resolve_module(module_merger)?,
            StatementBlock::Block(block) => block.resolve_module(module_merger)?,
        }
        Ok(())
    }
}

impl Resolvable for If {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        self.cond.resolve_module(module_merger)?;
        self.body.resolve_module(module_merger)?;
        if let Some(else_block) = &mut self.else_ {
            else_block.resolve_module(module_merger)?;
        }
        Ok(())
    }
}

impl Resolvable for Statement {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        match self {
            Statement::VarDecl(var_decl) => {
                var_decl.var_def.name.resolve_module(module_merger)?;
                if let Some(expr) = &mut var_decl.expr {
                    expr.resolve_module(module_merger)?;
                }
            }
            Statement::VarAssign(var_assign) => {
                var_assign.name_path.name.resolve_module(module_merger)?;
                var_assign.expr.resolve_module(module_merger)?;
            }
            Statement::If(if_) => {
                if_.resolve_module(module_merger)?;
            }
            Statement::While(while_) => {
                while_.cond.resolve_module(module_merger)?;
                while_.body.resolve_module(module_merger)?;
            }
            Statement::For(for_) => {
                if let Some(init) = &mut for_.init {
                    init.resolve_module(module_merger)?;
                }
                if let Some(cond) = &mut for_.cond {
                    cond.resolve_module(module_merger)?;
                }
                if let Some(step) = &mut for_.step {
                    step.resolve_module(module_merger)?;
                }
                for_.body.resolve_module(module_merger)?;
            }
            Statement::Expression(expr) => {
                expr.resolve_module(module_merger)?;
            }
            _ => {}
        }
        Ok(())
    }
}

impl Resolvable for Expression {
    fn resolve_module(&mut self, module_merger: &mut ModuleMerger) -> ResolveResult<()> {
        match self {
            Expression::AtomicExpression(atomic) => match atomic {
                AtomicExpression::Literal(_) => {}
                AtomicExpression::Variable(var) => {
                    var.name.resolve_module(module_merger)?;
                }
                AtomicExpression::FnCall(fn_call) => {
                    fn_call.name.resolve_module(module_merger)?;
                }
            },
            Expression::Unary(_, expr) => {
                expr.resolve_module(module_merger)?;
            }
            Expression::Binary(e0, _, e1) => {
                e0.resolve_module(module_merger)?;
                e1.resolve_module(module_merger)?;
            }
        }
        Ok(())
    }
}

fn resolve_definition(
    definition: Definition,
    module_merger: &mut ModuleMerger,
    is_public: bool,
) -> ResolveResult<()> {
    match definition {
        Definition::FnDef(mut fn_def) => {
            fn_def.name.resolve_module(module_merger)?;
            fn_def
                .body
                .as_mut()
                .unwrap()
                .resolve_module(module_merger)?;

            module_merger.insert_fn_definition(
                fn_def.name.global_resolved.clone().unwrap(),
                fn_def,
                is_public,
            );
        }
        Definition::StructDef(mut struct_def) => {
            struct_def.type_name.resolve_module(module_merger)?;

            module_merger.insert_struct_definition(
                struct_def.type_name.global_resolved.clone().unwrap(),
                struct_def,
                is_public,
            );
        }
        Definition::VarDecl(mut var_decl) => {
            var_decl.var_def.name.resolve_module(module_merger)?;
            if let Some(expr) = &mut var_decl.expr {
                expr.resolve_module(module_merger)?;
            }

            module_merger.insert_global_var_definition(
                var_decl.var_def.name.global_resolved.clone().unwrap(),
                var_decl,
                is_public,
            );
        }
    }
    Ok(())
}
