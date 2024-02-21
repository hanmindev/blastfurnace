use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use crate::front::ast_types::{AtomicExpression, Definition, ResolvedName};
use crate::front::mergers::package::module_resolution::module_merger::ModuleMerger;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    ImportVisibilityError(String, String, String),
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<ResolverError, ()> for ModuleMerger {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<()> {
        match ast_node {
            ASTNodeEnum::Block(block) => {
                while let Some(definition) = block.definitions.pop() {
                    resolve_definition(definition, self, false)?;
                }

                for statement in &mut block.statements {
                    statement.visit(self)?;
                }
            }
            ASTNodeEnum::Module(module) => {
                while let Some(mut use_) = module.uses.pop() {
                    use_.visit(self)?;
                }

                while let Some(definition) = module.public_definitions.pop() {
                    resolve_definition(definition, self, true)?;
                }

                module.block.visit(self)?;
            }
            ASTNodeEnum::Use(use_) => {
                for element in &mut use_.elements {
                    let original_name = element.origin_name.clone();
                    let local_name: Rc<ResolvedName> = element
                        .imported_name
                        .module_resolved
                        .as_ref()
                        .unwrap()
                        .clone();

                    let module_name = if use_.path[0] == "root" {
                        let mut s = "/".to_string();
                        s.push_str(&use_.path[1..].join("/"));
                        s
                    } else {
                        let s = use_.path.join("/");

                        if !self.can_call(&s) {
                            return Err(ResolverError::ImportVisibilityError(
                                s.clone(),
                                self.get_path().clone(),
                                "Cannot call module from this path".to_string(),
                            ));
                        }
                        s
                    };

                    let global_resolved_name =
                        self.create_or_get_global_name(module_name, format!("0_{original_name}"));

                    self.register_global_name(local_name, Rc::from(global_resolved_name), false);
                    element.imported_name.visit(self)?;
                }
            }
            ASTNodeEnum::Reference(reference) => {
                reference.global_resolved =
                    Some(self.resolve_global_name(reference.module_resolved.as_ref().unwrap()));
            }

            ASTNodeEnum::NamePath(name_path) => {
                name_path.name.visit(self)?;
            }

            ASTNodeEnum::Else(_)
            | ASTNodeEnum::If(_)
            | ASTNodeEnum::While(_)
            | ASTNodeEnum::For(_)
            | ASTNodeEnum::Statement(_)
            | ASTNodeEnum::Expression(_)
            | ASTNodeEnum::VarAssign(_)
            | ASTNodeEnum::VarDef(_)
            | ASTNodeEnum::FnCall(_)
            | ASTNodeEnum::LiteralValue(_)
            | ASTNodeEnum::VarDecl(_)
            | ASTNodeEnum::AtomicExpression(_) => return Ok((true, None)),

            ASTNodeEnum::FnDef(_)
            | ASTNodeEnum::StructDef(_)
            | ASTNodeEnum::Definition(_) => panic!("Should not be called directly"),
        };
        return Ok((false, None));
    }
}

fn resolve_definition(
    definition: Definition,
    module_merger: &mut ModuleMerger,
    is_public: bool,
) -> ResolveResult<()> {
    match definition {
        Definition::FnDef(mut fn_def) => {
            fn_def.name.visit(module_merger)?;
            fn_def.body.visit(module_merger)?;

            module_merger.insert_fn_definition(
                fn_def.name.global_resolved.clone().unwrap(),
                fn_def,
                is_public,
            );
        }
        Definition::StructDef(mut struct_def) => {
            struct_def.type_name.visit(module_merger)?;

            module_merger.insert_struct_definition(
                struct_def.type_name.global_resolved.clone().unwrap(),
                struct_def,
                is_public,
            );
        }
        Definition::VarDecl(mut var_decl) => {
            var_decl.var_def.name.visit(module_merger)?;
            if let Some(expr) = &mut var_decl.expr {
                expr.visit(module_merger)?;
            }

            module_merger.insert_global_var_definition(
                var_decl.var_def.name.global_resolved.clone().unwrap(),
                var_decl,
                is_public,
            );
        }
    }
    Ok((true, None))
}
