use std::collections::HashMap;
use std::rc::Rc;
use either::Either;
use crate::front::ast_types::{Expression, GlobalResolvedName, Type};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::TypeDependency;

pub struct VarTypeNode {
    pub types_: Either<Type, TypeDependency>,
}

pub struct VarDefTable {
    pub var_types: HashMap<Rc<GlobalResolvedName>, VarTypeNode>,
}

impl VarDefTable {
    pub fn new(program: &FrontProgram) -> VarDefTable {
        VarDefTable {
            var_types: program
                .definitions
                .global_var_definitions
                .iter()
                .map(|(k, v)| {
                    (Rc::clone(k), {
                        if let Some(type_) = &v.var_def.type_ {
                            VarTypeNode {
                                types_: Either::Left(type_.clone()),
                            }
                        } else {
                            VarTypeNode {
                                types_: Either::Right(TypeDependency::new(
                                    &v.expr.as_ref().unwrap(),
                                )),
                            }
                        }
                    })
                })
                .collect(),
        }
    }

    pub fn register_variable_type(&mut self, name: &Rc<GlobalResolvedName>, type_: &Option<Type>) {
        if let Some(type_) = type_ {
            self.var_types.insert(
                Rc::clone(name),
                VarTypeNode {
                    types_: Either::Left(type_.clone()),
                },
            );
        }
    }

    pub fn register_variable_expr(&mut self, name: &Rc<GlobalResolvedName>, expr: &Expression) {
        if let Some(_) = &expr.type_ {
            self.register_variable_type(name, &expr.type_);
        } else {
            if self.var_types.contains_key(name) {
                return;
            }
            self.var_types.insert(
                Rc::clone(name),
                VarTypeNode {
                    types_: Either::Right(TypeDependency::new(expr)),
                },
            );
        }
    }
}