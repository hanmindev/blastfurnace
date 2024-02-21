use crate::front::ast_types::{Expression, GlobalResolvedName, Type};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::TypeDependency;
use either::Either;
use std::collections::HashMap;
use std::rc::Rc;

pub struct VarTypeNode {
    pub types_: Either<Type, TypeDependency>,
}

pub struct VarDefTable<'a> {
    pub program: Option<&'a mut FrontProgram>,
    pub var_types: HashMap<Rc<GlobalResolvedName>, VarTypeNode>,
}

impl VarDefTable<'_> {
    pub fn new(var_types: HashMap<Rc<GlobalResolvedName>, VarTypeNode>) -> VarDefTable<'static> {
        VarDefTable {
            program: None,
            var_types,
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
