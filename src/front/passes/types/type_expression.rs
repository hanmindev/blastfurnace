use crate::front::ast_types::{
    AtomicExpression, BinOp, Expression, ExpressionEnum, GlobalResolvedName, LiteralValue, Type,
    UnOp,
};
use crate::front::exporter::export::FrontProgram;
use either::Left;
use std::collections::HashMap;
use std::rc::Rc;
use crate::front::passes::types::var_def_table::VarTypeNode;

struct BinOpNode {
    pub op: BinOp,
    pub left: Box<TypeTree>,
    pub right: Box<TypeTree>,
}

struct UnOpNode {
    pub op: UnOp,
    pub operand: Box<TypeTree>,
}

pub struct TypeDependency {
    pub deps: Vec<Rc<GlobalResolvedName>>,
    pub tree: TypeTree,
}

impl TypeDependency {
    pub fn new(expr: &Expression) -> TypeDependency {
        let mut deps = Vec::new();

        let tree = match &expr.expr {
            ExpressionEnum::AtomicExpression(x) => match x {
                AtomicExpression::Literal(x) => TypeTree::Type(literal_types(x)),
                AtomicExpression::Variable(x) => {
                    let var_name = x.name.global_resolved.as_ref().unwrap().clone();
                    deps.push(var_name.clone());
                    TypeTree::Var(var_name)
                }
                AtomicExpression::FnCall(x) => {
                    TypeTree::FnCall(x.name.global_resolved.as_ref().unwrap().clone())
                }
            },
            ExpressionEnum::Unary(unop, x) => {
                let child = TypeDependency::new(x);
                deps.extend(child.deps);
                TypeTree::UnOp(UnOpNode {
                    op: unop.clone(),
                    operand: Box::new(child.tree),
                })
            }
            ExpressionEnum::Binary(e0, binop, e1) => {
                let child0 = TypeDependency::new(e0);
                let child1 = TypeDependency::new(e1);
                deps.extend(child0.deps);
                deps.extend(child1.deps);
                TypeTree::BinOp(BinOpNode {
                    op: binop.clone(),
                    left: Box::new(child0.tree),
                    right: Box::new(child1.tree),
                })
            }
        };

        TypeDependency { deps, tree }
    }
}

pub enum TypeTree {
    BinOp(BinOpNode),
    UnOp(UnOpNode),
    Type(Type),
    Var(Rc<GlobalResolvedName>),
    FnCall(Rc<GlobalResolvedName>),
}

impl TypeTree {
    pub fn resolve_type(
        &self,
        front_program: &FrontProgram,
        vars: &HashMap<Rc<GlobalResolvedName>, VarTypeNode>,
    ) -> TypeResult<Type> {
        match self {
            TypeTree::BinOp(node) => {
                let left = node.left.resolve_type(front_program, vars)?;
                let right = node.right.resolve_type(front_program, vars)?;
                binop_type_resolver(&node.op, &left, &right)
            }
            TypeTree::UnOp(node) => {
                let operand = node.operand.resolve_type(front_program, vars)?;
                unop_type_resolver(&node.op, &operand)
            }
            TypeTree::Type(type_) => Ok(type_.clone()),
            TypeTree::Var(name) => {
                if let Some(var) = vars.get(name) {
                    if let Left(type_) = &var.types_ {
                        Ok(type_.clone())
                    } else {
                        panic!("Dependency Variable Type not resolved!")
                    }
                } else {
                    panic!("Dependency Variable Type not in table!")
                }
            }
            TypeTree::FnCall(name) => {
                if let Some(fn_def) = front_program.definitions.function_definitions.get(name) {
                    Ok(fn_def.return_type.clone())
                } else {
                    panic!("Dependency Function not in table!")
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch,
    MultipleTypes,
}

pub type TypeResult<T> = Result<T, TypeError>;

pub fn literal_types(literal: &LiteralValue) -> Type {
    match literal {
        LiteralValue::Null => Type::Void,
        LiteralValue::Int(_) => Type::Int,
        LiteralValue::Decimal(_) => Type::Double,
        LiteralValue::String(_) => Type::String,
        LiteralValue::Compound(_) => Type::Void,
    }
}

pub fn binop_type_resolver(op: &BinOp, left: &Type, right: &Type) -> TypeResult<Type> {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
            if left == &Type::Int && right == &Type::Int {
                Ok(Type::Int)
            } else if left == &Type::Float && right == &Type::Float {
                Ok(Type::Float)
            } else if left == &Type::Double && right == &Type::Double {
                Ok(Type::Double)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
        BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
            if left == right {
                Ok(Type::Int)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
        BinOp::And | BinOp::Or => {
            if left == &Type::Int && right == &Type::Int {
                Ok(Type::Int)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
    }
}

pub fn unop_type_resolver(op: &UnOp, operand: &Type) -> TypeResult<Type> {
    match op {
        UnOp::Neg => {
            if operand == &Type::Int {
                Ok(Type::Int)
            } else if operand == &Type::Float {
                Ok(Type::Float)
            } else if operand == &Type::Double {
                Ok(Type::Double)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
        UnOp::Not => {
            if operand == &Type::Int {
                Ok(Type::Int)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
        UnOp::PreInc | UnOp::PreDec | UnOp::PostInc | UnOp::PostDec => {
            if operand == &Type::Int {
                Ok(Type::Int)
            } else if operand == &Type::Float {
                Ok(Type::Float)
            } else if operand == &Type::Double {
                Ok(Type::Double)
            } else {
                Err(TypeError::TypeMismatch)
            }
        }
        _ => Err(TypeError::TypeMismatch),
    }
}
