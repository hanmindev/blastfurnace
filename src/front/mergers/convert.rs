use crate::front::ast_types::{
    AtomicExpression, BinOp, Block, Compound, CompoundValue, Expression, FnCall, FnDef, FnMod, For,
    GlobalResolvedName, If, LiteralValue, NamePath, Reference, Statement, StatementBlock,
    StructDef, Type, UnOp, VarAssign, VarDecl, VarDef, VarMod, While,
};
use crate::middle::format::ir_types::{IrAtomicExpression, IrBinOp, IrLiteralValue, IrNamePath};
use crate::middle::format::ir_types::{
    IrBlock, IrCompound, IrCompoundValue, IrExpression, IrFnCall, IrFnDef, IrFnMod, IrFor, IrIf,
    IrStatement, IrStatementBlock, IrStructDef, IrType, IrUnOp, IrVarAssign, IrVarDecl, IrVarDef,
    IrVarMod, IrWhile,
};
use crate::middle::format::types::GlobalName;
use std::rc::Rc;

pub fn global_name_updater(
    package_name: &str,
    global_resolved_name: &Rc<GlobalResolvedName>,
) -> GlobalName {
    format!(
        "{}{}/{}",
        package_name, global_resolved_name.module, global_resolved_name.name
    )
}

fn convert_compound_value(package_name: &str, ast_node: &CompoundValue) -> IrCompoundValue {
    return match ast_node {
        CompoundValue::Expression(expr) => {
            IrCompoundValue::Expression(Box::from(convert_expr(package_name, expr)))
        }
        CompoundValue::Compound(compound) => {
            IrCompoundValue::Compound(Box::from(convert_compound(package_name, compound)))
        }
    };
}

fn convert_compound(package_name: &str, ast_node: &Compound) -> IrCompound {
    return ast_node
        .iter()
        .map(|(k, v)| (k.clone(), convert_compound_value(package_name, v)))
        .collect();
}

fn convert_fn_call(package_name: &str, ast_node: &FnCall) -> IrFnCall {
    return IrFnCall {
        name: convert_reference(package_name, &ast_node.name),
        args: ast_node
            .args
            .iter()
            .map(|x| convert_expr(package_name, x))
            .collect::<Vec<IrExpression>>(),
    };
}

fn convert_unary_op(ast_node: &UnOp) -> IrUnOp {
    return match ast_node {
        UnOp::Neg => IrUnOp::Neg,
        UnOp::Not => IrUnOp::Not,
        UnOp::Deref => IrUnOp::Deref,
        UnOp::Ref => IrUnOp::Ref,
        UnOp::PreInc => IrUnOp::PreInc,
        UnOp::PreDec => IrUnOp::PreDec,
        UnOp::PostInc => IrUnOp::PostInc,
        UnOp::PostDec => IrUnOp::PostDec,
    };
}

fn convert_binary_op(ast_node: &BinOp) -> IrBinOp {
    return match ast_node {
        BinOp::Add => IrBinOp::Add,
        BinOp::Sub => IrBinOp::Sub,
        BinOp::Mul => IrBinOp::Mul,
        BinOp::Div => IrBinOp::Div,
        BinOp::Mod => IrBinOp::Mod,
        BinOp::Eq => IrBinOp::Eq,
        BinOp::Neq => IrBinOp::Neq,
        BinOp::Lt => IrBinOp::Lt,
        BinOp::Gt => IrBinOp::Gt,
        BinOp::Leq => IrBinOp::Leq,
        BinOp::Geq => IrBinOp::Geq,
        BinOp::And => IrBinOp::And,
        BinOp::Or => IrBinOp::Or,
    };
}

fn convert_expr(package_name: &str, ast_node: &Expression) -> IrExpression {
    return match ast_node {
        Expression::AtomicExpression(atomic) => IrExpression::AtomicExpression(match atomic {
            AtomicExpression::Literal(literal) => IrAtomicExpression::Literal(match literal {
                LiteralValue::Null => IrLiteralValue::Null,
                LiteralValue::Bool(val) => IrLiteralValue::Bool(*val),
                LiteralValue::Int(val) => IrLiteralValue::Int(*val),
                LiteralValue::Decimal(val) => IrLiteralValue::Decimal(*val),
                LiteralValue::String(val) => IrLiteralValue::String(val.clone()),
                LiteralValue::Compound(val) => {
                    IrLiteralValue::Compound(convert_compound(package_name, val))
                }
            }),
            AtomicExpression::Variable(var) => {
                IrAtomicExpression::Variable(convert_name_path(package_name, var))
            }
            AtomicExpression::FnCall(fn_call) => {
                IrAtomicExpression::FnCall(Box::from(convert_fn_call(package_name, fn_call)))
            }
        }),
        Expression::Unary(unop, expr) => IrExpression::Unary(
            convert_unary_op(unop),
            Box::from(convert_expr(package_name, expr)),
        ),
        Expression::Binary(e0, binop, e1) => IrExpression::Binary(
            Box::from(convert_expr(package_name, e0)),
            convert_binary_op(binop),
            Box::from(convert_expr(package_name, e1)),
        ),
    };
}

fn convert_var_def(package_name: &str, ast_node: &VarDef) -> IrVarDef {
    IrVarDef {
        mods: Rc::new(
            ast_node
                .mods
                .iter()
                .map(|x| match x {
                    VarMod::Const => IrVarMod::Const,
                })
                .collect(),
        ),
        name: convert_reference(package_name, &ast_node.name),
        type_: convert_type(package_name, &ast_node.type_),
    }
}

fn convert_var_decl(package_name: &str, ast_node: &VarDecl) -> IrVarDecl {
    IrVarDecl {
        var_def: convert_var_def(package_name, &ast_node.var_def),
        expr: match &ast_node.expr {
            Some(x) => Some(Box::from(convert_expr(package_name, x))),
            None => None,
        },
    }
}

fn convert_name_path(package_name: &str, ast_node: &NamePath) -> IrNamePath {
    IrNamePath {
        name: global_name_updater(
            package_name,
            ast_node.name.global_resolved.as_ref().unwrap(),
        ),
        path: ast_node.path.clone(),
    }
}

fn convert_var_assign(package_name: &str, ast_node: &VarAssign) -> IrVarAssign {
    IrVarAssign {
        name_path: convert_name_path(package_name, &ast_node.name_path),
        expr: Box::from(convert_expr(package_name, &ast_node.expr)),
    }
}

fn convert_if(package_name: &str, ast_node: &If) -> IrIf {
    IrIf {
        cond: Box::from(convert_expr(package_name, &ast_node.cond)),
        body: Box::from(convert_block(package_name, &ast_node.body)),
        else_: match &ast_node.else_ {
            Some(x) => Some(Box::from(convert_if(package_name, x))),
            None => None,
        },
    }
}

fn convert_while(package_name: &str, ast_node: &While) -> IrWhile {
    IrWhile {
        cond: Box::from(convert_expr(package_name, &ast_node.cond)),
        body: Box::from(convert_block(package_name, &ast_node.body)),
    }
}

fn convert_for(package_name: &str, ast_node: &For) -> IrFor {
    IrFor {
        init: match &ast_node.init {
            Some(x) => Some(Box::from(convert_statement(package_name, x))),
            None => None,
        },
        cond: match &ast_node.cond {
            Some(x) => Some(Box::from(convert_expr(package_name, x))),
            None => None,
        },
        step: match &ast_node.step {
            Some(x) => Some(Box::from(convert_statement(package_name, x))),
            None => None,
        },
        body: convert_block(package_name, &ast_node.body),
    }
}

fn convert_statement(package_name: &str, ast_node: &Statement) -> IrStatement {
    match ast_node {
        Statement::VarDecl(x) => IrStatement::VarDecl(convert_var_decl(package_name, x)),
        Statement::VarAssign(x) => IrStatement::VarAssign(convert_var_assign(package_name, x)),
        Statement::If(x) => IrStatement::If(convert_if(package_name, x)),
        Statement::While(x) => IrStatement::While(convert_while(package_name, x)),
        Statement::For(x) => IrStatement::For(convert_for(package_name, x)),
        Statement::Return(x) => IrStatement::Return(Box::from(convert_expr(package_name, x))),
        Statement::Break => IrStatement::Break,
        Statement::Continue => IrStatement::Continue,
        Statement::Expression(x) => {
            IrStatement::Expression(Box::from(convert_expr(package_name, x)))
        }
    }
}

fn convert_statement_block(package_name: &str, ast_node: &StatementBlock) -> IrStatementBlock {
    match ast_node {
        StatementBlock::Statement(x) => {
            IrStatementBlock::Statement(convert_statement(package_name, x))
        }
        StatementBlock::Block(x) => IrStatementBlock::Block(convert_block(package_name, x)),
    }
}

fn convert_block(package_name: &str, ast_node: &Block) -> IrBlock {
    IrBlock {
        statements: ast_node
            .statements
            .iter()
            .map(|x| convert_statement_block(package_name, x))
            .collect(),
    }
}

fn convert_reference(package_name: &str, ast_node: &Reference) -> String {
    return global_name_updater(package_name, ast_node.global_resolved.as_ref().unwrap());
}

fn convert_type(package_name: &str, ast_node: &Type) -> IrType {
    match ast_node {
        Type::Void => IrType::Void,
        Type::Int => IrType::Int,
        Type::Float => IrType::Float,
        Type::Double => IrType::Double,
        Type::Bool => IrType::Bool,
        Type::String => IrType::String,
        Type::Struct(x) => IrType::Struct(convert_reference(package_name, x)),
    }
}

pub fn convert_fn(package_name: &str, ast_node: &FnDef) -> IrFnDef {
    IrFnDef {
        body: convert_block(package_name, ast_node.body.as_ref().unwrap()),
        name: convert_reference(package_name, &ast_node.name),
        mods: Rc::from(
            ast_node
                .mods
                .iter()
                .map(|x| match x {
                    FnMod::Rec => IrFnMod::Rec,
                    FnMod::Inline => IrFnMod::Inline,
                })
                .collect::<Vec<IrFnMod>>(),
        ),
        args: ast_node
            .args
            .iter()
            .map(|x| convert_var_def(package_name, x))
            .collect(),
        return_type: convert_type(package_name, &ast_node.return_type),
    }
}

pub fn convert_struct(package_name: &str, ast_node: &StructDef) -> IrStructDef {
    IrStructDef {
        mods: Rc::from(vec![]),
        type_name: convert_reference(package_name, &ast_node.type_name),
        map: ast_node
            .map
            .iter()
            .map(|(k, v)| (k.clone(), convert_type(package_name, v)))
            .collect(),
    }
}

pub fn convert_global_var(package_name: &str, ast_node: &VarDecl) -> IrVarDecl {
    convert_var_decl(package_name, ast_node)
}
