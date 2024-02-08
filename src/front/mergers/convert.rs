pub mod context;

use crate::front::ast_types::{AtomicExpression, Block, Expression, FnCall, FnDef, For, GlobalResolvedName, If, LiteralValue, Reference, Statement, StatementBlock, VarAssign, VarDecl, While};
use crate::middle::format::ir_types::{IrBlock, IrFnCall, IrFnDef, IrIf, IrStatement, IrScoreSet, Address, Cond, CheckVal, IrUnless, IrScoreOperation, IrScoreOperationType};
use crate::middle::format::types::GlobalName;
use std::rc::Rc;
use crate::front::mergers::convert::context::Context;
use crate::front::mergers::definition_table::DefinitionTable;

pub fn global_name_updater(
    package_name: &str,
    global_resolved_name: &Rc<GlobalResolvedName>,
) -> GlobalName {
    format!(
        "{}{}/{}",
        package_name, global_resolved_name.module, global_resolved_name.name
    )
}

// fn convert_compound_value(context: &mut Context, ast_node: &CompoundValue) -> IrCompoundValue {
//     return match ast_node {
//         CompoundValue::Expression(expr) => {
//             IrCompoundValue::Expression(Box::from(convert_expr(context, expr)))
//         }
//         CompoundValue::Compound(compound) => {
//             IrCompoundValue::Compound(Box::from(convert_compound(context, compound)))
//         }
//     };
// }
//
// fn convert_compound(context: &mut Context, ast_node: &Compound) -> IrCompound {
//     return ast_node
//         .iter()
//         .map(|(k, v)| (k.clone(), convert_compound_value(context, v)))
//         .collect();
// }
//
fn convert_fn_call(context: &mut Context, ast_node: &FnCall) -> Vec<IrStatement> {
    let mut s: Vec<IrStatement> = vec![];
    for (i, arg) in &ast_node.args.iter() {
        s.append(&mut convert_expr(context, arg, &context.get_parameter_variable(ast_node.name.global_resolved.unwrap(), i)));
    }
    s.push(IrStatement::FnCall(IrFnCall {
        fn_name: convert_reference(&context.package_name, &ast_node.name),
    }));
    s
}

//
// fn convert_unary_op(ast_node: &UnOp) -> IrUnOp {
//     return match ast_node {
//         UnOp::Neg => IrUnOp::Neg,
//         UnOp::Not => IrUnOp::Not,
//         UnOp::Deref => IrUnOp::Deref,
//         UnOp::Ref => IrUnOp::Ref,
//         UnOp::PreInc => IrUnOp::PreInc,
//         UnOp::PreDec => IrUnOp::PreDec,
//         UnOp::PostInc => IrUnOp::PostInc,
//         UnOp::PostDec => IrUnOp::PostDec,
//     };
// }
//
// fn convert_binary_op(ast_node: &BinOp) -> IrBinOp {
//     return match ast_node {
//         BinOp::Add => IrBinOp::Add,
//         BinOp::Sub => IrBinOp::Sub,
//         BinOp::Mul => IrBinOp::Mul,
//         BinOp::Div => IrBinOp::Div,
//         BinOp::Mod => IrBinOp::Mod,
//         BinOp::Eq => IrBinOp::Eq,
//         BinOp::Neq => IrBinOp::Neq,
//         BinOp::Lt => IrBinOp::Lt,
//         BinOp::Gt => IrBinOp::Gt,
//         BinOp::Leq => IrBinOp::Leq,
//         BinOp::Geq => IrBinOp::Geq,
//         BinOp::And => IrBinOp::And,
//         BinOp::Or => IrBinOp::Or,
//     };
// }
//

fn set_from_atomic(context: &mut Context, ast_node: &AtomicExpression, result_var_name: &Address) -> Vec<IrStatement> {
    match ast_node {
        AtomicExpression::Literal(x) => {
            match x {
                LiteralValue::Null => {
                    vec![IrStatement::ScoreSet(IrScoreSet {
                        var_name: result_var_name.clone(),
                        value: 0,
                    })]
                }
                LiteralValue::Bool(b) => {
                    vec![IrStatement::ScoreSet(IrScoreSet {
                        var_name: result_var_name.clone(),
                        value: b.clone() as i32,
                    })]
                }
                LiteralValue::Int(x) => {
                    vec![IrStatement::ScoreSet(IrScoreSet {
                        var_name: result_var_name.clone(),
                        value: x.clone(),
                    })]
                }
                // LiteralValue::Decimal(_) => {}
                // LiteralValue::String(_) => {}
                // LiteralValue::Compound(_) => {}
                _ => panic!("Not implemented") // TODO: implement storage types
            }
        }
        AtomicExpression::Variable(x) => {
            // TODO: this only works with score types for now
            vec![IrStatement::ScoreOperation(IrScoreOperation {
                left: result_var_name.clone(),
                op: IrScoreOperationType::Assign,
                right: context.convert_name_path(x),
            })]
        }
        AtomicExpression::FnCall(x) => {
            let mut s = vec![];
            s.append(&mut convert_fn_call(context, x));

            s.push(IrStatement::ScoreOperation(IrScoreOperation {
                left: result_var_name.clone(),
                op: IrScoreOperationType::Assign,
                right: context.get_return_variable(),
            }));
            s
        }
    }
}

fn rec_convert_expr(context: &mut Context, ast_node: &Expression, result_var_name: &Address, a: &Address, other: &Address) -> Vec<IrStatement> {
    return match ast_node {
        Expression::AtomicExpression(x) => set_from_atomic(context, x, result_var_name),
        Expression::Unary(_, _) => {}
        Expression::Binary(_, _, _) => {}
    };
}

fn convert_expr(context: &mut Context, ast_node: &Expression, result_var_name: &Address) -> Vec<IrStatement> {
    let a0 = context.create_variable();
    let a1 = context.create_variable();

    return rec_convert_expr(context, ast_node, result_var_name, &a0, &a1);
}

fn convert_var_decl(context: &mut Context, ast_node: &VarDecl) -> Vec<IrStatement> {
    if let Some(expr) = &ast_node.expr {
        convert_expr(context, expr, &context.convert_var_name(&ast_node.var_def.name))
    } else {
        vec![]
    }
}

fn convert_var_assign(context: &mut Context, ast_node: &VarAssign) -> Vec<IrStatement> {
    convert_expr(context, &ast_node.expr, &context.convert_name_path(&ast_node.name_path))
}

fn convert_if(context: &mut Context, ast_node: &If) -> Vec<IrStatement> {
    /**
    Convert the if statement to a series of commands

    if cond {
        body
    } else if cond {
        body
    } else {
        body
    }

    becomes

    set if_check to 1
    compute cond
    execute if cond run {
        body
        set if_check to 0
    }

    execute if if_check == 1 run {
        compute cond
        execute if cond run {
            body
            set if_check to 0
        }
    }
    execute if if_check == 1 run {
        body
    }
     */
    let mut elses = vec![];

    let mut cur = ast_node;

    while let Some(next) = cur.else_.as_ref() {
        cur = next;
        elses.push((&next.cond, &next.body));
    }

    let if_variable = context.get_if_variable();

    let mut s = vec![];

    if elses.len() > 0 {        // Set if_check to 1, only needed if there are elses.
        s.push(IrStatement::ScoreSet(IrScoreSet {
            var_name: if_variable.clone(),
            value: 1,
        }));
    }

    let cond_var = context.create_variable();
    // compute cond
    s.append(&mut convert_expr(context, &ast_node.cond, &cond_var));

    // execute if cond run {
    s.push(IrStatement::Unless(IrUnless {
        cond: Cond::CheckVal(CheckVal {
            var_name: cond_var,
            min: 0,
            max: 0,
        }),
        body: IrStatement::Block({
            let mut s = convert_block(context, &ast_node.body);
            if elses.len() > 0 {
                s.statements.push(IrStatement::ScoreSet(IrScoreSet {
                    var_name: if_variable.clone(),
                    value: 0,
                }));
            }
            s
        }),
    }));

    // TODO: could be optimized

    for (cond, body) in elses {
        // compute cond
        s.append(&mut convert_expr(context, &cond, &cond_var));

        // execute if if_check == 1 run {
        s.push(IrStatement::Unless(IrUnless {
            cond: Cond::CheckVal(CheckVal {
                var_name: if_variable.clone(),
                min: 0,
                max: 0,
            }),
            body: IrStatement::Unless(IrUnless {
                cond: Cond::CheckVal(CheckVal {
                    var_name: cond_var.clone(),
                    min: 0,
                    max: 0,
                }),
                body: IrStatement::Block({
                    let mut s = convert_block(context, &body);
                    s.statements.push(IrStatement::ScoreSet(IrScoreSet {
                        var_name: if_variable.clone(),
                        value: 0,
                    }));
                    s
                }),
            }),
        }));
    }
    s
}


fn convert_while(context: &mut Context, ast_node: &While) -> Vec<IrStatement> {
    let mut s: Vec<IrStatement> = vec![];

    let mut condition = vec![];

    // if condition is 0, return
    if let Some(cond) = &ast_node.cond {
        let add = context.create_variable();
        condition.append(&mut convert_expr(context, cond, &add));
        condition.push(IrStatement::If(IrIf {
            cond: Cond::CheckVal(CheckVal {
                var_name: add,
                min: 0,
                max: 0,
            }),
            body: IrStatement::Return,
        }));
    }

    // parse body
    let mut body = convert_block(context, &ast_node.body);

    // insert condition before body
    condition.append(&mut body.statements);
    body.statements = condition;

    // insert recursion
    body.statements.push(IrStatement::FnCall(IrFnCall {
        fn_name: body.get_fn_name(),
    }));
    s.push(IrStatement::Block(body));
    s
}

fn convert_for(context: &mut Context, ast_node: &For) -> Vec<IrStatement> {
    let mut s: Vec<IrStatement> = vec![];
    if let Some(init) = &ast_node.init {
        s.append(&mut convert_statement(context, init));
    }

    let mut condition = vec![];

    // if condition is 0, return
    if let Some(cond) = &ast_node.cond {
        let add = context.create_variable();
        condition.append(&mut convert_expr(context, cond, &add));
        condition.push(IrStatement::If(IrIf {
            cond: Cond::CheckVal(CheckVal {
                var_name: add,
                min: 0,
                max: 0,
            }),
            body: IrStatement::Return,
        }));
    }

    // parse body
    let mut body = convert_block(context, &ast_node.body);

    // insert condition before body
    condition.append(&mut body.statements);
    body.statements = condition;

    // insert step statement after body
    body.statements.append(&mut {
        if let Some(step) = &ast_node.step {
            convert_statement(context, step)
        } else {
            vec![]
        }
    });

    // insert recursion
    body.statements.push(IrStatement::FnCall(IrFnCall {
        fn_name: body.get_fn_name(),
    }));
    s.push(IrStatement::Block(body));
    s
}

fn convert_statement(context: &mut Context, ast_node: &Statement) -> Vec<IrStatement> {
    return match ast_node {
        Statement::VarDecl(x) => convert_var_decl(context, x),
        Statement::VarAssign(x) => convert_var_assign(context, x),
        Statement::If(x) => convert_if(context, x),
        Statement::While(x) => convert_while(context, x),
        Statement::For(x) => convert_for(context, x),
        _ => (panic!("Not implemented"))
    };
}

fn convert_statement_block(context: &mut Context, ast_node: &StatementBlock) -> IrStatement {
    return match ast_node {
        StatementBlock::Block(x) => {
            IrStatement::Block(convert_block(context, x))
        }
        StatementBlock::Statement(statement) => convert_statement(context, statement),
    };
}

fn convert_block(context: &mut Context, ast_node: &Block) -> IrBlock {
    IrBlock {
        root_fn_name: context.fn_name.to_string(),
        fn_block_index: context.use_block() as usize,
        statements: ast_node
            .statements
            .iter()
            .map(|x| convert_statement_block(context, x))
            .collect(),
    }
}

fn convert_reference(package_name: &str, ast_node: &Reference) -> String {
    return global_name_updater(package_name, ast_node.global_resolved.as_ref().unwrap());
}

pub fn convert_fn(package_name: &str, ast_node: &FnDef, definition_table: &DefinitionTable<Rc<GlobalResolvedName>>) -> IrFnDef {
    let fn_name = convert_reference(package_name, &ast_node.name);
    let mut ctx = Context::new(package_name, &fn_name, definition_table);

    IrFnDef {
        fn_name: convert_reference(package_name, &ast_node.name),
        body: convert_block(&mut ctx, ast_node.body.as_ref().unwrap()),
    }
}