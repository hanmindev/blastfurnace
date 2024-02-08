pub mod context;

use crate::front::ast_types::{
    AtomicExpression, BinOp, Block, Expression, FnCall, FnDef, For, GlobalResolvedName, If,
    LiteralValue, Reference, Statement, StatementBlock, UnOp, VarAssign, VarDecl, While,
};
use crate::front::mergers::convert::context::Context;
use crate::front::mergers::definition_table::DefinitionTable;
use crate::middle::format::ir_types::{
    Address, CheckVal, Cond, IrBlock, IrFnCall, IrFnDef, IrIf,
    IrScoreOperation, IrScoreOperationType, IrScoreSet, IrStatement, IrUnless,
};
use crate::middle::format::types::GlobalName;
use std::rc::Rc;

pub fn global_name_updater(global_resolved_name: &Rc<GlobalResolvedName>) -> GlobalName {
    format!(
        "{}{}/{}",
        global_resolved_name.package, global_resolved_name.module, global_resolved_name.name
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
    for (i, arg) in ast_node.args.iter().enumerate() {
        s.append(&mut convert_expr(
            context,
            arg,
            &context
                .get_parameter_variable(&ast_node.name.global_resolved.as_ref().unwrap(), i as u32),
        ));
    }
    s.push(IrStatement::FnCall(IrFnCall {
        fn_name: convert_reference(&ast_node.name),
    }));
    s
}

fn set_from_atomic(
    context: &mut Context,
    ast_node: &AtomicExpression,
    result_var_name: &Address,
) -> Vec<IrStatement> {
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
                _ => panic!("Not implemented"), // TODO: implement storage types
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

fn convert_expr(
    context: &mut Context,
    ast_node: &Expression,
    result_var_name: &Address,
) -> Vec<IrStatement> {
    return match ast_node {
        Expression::AtomicExpression(x) => set_from_atomic(context, x, result_var_name),
        Expression::Unary(unop, e) => {
            let mut s = vec![];
            s.append(&mut convert_expr(context, e, result_var_name));

            s.append(&mut match unop {
                UnOp::Neg => {
                    vec![IrStatement::ScoreOperation(IrScoreOperation {
                        left: result_var_name.clone(),
                        op: IrScoreOperationType::Mul,
                        right: context.const_generator.get_const(-1),
                    })]
                }
                UnOp::Not => {
                    vec![IrStatement::ScoreOperation(IrScoreOperation {
                        left: result_var_name.clone(),
                        op: IrScoreOperationType::Eq,
                        right: context.const_generator.get_const(0),
                    })]
                }
                // UnOp::Deref => IrScoreOperationType::Deref, // TODO
                // UnOp::Ref => IrScoreOperationType::Ref,
                // UnOp::PreInc => {},
                // UnOp::PreDec => IrScoreOperationType::PreDec,
                // UnOp::PostInc => IrScoreOperationType::PostInc,
                // UnOp::PostDec => IrScoreOperationType::PostDec,
                _ => {
                    vec![]
                }
            });

            s
        }
        Expression::Binary(e0, binop, e1) => {
            let mut s = vec![];
            let a0 = context.get_variable();
            s.append(&mut convert_expr(context, e0, result_var_name));
            s.append(&mut convert_expr(context, e1, &a0));

            s.push(IrStatement::ScoreOperation(IrScoreOperation {
                left: result_var_name.clone(),
                op: {
                    match binop {
                        BinOp::Add => IrScoreOperationType::Add,
                        BinOp::Sub => IrScoreOperationType::Sub,
                        BinOp::Mul => IrScoreOperationType::Mul,
                        BinOp::Div => IrScoreOperationType::Div,
                        BinOp::Mod => IrScoreOperationType::Mod,

                        BinOp::Eq => IrScoreOperationType::Eq,
                        BinOp::Neq => IrScoreOperationType::Neq,
                        BinOp::Lt => IrScoreOperationType::Lt,
                        BinOp::Gt => IrScoreOperationType::Gt,
                        BinOp::Leq => IrScoreOperationType::Leq,
                        BinOp::Geq => IrScoreOperationType::Geq,

                        BinOp::And => IrScoreOperationType::And,
                        BinOp::Or => IrScoreOperationType::Or,
                    }
                },
                right: a0.clone(),
            }));
            context.forfeit_variable(&a0);

            s
        }
    };
}

fn convert_var_decl(context: &mut Context, ast_node: &VarDecl) -> Vec<IrStatement> {
    if let Some(expr) = &ast_node.expr {
        convert_expr(
            context,
            expr,
            &context.convert_var_name(&ast_node.var_def.name),
        )
    } else {
        vec![]
    }
}

fn convert_var_assign(context: &mut Context, ast_node: &VarAssign) -> Vec<IrStatement> {
    convert_expr(
        context,
        &ast_node.expr,
        &context.convert_name_path(&ast_node.name_path),
    )
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

    if elses.len() > 0 {
        // Set if_check to 1, only needed if there are elses.
        s.push(IrStatement::ScoreSet(IrScoreSet {
            var_name: if_variable.clone(),
            value: 1,
        }));
    }

    let cond_var = context.get_variable();
    // compute cond
    s.append(&mut convert_expr(context, &ast_node.cond, &cond_var));

    // execute if cond run {
    s.push(IrStatement::Unless(IrUnless {
        cond: Cond::CheckVal(CheckVal {
            var_name: cond_var.clone(),
            min: 0,
            max: 0,
        }),
        body: Box::from(IrStatement::Block({
            let mut s = convert_block(context, &ast_node.body);
            if elses.len() > 0 {
                s.statements.push(IrStatement::ScoreSet(IrScoreSet {
                    var_name: if_variable.clone(),
                    value: 0,
                }));
            }
            s
        })),
    }));

    // TODO: could be optimized

    for (cond, body) in elses {
        // compute cond
        s.append(&mut convert_expr(context, &cond, &cond_var.clone()));

        // execute if if_check == 1 run {
        s.push(IrStatement::Unless(IrUnless {
            cond: Cond::CheckVal(CheckVal {
                var_name: if_variable.clone(),
                min: 0,
                max: 0,
            }),
            body: Box::from(IrStatement::Unless(IrUnless {
                cond: Cond::CheckVal(CheckVal {
                    var_name: cond_var.clone(),
                    min: 0,
                    max: 0,
                }),
                body: Box::from(IrStatement::Block({
                    let mut s = convert_block(context, &body);
                    s.statements.push(IrStatement::ScoreSet(IrScoreSet {
                        var_name: if_variable.clone(),
                        value: 0,
                    }));
                    s
                })),
            })),
        }));
    }
    s
}

fn convert_while(context: &mut Context, ast_node: &While) -> Vec<IrStatement> {
    let mut s: Vec<IrStatement> = vec![];

    let mut condition = vec![];

    // if condition is 0, return
    let add = context.get_variable();
    condition.append(&mut convert_expr(context, &ast_node.cond, &add));
    condition.push(IrStatement::If(IrIf {
        cond: Cond::CheckVal(CheckVal {
            var_name: add,
            min: 0,
            max: 0,
        }),
        body: Box::from(IrStatement::Return),
    }));

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
        let add = context.get_variable();
        condition.append(&mut convert_expr(context, cond, &add));
        condition.push(IrStatement::If(IrIf {
            cond: Cond::CheckVal(CheckVal {
                var_name: add,
                min: 0,
                max: 0,
            }),
            body: Box::from(IrStatement::Return),
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
        _ => panic!("Not implemented"),
    };
}

fn convert_statement_block(context: &mut Context, ast_node: &StatementBlock) -> Vec<IrStatement> {
    return match ast_node {
        StatementBlock::Block(x) => {
            vec![IrStatement::Block(convert_block(context, x))]
        }
        StatementBlock::Statement(statement) => convert_statement(context, statement),
    };
}

fn convert_block(context: &mut Context, ast_node: &Block) -> IrBlock {
    let mut statements = vec![];
    for statement_block in &ast_node.statements {
        statements.append(&mut convert_statement_block(context, statement_block));
    }

    IrBlock {
        root_fn_name: context.fn_name.to_string(),
        fn_block_index: context.use_block() as usize,
        statements,
    }
}

fn convert_reference(ast_node: &Reference) -> String {
    return global_name_updater(ast_node.global_resolved.as_ref().unwrap());
}

pub fn convert_fn(
    ast_node: &FnDef,
    definition_table: &DefinitionTable<Rc<GlobalResolvedName>>,
    const_generator: &mut context::ConstGenerator,
) -> IrFnDef {
    let fn_name = convert_reference(&ast_node.name);
    let mut ctx = Context::new(&fn_name, definition_table, const_generator);

    IrFnDef {
        fn_name: convert_reference(&ast_node.name),
        body: convert_block(&mut ctx, ast_node.body.as_ref().unwrap()),
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::middle::format::ir_types::{
        Address, AddressOrigin, CompareOp, Cond, IrBlock, IrScoreOperationType, IrStatement,
    };
    use std::collections::HashMap;

    fn test_calculation(block: &IrBlock, result_address: &Address) -> i32 {
        let mut vars = HashMap::new();

        fn run_statement(statement: &IrStatement, vars: &mut HashMap<Address, i32>) {
            match statement {
                IrStatement::ScoreSet(x) => {
                    vars.insert(x.var_name.clone(), x.value);
                }
                IrStatement::ScoreAddI(x) => {
                    vars.insert(x.var_name.clone(), vars.get(&x.var_name).unwrap() + x.value);
                }
                IrStatement::ScoreOperation(x) => {
                    let left = vars.get(&x.left).unwrap_or(&0);
                    let right = vars.get(&x.right).unwrap_or(&0);
                    let result = match x.op {
                        IrScoreOperationType::Add => left + right,
                        IrScoreOperationType::Sub => left - right,
                        IrScoreOperationType::Mul => left * right,
                        IrScoreOperationType::Div => left / right,
                        IrScoreOperationType::Mod => left % right,
                        IrScoreOperationType::Eq => ((*left != 0) == (*right != 0)) as i32,
                        IrScoreOperationType::Neq => ((*left != 0) != (*right != 0)) as i32,
                        IrScoreOperationType::Lt => ((*left != 0) < (*right != 0)) as i32,
                        IrScoreOperationType::Gt => ((*left != 0) > (*right != 0)) as i32,
                        IrScoreOperationType::Leq => ((*left != 0) <= (*right != 0)) as i32,
                        IrScoreOperationType::Geq => ((*left != 0) >= (*right != 0)) as i32,
                        IrScoreOperationType::And => ((*left != 0) && (*right != 0)) as i32,
                        IrScoreOperationType::Or => ((*left != 0) || (*right != 0)) as i32,
                        IrScoreOperationType::Assign => *right,
                    };
                    vars.insert(x.left.clone(), result);
                }
                IrStatement::If(x) => match &x.cond {
                    Cond::CheckVal(y) => {
                        let a = *vars.get(&y.var_name).unwrap();
                        if y.min <= a && a <= y.max {
                            run_statement(&x.body, vars);
                        }
                    }
                    Cond::CompareVal(y) => {
                        let a = *vars.get(&y.var_0).unwrap();
                        let b = *vars.get(&y.var_1).unwrap();
                        if match y.op {
                            CompareOp::Eq => a == b,
                            CompareOp::Neq => a != b,
                            CompareOp::Lt => a < b,
                            CompareOp::Gt => a > b,
                            CompareOp::Leq => a <= b,
                            CompareOp::Geq => a >= b,
                        } {
                            run_statement(&x.body, vars);
                        }
                    }
                },
                IrStatement::Unless(x) => match &x.cond {
                    Cond::CheckVal(y) => {
                        let a = *vars.get(&y.var_name).unwrap();
                        if y.min <= a && a <= y.max {
                            run_statement(&x.body, vars);
                        }
                    }
                    Cond::CompareVal(y) => {
                        let a = *vars.get(&y.var_0).unwrap();
                        let b = *vars.get(&y.var_1).unwrap();
                        if match y.op {
                            CompareOp::Eq => a == b,
                            CompareOp::Neq => a != b,
                            CompareOp::Lt => a < b,
                            CompareOp::Gt => a > b,
                            CompareOp::Leq => a <= b,
                            CompareOp::Geq => a >= b,
                        } {
                            run_statement(&x.body, vars);
                        }
                    }
                },
                IrStatement::Return => {
                    return;
                }
                IrStatement::Block(x) => {
                    for statement in &x.statements {
                        run_statement(&statement, vars);
                    }
                }
                IrStatement::FnCall(_) => {} // TODO: not implemented
            }
        }

        for statement in &block.statements {
            run_statement(statement, &mut vars);
        }

        return *vars.get(result_address).unwrap();
    }

    #[test]
    fn test_convert_simple() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "pub fn main() { let a: int = 1; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        match &program
            .function_definitions
            .get("pkg/root/0_main")
            .unwrap()
            .body
            .statements[0]
        {
            IrStatement::ScoreSet(x) => {
                assert_eq!(
                    x.var_name.name,
                    AddressOrigin::User("pkg/root/0_a".to_string())
                );
                assert_eq!(x.value, 1);
            }
            _ => {}
        }
    }

    #[test]
    fn test_simple_expression() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file(
            "/main.ing",
            "pub fn main() { let a: int = 8; let b: int = 2 * a / 8 + 9; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                &program
                    .function_definitions
                    .get("pkg/root/0_main")
                    .unwrap()
                    .body,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_b".to_string()),
                    offset: 0,
                },
            ),
            11
        );
    }

    #[test]
    fn test_complicated_expression() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file(
            "/main.ing",
            "pub fn main() { let a: int = 2 * 18 / 9 * (6 - 8 * 3 % 3) + (true && (false || true) && !false); }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                &program
                    .function_definitions
                    .get("pkg/root/0_main")
                    .unwrap()
                    .body,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_a".to_string()),
                    offset: 0,
                },
            ),
            25
        );
    }
}
