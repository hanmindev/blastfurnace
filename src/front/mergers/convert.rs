pub mod context;

use crate::front::ast_types::{
    AtomicExpression, BinOp, Block, Expression, FnCall, FnDef, For, GlobalResolvedName, If,
    LiteralValue, Reference, Statement, StatementBlock, UnOp, VarAssign, VarDecl, While,
};
use crate::front::mergers::convert::context::Context;
use crate::front::mergers::definition_table::DefinitionTable;
use crate::middle::format::ir_types::{
    Address, CheckVal, CompareOp, CompareVal, Cond, IrBlock, IrFnCall, IrFnDef, IrIf,
    IrScoreOperation, IrScoreOperationType, IrStatement,
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
    _result_var_name: &Address,
) -> ExprEval {
    match ast_node {
        AtomicExpression::Literal(x) => {
            match x {
                LiteralValue::Null => ExprEval {
                    statements: vec![],
                    existing_address: Some(context.const_generator.get_const(0)),
                },
                LiteralValue::Bool(b) => ExprEval {
                    statements: vec![],
                    existing_address: Some(context.const_generator.get_const(b.clone() as i32)),
                },
                LiteralValue::Int(x) => ExprEval {
                    statements: vec![],
                    existing_address: Some(context.const_generator.get_const(x.clone())),
                },
                // LiteralValue::Decimal(_) => {}
                // LiteralValue::String(_) => {}
                // LiteralValue::Compound(_) => {}
                _ => panic!("Not implemented"), // TODO: implement storage types
            }
        }
        AtomicExpression::Variable(x) => {
            // TODO: this only works with score types for now
            ExprEval {
                statements: vec![],
                existing_address: Some(context.convert_name_path(x)),
            }
        }
        AtomicExpression::FnCall(x) => {
            let mut s = vec![];
            s.append(&mut convert_fn_call(context, x));

            ExprEval {
                statements: s,
                existing_address: Some(context.get_return_variable()),
            }
        }
    }
}

struct ExprEval {
    statements: Vec<IrStatement>,
    existing_address: Option<Address>,
}

fn rec_convert_expr(
    context: &mut Context,
    ast_node: &Expression,
    result_var_name: &Address,
) -> ExprEval {
    return match ast_node {
        Expression::AtomicExpression(x) => set_from_atomic(context, x, result_var_name),
        Expression::Unary(unop, e) => {
            let mut s = vec![];

            let mut expr = rec_convert_expr(context, e, &result_var_name);
            s.append(&mut expr.statements);
            if let Some(e_a) = expr.existing_address {
                s.push(IrStatement::ScoreOperation(IrScoreOperation {
                    left: result_var_name.clone(),
                    op: IrScoreOperationType::Assign,
                    right: e_a,
                }));
            }

            match unop {
                UnOp::Neg => s.push(IrStatement::ScoreOperation(IrScoreOperation {
                    left: result_var_name.clone(),
                    op: IrScoreOperationType::Mul,
                    right: context.const_generator.get_const(-1),
                })),
                UnOp::Not => {
                    s.push(IrStatement::ScoreOperation(IrScoreOperation {
                        left: result_var_name.clone(),
                        op: IrScoreOperationType::Eq,
                        right: context.const_generator.get_const(0),
                    })) // TODO can use match instead
                }
                // UnOp::Deref => IrScoreOperationType::Deref, // TODO
                // UnOp::Ref => IrScoreOperationType::Ref,
                // UnOp::PreInc => {},
                // UnOp::PreDec => IrScoreOperationType::PreDec,
                // UnOp::PostInc => IrScoreOperationType::PostInc,
                // UnOp::PostDec => IrScoreOperationType::PostDec,
                _ => {}
            };

            ExprEval {
                statements: s,
                existing_address: None,
            }
        }
        Expression::Binary(e0, binop, e1) => {
            let mut s = vec![];

            let mut expr0 = rec_convert_expr(context, e0, result_var_name);
            expr0.existing_address.as_ref().unwrap_or(result_var_name);
            s.append(&mut expr0.statements);
            if let Some(e_a) = expr0.existing_address {
                if result_var_name != &e_a {
                    s.push(IrStatement::ScoreOperation(IrScoreOperation {
                        left: result_var_name.clone(),
                        op: IrScoreOperationType::Assign,
                        right: e_a,
                    }));
                }
            }

            let a0 = context.get_variable();
            let mut expr1 = rec_convert_expr(context, e1, &a0);
            let mut f = false;
            let existing_address1 = if let Some(e_a) = expr1.existing_address {
                context.forfeit_variable(&a0);
                e_a
            } else {
                f = true;
                a0.clone()
            };
            s.append(&mut expr1.statements);
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
                right: existing_address1,
            }));
            if f {
                context.forfeit_variable(&a0);
            }

            ExprEval {
                statements: s,
                existing_address: None,
            }
        }
    };
}

fn convert_expr(
    context: &mut Context,
    ast_node: &Expression,
    result_var_name: &Address,
) -> Vec<IrStatement> {
    let expr = rec_convert_expr(context, ast_node, result_var_name);
    let mut s = expr.statements;

    if let Some(e) = expr.existing_address {
        s.push(IrStatement::ScoreOperation(IrScoreOperation {
            left: result_var_name.clone(),
            op: IrScoreOperationType::Assign,
            right: e,
        }))
    }

    s
}

fn convert_expr_for_comparison(
    context: &mut Context,
    ast_node: &Expression,
) -> (Vec<IrStatement>, Cond, bool) {
    match ast_node {
        Expression::AtomicExpression(x) => {
            let a0 = context.get_variable();
            let expr = set_from_atomic(context, x, &a0);
            let mut s = expr.statements;
            let address = expr.existing_address.unwrap_or(a0.clone());
            context.forfeit_variable(&a0);
            return (
                s,
                Cond::CheckVal(CheckVal {
                    var_name: address,
                    min: 0,
                    max: 0,
                }),
                false,
            );
        }
        Expression::Unary(unop, x) => {
            if matches!(unop, UnOp::Not) {
                let a0 = context.get_variable();
                let expr = rec_convert_expr(context, x, &a0);
                let mut s = expr.statements;
                let address = expr.existing_address.unwrap_or(a0.clone());
                context.forfeit_variable(&a0);

                return (
                    s,
                    Cond::CheckVal(CheckVal {
                        var_name: address,
                        min: 0,
                        max: 0,
                    }),
                    true,
                );
            }
        }
        Expression::Binary(e0, binop, e1) => match binop {
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => {
                let a0 = context.get_variable();
                let mut expr0 = rec_convert_expr(context, e0, &a0);
                let a1 = context.get_variable();
                let mut expr1 = rec_convert_expr(context, e1, &a1);

                let mut s = expr0.statements;
                s.append(&mut expr1.statements);

                let address0 = expr0.existing_address.unwrap_or(a0.clone());
                let address1 = expr1.existing_address.unwrap_or(a1.clone());
                context.forfeit_variable(&a0);
                context.forfeit_variable(&a1);

                return (
                    s,
                    Cond::CompareVal({
                        CompareVal {
                            var_0: address0,
                            op: match binop {
                                BinOp::Eq => CompareOp::Eq,
                                BinOp::Neq => CompareOp::Neq,
                                BinOp::Lt => CompareOp::Lt,
                                BinOp::Gt => CompareOp::Gt,
                                BinOp::Leq => CompareOp::Leq,
                                BinOp::Geq => CompareOp::Geq,
                                _ => panic!("Impossible, match arms must be incorrect"),
                            },
                            var_1: address1,
                        }
                    }),
                    false,
                );
            }
            _ => {}
        },
    }

    let a0 = context.get_variable();
    let expr = convert_expr(context, ast_node, &a0);

    (
        expr,
        Cond::CheckVal(CheckVal {
            var_name: a0,
            min: 0,
            max: 0,
        }),
        false,
    )
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

fn convert_condition(
    context: &mut Context,
    cond: &Box<Expression>,
    invert_cond: bool,
    body: IrStatement,
) -> Vec<IrStatement> {
    let mut condition = vec![];
    // if condition is 0, return
    let (mut expr_statements, cond, invert) = convert_expr_for_comparison(context, cond);
    condition.append(&mut expr_statements);
    condition.push(IrStatement::If(IrIf {
        invert: invert != invert_cond,
        cond,
        body: Box::from(body),
    }));
    return condition;
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
        s.push(IrStatement::ScoreOperation(IrScoreOperation {
            left: if_variable.clone(),
            op: IrScoreOperationType::Assign,
            right: context.const_generator.get_const(1),
        }));
    }

    // compute block for first if statement
    let block = IrStatement::Block({
        let mut s = convert_block(context, &ast_node.body, true);
        if elses.len() > 0 {
            s.statements
                .push(IrStatement::ScoreOperation(IrScoreOperation {
                    left: if_variable.clone(),
                    op: IrScoreOperationType::Assign,
                    right: context.const_generator.get_const(0),
                }));
        }
        s
    });
    // write if statement
    s.append(&mut convert_condition(
        context,
        &ast_node.cond,
        false,
        block,
    ));

    for (condition, body) in elses {
        let block = IrStatement::Block({
            let mut s = convert_block(context, &body, true);
            s.statements
                .push(IrStatement::ScoreOperation(IrScoreOperation {
                    left: if_variable.clone(),
                    op: IrScoreOperationType::Assign,
                    right: context.const_generator.get_const(0),
                }));
            s
        });

        // for if else
        let else_block = IrStatement::Block(IrBlock {
            can_embed: true,
            root_fn_name: context.fn_name.clone(),
            fn_block_index: context.use_block(),
            statements: convert_condition(context, condition, false, block),
        });
        // execute if if_check == 1 run {
        s.push(IrStatement::If(IrIf {
            invert: true,
            cond: Cond::CheckVal(CheckVal {
                var_name: if_variable.clone(),
                min: 0,
                max: 0,
            }),
            body: Box::from(else_block),
        }));
    }
    s
}

fn convert_while(context: &mut Context, ast_node: &While) -> Vec<IrStatement> {
    let mut s: Vec<IrStatement> = vec![];

    let mut condition = convert_condition(context, &ast_node.cond, true, IrStatement::Return);

    // parse body
    let mut body = convert_block(context, &ast_node.body, false);

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
    if let Some(cond) = &ast_node.cond {
        condition.append(&mut convert_condition(
            context,
            cond,
            true,
            IrStatement::Return,
        ));
    }

    // parse body
    let mut body = convert_block(context, &ast_node.body, false);

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
            vec![IrStatement::Block(convert_block(context, x, true))]
        }
        StatementBlock::Statement(statement) => convert_statement(context, statement),
    };
}

fn convert_block(context: &mut Context, ast_node: &Block, can_embed: bool) -> IrBlock {
    let mut statements = vec![];
    for statement_block in &ast_node.statements {
        statements.append(&mut convert_statement_block(context, statement_block));
    }

    IrBlock {
        can_embed,
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
        statements: convert_block(&mut ctx, ast_node.body.as_ref().unwrap(), true).statements,
        block_count: ctx.block_count,
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::middle::format::ir_types::IrFnDef;
    use crate::middle::format::ir_types::{
        Address, AddressOrigin, CompareOp, Cond, IrScoreOperationType, IrStatement,
    };
    use crate::middle::format::types::GlobalName;
    use camino::Utf8PathBuf;
    use std::collections::HashMap;
    use std::ops::Deref;

    fn test_calculation(
        run_function: &str,
        functions: &HashMap<GlobalName, IrFnDef>,
        result_address: &Address,
    ) -> i32 {
        struct Vars {
            var_map: HashMap<Address, i32>,
        }
        impl Vars {
            fn get(&self, address: &Address) -> i32 {
                match &address.name {
                    AddressOrigin::Const(c) => {
                        return *c;
                    }
                    _ => {}
                }
                return *self.var_map.get(address).unwrap();
            }

            fn insert(&mut self, address: Address, value: i32) {
                if matches!(address.name, AddressOrigin::Const(_)) {
                    panic!("Cannot insert into const")
                }

                self.var_map.insert(address, value);
            }
        }

        fn run_statements(
            curr_fn_name: &str,
            statements: &Vec<IrStatement>,
            vars_ref: &mut Vars,
            functions: &HashMap<GlobalName, IrFnDef>,
        ) -> bool {
            for statement in statements {
                match statement {
                    IrStatement::ScoreOperation(x) => {
                        let left = match x.op {
                            IrScoreOperationType::Assign => 0,
                            _ => vars_ref.get(&x.left),
                        };
                        let right = vars_ref.get(&x.right);
                        let result = match x.op {
                            IrScoreOperationType::Add => left + right,
                            IrScoreOperationType::Sub => left - right,
                            IrScoreOperationType::Mul => left * right,
                            IrScoreOperationType::Div => left / right,
                            IrScoreOperationType::Mod => left % right,
                            IrScoreOperationType::Eq => ((left != 0) == (right != 0)) as i32,
                            IrScoreOperationType::Neq => ((left != 0) != (right != 0)) as i32,
                            IrScoreOperationType::Lt => ((left != 0) < (right != 0)) as i32,
                            IrScoreOperationType::Gt => ((left != 0) > (right != 0)) as i32,
                            IrScoreOperationType::Leq => ((left != 0) <= (right != 0)) as i32,
                            IrScoreOperationType::Geq => ((left != 0) >= (right != 0)) as i32,
                            IrScoreOperationType::And => ((left != 0) && (right != 0)) as i32,
                            IrScoreOperationType::Or => ((left != 0) || (right != 0)) as i32,
                            IrScoreOperationType::Assign => right,
                        };
                        vars_ref.insert(x.left.clone(), result);
                    }
                    IrStatement::If(x) => match &x.cond {
                        Cond::CheckVal(y) => {
                            let a = vars_ref.get(&y.var_name);
                            if (y.min <= a && a <= y.max) != x.invert {
                                run_statements(
                                    curr_fn_name,
                                    &vec![(*x.body.deref()).clone()],
                                    vars_ref,
                                    functions,
                                );
                            }
                        }
                        Cond::CompareVal(y) => {
                            let a = vars_ref.get(&y.var_0);
                            let b = vars_ref.get(&y.var_1);
                            if match y.op {
                                CompareOp::Eq => a == b,
                                CompareOp::Neq => a != b,
                                CompareOp::Lt => a < b,
                                CompareOp::Gt => a > b,
                                CompareOp::Leq => a <= b,
                                CompareOp::Geq => a >= b,
                            } != x.invert
                            {
                                if run_statements(
                                    curr_fn_name,
                                    &vec![(*x.body.deref()).clone()],
                                    vars_ref,
                                    functions,
                                ) {
                                    return false;
                                }
                            }
                        }
                    },
                    IrStatement::Return => {
                        return true;
                    }
                    IrStatement::Block(x) => {
                        run_statements(&x.get_fn_name(), &x.statements, vars_ref, functions);
                    }
                    IrStatement::FnCall(x) => {
                        if &x.fn_name == curr_fn_name {
                            run_statements(&x.fn_name, statements, vars_ref, functions);
                        } else {
                            run_statements(
                                &x.fn_name,
                                &functions.get(&x.fn_name).unwrap().statements,
                                vars_ref,
                                functions,
                            );
                        }
                    }
                    _ => {
                        panic!("{:?} Not implemented", statement)
                    }
                }
            }
            return false;
        }

        let mut vars = Vars {
            var_map: HashMap::new(),
        };
        run_statements(
            "pkg/root/0_main",
            &functions.get("pkg/root/0_main").unwrap().statements,
            &mut vars,
            &functions,
        );

        return vars.get(result_address);
    }

    #[test]
    fn test_convert_simple() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 1; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        match &program
            .function_definitions
            .get("pkg/root/0_main")
            .unwrap()
            .statements[0]
        {
            IrStatement::ScoreOperation(x) => {
                assert_eq!(x.left.name, AddressOrigin::User("pkg/root/0_a".to_string()));
                assert_eq!(
                    x.right,
                    Address {
                        name: AddressOrigin::Const(1),
                        offset: 0,
                    }
                );
            }
            _ => {}
        }
    }

    #[test]
    fn test_simple_expression() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 8; let b: int = 2 * a / 8 + 9; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                "pkg/root/0_main",
                &program.function_definitions,
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
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 2 * 18 / 9 * (6 - 8 * 3 % 3) + (true && (false || true) && !false); }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                "pkg/root/0_main",
                &program.function_definitions,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_a".to_string()),
                    offset: 0,
                },
            ),
            25
        );
    }

    #[test]
    fn test_if_else() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 3; let r: int = 0; if (a == 1) { r = 1; } else if (a == 2) { r = 2; } else if (a == 3) { r = 3; } }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                "pkg/root/0_main",
                &program.function_definitions,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_r".to_string()),
                    offset: 0,
                },
            ),
            3
        );
    }

    #[test]
    fn test_while() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 3; while (a < 10) { a+=1; } }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                "pkg/root/0_main",
                &program.function_definitions,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_a".to_string()),
                    offset: 0,
                },
            ),
            10
        );
    }

    #[test]
    fn test_for() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { for (let a: int = 0; a < 10; a += 1) { } }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        assert_eq!(
            test_calculation(
                "pkg/root/0_main",
                &program.function_definitions,
                &Address {
                    name: AddressOrigin::User("pkg/root/0_a".to_string()),
                    offset: 0,
                },
            ),
            10
        );
    }
}
