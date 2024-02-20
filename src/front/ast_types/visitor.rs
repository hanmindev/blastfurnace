use crate::front::ast_types::{AtomicExpression, Block, Else, Expression, ExpressionEnum, FnCall, FnDef, For, If, LiteralValue, Statement, VarAssign, VarDecl, VarDef, While};

pub enum ASTNodeEnum<'a> {
    VarDecl(&'a mut VarDecl),
    VarAssign(&'a mut VarAssign),
    VarDef(&'a mut VarDef),

    FnDef(&'a mut FnDef),
    FnCall(&'a mut FnCall),

    LiteralValue(&'a mut LiteralValue),
    AtomicExpression(&'a mut AtomicExpression),
    Expression(&'a mut Expression),

    If(&'a mut If),
    Else(&'a mut Else),
    While(&'a mut While),
    For(&'a mut For),

    Statement(&'a mut Statement),
    Block(&'a mut Block),
}

pub trait Visitor {
    fn apply(&mut self, _ast_node: &mut ASTNodeEnum) -> bool {
        return true;
    }
}

pub trait Visitable<V: Visitor> {
    fn visit(&mut self, visitor: &mut V);
}


// sample
#[derive(Debug)]
struct PrintVisitor;
impl Visitor for PrintVisitor {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> bool {
        return match ast_node {
            ASTNodeEnum::FnDef(x) => {
                println!("FnDef: {:?}", x);
                true
            }
            _ => true,
        };
    }
}
// sample end


impl<V: Visitor> Visitable<V> for FnCall {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::FnCall(self)) {
            for mut arg in &mut self.args {
                arg.visit(visitor);
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for LiteralValue {
    fn visit(&mut self, visitor: &mut V) {
        visitor.apply(&mut ASTNodeEnum::LiteralValue(self));
    }
}

impl<V: Visitor> Visitable<V> for AtomicExpression {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::AtomicExpression(self)) {
            match self {
                AtomicExpression::Literal(x) => { x.visit(visitor) }
                AtomicExpression::FnCall(x) => { x.visit(visitor) }
                AtomicExpression::Variable(_) => {}
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for Expression {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::Expression(self)) {
            match &mut self.expr {
                ExpressionEnum::AtomicExpression(x) => x.visit(visitor),
                ExpressionEnum::Unary(_, x) => x.visit(visitor),
                ExpressionEnum::Binary(x, _, y) => {
                    x.visit(visitor);
                    y.visit(visitor);
                }
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for Else {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::Else(self)) {
            match self {
                Else::If(x) => {
                    x.visit(visitor);
                }
                Else::Block(x) => {
                    x.visit(visitor);
                }
            }
        }
    }
}


impl<V: Visitor> Visitable<V> for If {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::If(self)) {
            self.cond.visit(visitor);
            self.body.visit(visitor);
            if let Some(else_) = &mut self.else_ {
                else_.visit(visitor);
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for While {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::While(self)) {
            self.cond.visit(visitor);
            self.body.visit(visitor);
        }
    }
}

impl<V: Visitor> Visitable<V> for For {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::For(self)) {
            if let Some(init) = &mut self.init {
                init.visit(visitor);
            }
            if let Some(cond) = &mut self.cond {
                cond.visit(visitor);
            }
            self.body.visit(visitor);
        }
    }
}

impl<V: Visitor> Visitable<V> for VarDef {
    fn visit(&mut self, visitor: &mut V) {
        visitor.apply(&mut ASTNodeEnum::VarDef(self));
    }
}

impl<V: Visitor> Visitable<V> for VarAssign {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::VarAssign(self)) {
            self.expr.visit(visitor);
        }
    }
}

impl<V: Visitor> Visitable<V> for VarDecl {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::VarDecl(self)) {
            self.var_def.visit(visitor);
            if let Some(expr) = &mut self.expr {
                expr.visit(visitor);
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for Statement {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::Statement(self)) {
            match self {
                Statement::VarDecl(x) => x.visit(visitor),
                Statement::VarAssign(x) => x.visit(visitor),
                Statement::If(x) => x.visit(visitor),
                Statement::While(x) => x.visit(visitor),
                Statement::For(x) => x.visit(visitor),
                Statement::Return(x) => x.visit(visitor),
                Statement::Expression(x) => x.visit(visitor),
                Statement::Block(x) => x.visit(visitor),
                Statement::Continue | Statement::Break => {}
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for Block {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::Block(self)) {
            for mut statement in &mut self.statements {
                statement.visit(visitor);
            }
        }
    }
}

impl<V: Visitor> Visitable<V> for FnDef {
    fn visit(&mut self, visitor: &mut V) {
        if visitor.apply(&mut ASTNodeEnum::FnDef(self)) {
            self.body.visit(visitor);
        }
    }
}