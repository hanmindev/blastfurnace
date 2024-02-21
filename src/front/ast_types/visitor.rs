use crate::front::ast_types::{
    AtomicExpression, Block, Definition, Else, Expression, ExpressionEnum, FnCall, FnDef, For, If,
    LiteralValue, Module, NamePath, Reference, Statement, StructDef, Type, Use, VarAssign, VarDecl,
    VarDef, While,
};

pub enum ASTNodeEnum<'a> {
    NamePath(&'a mut NamePath),
    Reference(&'a mut Reference),

    VarDecl(&'a mut VarDecl),
    VarAssign(&'a mut VarAssign),
    VarDef(&'a mut VarDef),

    FnDef(&'a mut FnDef),
    FnCall(&'a mut FnCall),

    StructDef(&'a mut StructDef),

    LiteralValue(&'a mut LiteralValue),
    AtomicExpression(&'a mut AtomicExpression),
    Expression(&'a mut Expression),

    If(&'a mut If),
    Else(&'a mut Else),
    While(&'a mut While),
    For(&'a mut For),

    Statement(&'a mut Statement),
    Definition(&'a mut Definition),
    Block(&'a mut Block),
    Module(&'a mut Module),

    Use(&'a mut Use),
}

pub trait Visitor<V> {
    fn apply(&mut self, _ast_node: &mut ASTNodeEnum) -> Result<bool, V> {
        return Ok(true);
    }
}

pub trait Visitable<T: Visitor<V>, V> {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V>;
}

impl<T: Visitor<V>, V> Visitable<T, V> for FnCall {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::FnCall(self))? {
            self.name.visit(visitor)?;
            for mut arg in &mut self.args {
                arg.visit(visitor)?;
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for LiteralValue {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        visitor.apply(&mut ASTNodeEnum::LiteralValue(self))?;
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Reference {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        visitor.apply(&mut ASTNodeEnum::Reference(self))?;
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for NamePath {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::NamePath(self))? {
            self.name.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for AtomicExpression {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::AtomicExpression(self))? {
            match self {
                AtomicExpression::Literal(x) => x.visit(visitor)?,
                AtomicExpression::FnCall(x) => x.visit(visitor)?,
                AtomicExpression::Variable(x) => x.visit(visitor)?,
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Expression {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Expression(self))? {
            match &mut self.expr {
                ExpressionEnum::AtomicExpression(x) => x.visit(visitor)?,
                ExpressionEnum::Unary(_, x) => x.visit(visitor)?,
                ExpressionEnum::Binary(x, _, y) => {
                    x.visit(visitor)?;
                    y.visit(visitor)?;
                }
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Else {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Else(self))? {
            match self {
                Else::If(x) => {
                    x.visit(visitor)?;
                }
                Else::Block(x) => {
                    x.visit(visitor)?;
                }
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for If {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::If(self))? {
            self.cond.visit(visitor)?;
            self.body.visit(visitor)?;
            if let Some(else_) = &mut self.else_ {
                else_.visit(visitor)?;
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for While {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::While(self))? {
            self.cond.visit(visitor)?;
            self.body.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for For {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::For(self))? {
            if let Some(init) = &mut self.init {
                init.visit(visitor)?;
            }
            if let Some(cond) = &mut self.cond {
                cond.visit(visitor)?;
            }
            if let Some(step) = &mut self.step {
                step.visit(visitor)?;
            }
            self.body.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for VarDef {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::VarDef(self))? {
            if let Some(Type::Struct(struct_name)) = &mut self.type_ {
                struct_name.visit(visitor)?;
            }
            self.name.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for VarAssign {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::VarAssign(self))? {
            self.expr.visit(visitor)?;
            self.name_path.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for VarDecl {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::VarDecl(self))? {
            if let Some(expr) = &mut self.expr {
                expr.visit(visitor)?;
            }
            self.var_def.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Statement {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Statement(self))? {
            match self {
                Statement::VarDecl(x) => x.visit(visitor)?,
                Statement::VarAssign(x) => x.visit(visitor)?,
                Statement::If(x) => x.visit(visitor)?,
                Statement::While(x) => x.visit(visitor)?,
                Statement::For(x) => x.visit(visitor)?,
                Statement::Return(x) => x.visit(visitor)?,
                Statement::Expression(x) => x.visit(visitor)?,
                Statement::Block(x) => x.visit(visitor)?,
                Statement::Continue | Statement::Break => {}
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Block {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Block(self))? {
            for definitions in &mut self.definitions {
                definitions.visit(visitor)?;
            }

            for mut statement in &mut self.statements {
                statement.visit(visitor)?;
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for FnDef {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::FnDef(self))? {
            self.body.visit(visitor)?;
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for StructDef {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        visitor.apply(&mut ASTNodeEnum::StructDef(self))?;
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Definition {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Definition(self))? {
            match self {
                Definition::VarDecl(x) => x.visit(visitor)?,
                Definition::StructDef(x) => x.visit(visitor)?,
                Definition::FnDef(x) => x.visit(visitor)?,
            }
        }
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Use {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        visitor.apply(&mut ASTNodeEnum::Use(self))?;
        Ok(())
    }
}

impl<T: Visitor<V>, V> Visitable<T, V> for Module {
    fn visit(&mut self, visitor: &mut T) -> Result<(), V> {
        if visitor.apply(&mut ASTNodeEnum::Module(self))? {
            for use_ in &mut self.uses {
                use_.visit(visitor)?;
            }

            for definitions in &mut self.public_definitions {
                definitions.visit(visitor)?;
            }
            self.block.visit(visitor)?;
        }
        Ok(())
    }
}
