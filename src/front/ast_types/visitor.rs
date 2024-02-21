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

pub type GenericResolveResult<K, V> = Result<(bool, Option<K>), V>;

pub trait Visitor<V, K> {
    fn apply(&mut self, _ast_node: &mut ASTNodeEnum) -> GenericResolveResult<K, V> {
        return Ok((true, None));
    }
}

pub trait Visitable<T: Visitor<V, K>, V, K> {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V>;
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for FnCall {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::FnCall(self))?;
        if visit_result {
            self.name.visit(visitor)?;
            for arg in &mut self.args {
                arg.visit(visitor)?;
            }
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for LiteralValue {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        Ok(visitor.apply(&mut ASTNodeEnum::LiteralValue(self))?.1)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Reference {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        Ok(visitor.apply(&mut ASTNodeEnum::Reference(self))?.1)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for NamePath {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::NamePath(self))?;
        if visit_result {
            self.name.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for AtomicExpression {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::AtomicExpression(self))?;
        if visit_result {
            match self {
                AtomicExpression::Literal(x) => x.visit(visitor)?,
                AtomicExpression::FnCall(x) => x.visit(visitor)?,
                AtomicExpression::Variable(x) => x.visit(visitor)?,
            };
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Expression {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Expression(self))?;
        if visit_result {
            match &mut self.expr {
                ExpressionEnum::AtomicExpression(x) => {
                    x.visit(visitor)?;
                }
                ExpressionEnum::Unary(_, x) => {
                    x.visit(visitor)?;
                }
                ExpressionEnum::Binary(x, _, y) => {
                    x.visit(visitor)?;
                    y.visit(visitor)?;
                }
            };
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Else {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Else(self))?;
        if visit_result {
            match self {
                Else::If(x) => {
                    x.visit(visitor)?;
                }
                Else::Block(x) => {
                    x.visit(visitor)?;
                }
            }
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for If {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::If(self))?;
        if visit_result {
            self.cond.visit(visitor)?;
            self.body.visit(visitor)?;
            if let Some(else_) = &mut self.else_ {
                else_.visit(visitor)?;
            }
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for While {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::While(self))?;
        if visit_result {
            self.cond.visit(visitor)?;
            self.body.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for For {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::For(self))?;
        if visit_result {
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
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for VarDef {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::VarDef(self))?;
        if visit_result {
            if let Some(Type::Struct(struct_name)) = &mut self.type_ {
                struct_name.visit(visitor)?;
            }
            self.name.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for VarAssign {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::VarAssign(self))?;
        if visit_result {
            self.expr.visit(visitor)?;
            self.name_path.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for VarDecl {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::VarDecl(self))?;
        if visit_result {
            if let Some(expr) = &mut self.expr {
                expr.visit(visitor)?;
            }
            self.var_def.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Statement {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Statement(self))?;
        if visit_result {
            match self {
                Statement::VarDecl(x) => {
                    x.visit(visitor)?;
                }
                Statement::VarAssign(x) => {
                    x.visit(visitor)?;
                }
                Statement::If(x) => {
                    x.visit(visitor)?;
                }
                Statement::While(x) => {
                    x.visit(visitor)?;
                }
                Statement::For(x) => {
                    x.visit(visitor)?;
                }
                Statement::Return(x) => {
                    x.visit(visitor)?;
                }
                Statement::Expression(x) => {
                    x.visit(visitor)?;
                }
                Statement::Block(x) => {
                    x.visit(visitor)?;
                }
                Statement::Continue | Statement::Break => {}
            };
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Block {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Block(self))?;
        if visit_result {
            for definitions in &mut self.definitions {
                definitions.visit(visitor)?;
            }

            for statement in &mut self.statements {
                statement.visit(visitor)?;
            }
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for FnDef {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::FnDef(self))?;
        if visit_result {
            self.body.visit(visitor)?;
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for StructDef {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        Ok(visitor.apply(&mut ASTNodeEnum::StructDef(self))?.1)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Definition {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Definition(self))?;
        if visit_result {
            match self {
                Definition::VarDecl(x) => x.visit(visitor)?,
                Definition::StructDef(x) => x.visit(visitor)?,
                Definition::FnDef(x) => x.visit(visitor)?,
            };
        }
        Ok(res)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Use {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        Ok(visitor.apply(&mut ASTNodeEnum::Use(self))?.1)
    }
}

impl<T: Visitor<V, K>, V, K> Visitable<T, V, K> for Module {
    fn visit(&mut self, visitor: &mut T) -> Result<Option<K>, V> {
        let (visit_result, res) = visitor.apply(&mut ASTNodeEnum::Module(self))?;
        if visit_result {
            for use_ in &mut self.uses {
                use_.visit(visitor)?;
            }

            for definitions in &mut self.public_definitions {
                definitions.visit(visitor)?;
            }
            self.block.visit(visitor)?;
        }
        Ok(res)
    }
}
