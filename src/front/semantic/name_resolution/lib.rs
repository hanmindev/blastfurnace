#[cfg(test)]
mod tests {
    use crate::front::lexical::lexer::Lexer;
    use crate::front::lexical::lexer_string_reader::StringReader;
    use crate::front::semantic::name_resolution::resolver::Resolvable;
    use crate::front::semantic::name_resolution::scope_table::{ScopeTable, SymbolInfo};
    use crate::front::syntax::parser::Parser;

    #[test]
    fn simple_scope() {
        let mut scope_table = ScopeTable::new();

        let statement = "int a; fn main(int a) { int a; a + 1; return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse().unwrap();

        block.resolve(&mut scope_table).unwrap();

        scope_table.get_global().iter().for_each(|(k, v)| {
            println!("{}: {:?}", k, v);
        });
    }
}
