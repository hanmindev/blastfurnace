use crate::front::file_system::byte_stream::{ByteStream, StringReader};
use crate::front::ast_retriever::lexical::lexer::Lexer;
use crate::front::internal_ast_types::Module;
use crate::front::ast_retriever::syntax::parser::{Parser, ParseResult};

pub mod retriever;
mod lexical;
mod syntax;

pub fn string_to_module(statement: &str) -> ParseResult<Module> {
    let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
        statement.to_string(),
    ))));
    let mut parser = Parser::new(lexer);

    return parser.parse_module();
}
