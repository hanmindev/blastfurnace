use crate::front::ast_retriever::reader::lexical::lexer::Lexer;
use crate::front::ast_retriever::reader::syntax::parser::{ParseResult, Parser};
use crate::front::file_system::byte_stream::{ByteStream, StringReader};
use crate::front::internal_ast_types::Module;

mod name_resolution;
mod reader;
pub mod retriever;

pub fn string_to_module(statement: &str) -> ParseResult<Module> {
    let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
        statement.to_string(),
    ))));
    let mut parser = Parser::new(lexer);

    return parser.parse_module();
}
