#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Any,

    Null,
    Ident(String),
    Bool(bool),
    Int(i32),
    Float(f32),
    Double(f64),
    String(String), // anything between double quotes

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exclamation,
    Ampersand,
    And,
    Or,
    PlusPlus,
    MinusMinus,

    // Comparison
    Equal,
    NotEqual,
    // less and greater are below in LAngle and RAngle
    Leq,
    Geq,

    // Assignment
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,

    // Other Symbols
    Comma,
    Semicolon,
    Colon,
    Dot,
    LParen, // ()
    RParen,
    LBrace, // {}
    RBrace,
    LBracket, // []
    RBracket,
    LAngle, // <>
    RAngle,

    Arrow, // ->

    // Keywords
    Const,

    VoidType,
    IntType,
    FloatType,
    DoubleType,
    BoolType,
    StringType,
    StructType,

    Impl,

    Let,

    Fn,
    Rec,
    Inline,

    If,
    Else,
    While,
    For,

    Return,
    Break,
    Continue,

    Use,
    As,
    Mod,
    Pub,

    // Misc
    Eof,
}
