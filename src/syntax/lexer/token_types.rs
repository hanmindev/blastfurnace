pub enum Token {
    Any,

    Ident(String),
    Bool(bool),
    Int(i32),
    Decimal(f64),
    String(String), // anything between double quotes

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exclamation,
    Ampersand,

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

    // Misc
    EOF,
    Invalid(String),
}