namespace Monkey

module Token =

    type TokenType = string

    type Token = { Type : TokenType; Literal : string }

module TokenConsts =

    [<Literal>]
    let ILLEGAL = "ILLEGAL"
    [<Literal>]
    let EOF = "EOF"
    
    // Identifiers + literals
    [<Literal>]
    let IDENT = "IDENT"
    [<Literal>]
    let INT = "INT"

    // Operators
    [<Literal>]
    let ASSIGN = "="
    [<Literal>]
    let PLUS = "+"

    // Delimiters
    [<Literal>]
    let COMMA = ","
    [<Literal>]
    let SEMICOLON = ";"

    [<Literal>]
    let LPAREN = "("
    [<Literal>]
    let RPAREN = ")"
    [<Literal>]
    let LBRACE = "{"
    [<Literal>]
    let RBRACE = "}"

    // Keywords
    [<Literal>]
    let FUNCTION = "FUNCTION"
    [<Literal>]
    let LET = "LET"