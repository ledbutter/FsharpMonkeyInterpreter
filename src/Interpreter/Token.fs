namespace Monkey

module Token =

    type TokenType = string

    type Token = { Type : TokenType; Literal : string }

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
    [<Literal>]
    let MINUS = "-"
    [<Literal>]
    let BANG = "!"
    [<Literal>]
    let ASTERISK = "*"
    [<Literal>]
    let SLASH = "/"

    [<Literal>]
    let LT = "<"
    [<Literal>]
    let GT = ">"

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

    let private keywords = dict [ ("fn", FUNCTION); ("let", LET) ]

    let lookupIdent ident =
        let found, value = keywords.TryGetValue ident
        if found then
            value
        else
            IDENT