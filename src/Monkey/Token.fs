namespace Monkey

module Token =

    type TokenType = string

    [<StructuredFormatDisplay("Token: {Type} {Literal}")>]
    type Token = 
        { Type : TokenType; Literal : string }

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

    [<Literal>]
    let EQ = "=="
    [<Literal>]
    let NOT_EQ = "!="

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
    [<Literal>]
    let TRUE = "TRUE"
    [<Literal>]
    let FALSE = "FALSE"
    [<Literal>]
    let IF = "IF"
    [<Literal>]
    let ELSE = "ELSE"
    [<Literal>]
    let RETURN = "RETURN"

    let private keywords = dict [ ("fn", FUNCTION); ("let", LET); ("true", TRUE); ("false", FALSE); ("if", IF); ("else", ELSE); ("return", RETURN) ]

    let lookupIdent ident =
        let found, value = keywords.TryGetValue ident
        if found then
            value
        else
            IDENT