namespace Monkey
open Token

module Lexer =

    exception UnknownCharacter of char

    let emptyChar = (char 0)

    type Lexer = { Input : string; Position: int; ReadPosition : int; CurrentChar : char }

    let readChar l =
        let incrementLexer l newCurrentChar =
            {l with CurrentChar = newCurrentChar; Position = l.ReadPosition; ReadPosition = l.ReadPosition + 1 }

        if l.ReadPosition >= l.Input.Length then
            incrementLexer l emptyChar
        else
            incrementLexer l l.Input.[l.ReadPosition]

    let createLexer s =
        let l = {Input = s; Position = 0; ReadPosition = 0; CurrentChar = emptyChar}
        readChar l 

    let isLetter ch =
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch = '_'

    let readIdentifier l =
        let position = l.Position
        let input = l.Input
        let rec readIdentiferUntilNotChar lex =
            match isLetter lex.CurrentChar with
            | true -> 
                let newLexer = readChar lex
                readIdentiferUntilNotChar newLexer
            | false -> (lex, input.[position..lex.Position-1])
        readIdentiferUntilNotChar l

    let skipWhitespace l =
        let isWhitespace ch =
            ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'
        let rec skipWhitespaceRec lex =
            match isWhitespace lex.CurrentChar with
            | true -> 
                let newLexer = readChar lex
                skipWhitespaceRec newLexer
            | false -> lex
        skipWhitespaceRec l

    let nextToken l =
        let newToken literal tokenType =
            {Type = tokenType; Literal = literal}

        let wsLexer = skipWhitespace l

        let partialToken = newToken (wsLexer.CurrentChar.ToString())
        match wsLexer.CurrentChar with
            | '=' -> (readChar wsLexer, partialToken ASSIGN)
            | ';' -> (readChar wsLexer, partialToken SEMICOLON)
            | '(' -> (readChar wsLexer, partialToken LPAREN)
            | ')' -> (readChar wsLexer, partialToken RPAREN)
            | ',' -> (readChar wsLexer, partialToken COMMA)
            | '+' -> (readChar wsLexer, partialToken PLUS)
            | '{' -> (readChar wsLexer, partialToken LBRACE)
            | '}' -> (readChar wsLexer, partialToken RBRACE)
            | _ -> 
                if isLetter wsLexer.CurrentChar then
                    let newLexer, identifier = readIdentifier wsLexer
                    let tokenType = lookupIdent identifier
                    let token = newToken identifier tokenType
                    (newLexer, token)
                else if wsLexer.CurrentChar = emptyChar then
                    (readChar wsLexer, partialToken EOF)
                else
                    (readChar wsLexer, partialToken ILLEGAL)
