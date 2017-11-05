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
            | false -> (lex, input.[position..lex.Position])
        readIdentiferUntilNotChar l

    let nextToken l =
        let newToken literal tokenType =
            {Type = tokenType; Literal = literal}

        let partialToken = newToken (l.CurrentChar.ToString())
        match l.CurrentChar with
            | '=' -> (readChar l, partialToken ASSIGN)
            | ';' -> (readChar l, partialToken SEMICOLON)
            | '(' -> (readChar l, partialToken LPAREN)
            | ')' -> (readChar l, partialToken RPAREN)
            | ',' -> (readChar l, partialToken COMMA)
            | '+' -> (readChar l, partialToken PLUS)
            | '{' -> (readChar l, partialToken LBRACE)
            | '}' -> (readChar l, partialToken RBRACE)
            | emptyChar -> (readChar l, partialToken EOF)
            | _ -> 
                if isLetter l.CurrentChar then
                    let newLexer, identifier = readIdentifier l
                    let token = newToken identifier ASSIGN
                    (newLexer, token)
                else
                    (readChar l, partialToken ILLEGAL)
