namespace Monkey
open TokenConsts
open Token

module Lexer =

    exception UnknownCharacter of char

    let emptyChar = (char 0)

    type Lexer = { Input : string; Position: int; ReadPosition : int; CurrentChar : char }

    let incrementLexer l newCurrentChar =
        {l with CurrentChar = newCurrentChar; Position = l.ReadPosition; ReadPosition = l.ReadPosition + 1 }

    let readChar l =
        if l.ReadPosition >= l.Input.Length then
            incrementLexer l emptyChar
        else
            incrementLexer l l.Input.[l.ReadPosition]

    let createLexer s =
        let l = {Input = s; Position = 0; ReadPosition = 0; CurrentChar = emptyChar}
        readChar l 
    
    let newToken (tokenChar:char) tokenType =
        {Type = tokenType; Literal = (tokenChar.ToString())}

    let nextToken l =
        let partialToken = newToken l.CurrentChar
        match l.CurrentChar with
            | '=' -> partialToken ASSIGN
            | ';' -> partialToken SEMICOLON
            | '(' -> partialToken LPAREN
            | ')' -> partialToken RPAREN
            | ',' -> partialToken COMMA
            | '+' -> partialToken PLUS
            | '{' -> partialToken LBRACE
            | '}' -> partialToken RBRACE
            | emptyChar -> partialToken EOF
            | _ -> raise (UnknownCharacter l.CurrentChar)
