﻿namespace Monkey
open Token
open System

// this is the old, more Go-ish start at doing this, I abandoned it for Lexer2 where we just parse everything at once
// and avoid a lot of state tracking
module Lexer =

    exception UnknownCharacter of char

    let emptyChar = (char 0)

    // todo: there has to be a better way than copying the Input around for each lexer!
    type Lexer = { Input : string; Position: int; ReadPosition : int; CurrentChar : char }

    let readChar l =
        let incrementLexer newCurrentChar =
            {l with CurrentChar = newCurrentChar; Position = l.ReadPosition; ReadPosition = l.ReadPosition + 1}

        if l.ReadPosition >= l.Input.Length then
            incrementLexer emptyChar
        else
            incrementLexer l.Input.[l.ReadPosition]

    let createLexer s =
        let l = {Input = s; Position = 0; ReadPosition = 0; CurrentChar = emptyChar}
        readChar l 

    let nextToken l =
        let newToken literal tokenType =
            {Type = tokenType; Literal = literal}

        let isLetter ch =
            Char.IsLetter(ch)

        let isNumber ch =
            Char.IsNumber(ch)

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
                Char.IsWhiteSpace(ch)
            let rec skipWhitespaceRec lex =
                match isWhitespace lex.CurrentChar with
                | true -> 
                    let newLexer = readChar lex
                    skipWhitespaceRec newLexer
                | false -> lex
            skipWhitespaceRec l
            
        let readNumber l =
            let rec readNumberRec lex pos =
                match isNumber lex.CurrentChar with
                | true ->
                    let newLexer = readChar lex
                    readNumberRec newLexer (pos + 1)
                | false -> 
                    let numberValue = lex.Input.Substring(l.Position, lex.Position - l.Position)
                    (lex, numberValue)
            readNumberRec l l.Position

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
                else if isNumber wsLexer.CurrentChar then
                    let newLexer, numberValue = readNumber wsLexer
                    let token = newToken numberValue INT
                    (newLexer, token)
                else if wsLexer.CurrentChar = emptyChar then
                    (readChar wsLexer, partialToken EOF)
                else
                    (readChar wsLexer, partialToken ILLEGAL)