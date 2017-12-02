namespace Monkey
open Token
open System
open Lexer

module Lexer2 =

    type LexerPosition = { Pos : int; ReadPos : int; CurrentChar : char }

    let tokenizeInput (input:string) =
        let newToken literal tokenType =
            {Type = tokenType; Literal = literal}

        let isLetter ch =
            Char.IsLetter(ch)

        let isNumber ch =
            Char.IsNumber(ch)

        let incrementPosition lexerPos =
            let incrementPartial newCurrentChar =
                {lexerPos with CurrentChar = newCurrentChar; Pos = lexerPos.ReadPos; ReadPos = lexerPos.ReadPos + 1}

            if lexerPos.ReadPos >= input.Length then
                incrementPartial emptyChar
            else
                incrementPartial input.[lexerPos.ReadPos]

        let skipWhitespace lexerPos =
            let isWhitespace ch =
                Char.IsWhiteSpace(ch)
            let rec skipWhitespaceRec currentLexerPos =
                match isWhitespace currentLexerPos.CurrentChar with
                | true ->
                    let newPos = incrementPosition currentLexerPos
                    skipWhitespaceRec newPos
                | false ->
                    currentLexerPos
            skipWhitespaceRec lexerPos

        let readIdentifier lexerPos =
            let position = lexerPos.Pos
            let rec readIdentiferUntilNotChar currentLexPos =
                match isLetter currentLexPos.CurrentChar with
                | true -> 
                    let newPos = incrementPosition currentLexPos
                    readIdentiferUntilNotChar newPos
                | false -> (currentLexPos, input.[position..currentLexPos.Pos-1])
            readIdentiferUntilNotChar lexerPos

        let readNumber lexerPos =
            let rec readNumberRec currentLexPos =
                match isNumber currentLexPos.CurrentChar with
                | true ->
                    let newPos = incrementPosition currentLexPos
                    readNumberRec newPos
                | false ->
                    let numberValue = input.[lexerPos.Pos..currentLexPos.Pos-1]
                    (currentLexPos, numberValue)
            readNumberRec lexerPos

        let rec nextTokenRec lexerPos tokens =
            if lexerPos.CurrentChar = emptyChar then
                List.rev tokens
            else 
                let wsPos = skipWhitespace lexerPos
                let partialToken = newToken (wsPos.CurrentChar.ToString())
                let oneCharacterPositionIncrement = incrementPosition wsPos

                let newLexerPos, token =
                    match wsPos.CurrentChar with
                    | '=' -> (oneCharacterPositionIncrement, partialToken ASSIGN) 
                    | ';' -> (oneCharacterPositionIncrement, partialToken SEMICOLON)
                    | '(' -> (oneCharacterPositionIncrement, partialToken LPAREN)
                    | ')' -> (oneCharacterPositionIncrement, partialToken RPAREN)
                    | ',' -> (oneCharacterPositionIncrement, partialToken COMMA)
                    | '+' -> (oneCharacterPositionIncrement, partialToken PLUS)
                    | '{' -> (oneCharacterPositionIncrement, partialToken LBRACE)
                    | '}' -> (oneCharacterPositionIncrement, partialToken RBRACE)
                    | '-' -> (oneCharacterPositionIncrement, partialToken MINUS)
                    | '!' -> (oneCharacterPositionIncrement, partialToken BANG)
                    | '/' -> (oneCharacterPositionIncrement, partialToken SLASH)
                    | '*' -> (oneCharacterPositionIncrement, partialToken ASTERISK)
                    | '>' -> (oneCharacterPositionIncrement, partialToken GT)
                    | '<' -> (oneCharacterPositionIncrement, partialToken LT)
                    | _ -> 
                        if isLetter wsPos.CurrentChar then
                            let newPos, identifier = readIdentifier wsPos
                            let tokenType = lookupIdent identifier
                            let identiferToken = newToken identifier tokenType
                            (newPos, identiferToken)
                        else if isNumber wsPos.CurrentChar then
                            let newPos, numberValue = readNumber wsPos
                            let numberToken = newToken numberValue INT
                            (newPos, numberToken)
                        else if wsPos.CurrentChar = emptyChar then
                            (oneCharacterPositionIncrement, partialToken EOF)
                        else
                            (oneCharacterPositionIncrement, partialToken ILLEGAL)
                nextTokenRec newLexerPos (token::tokens)
                
        let initialLexerPosition = { Pos = 0; ReadPos = 1; CurrentChar = input.[0] }
        nextTokenRec initialLexerPosition []


