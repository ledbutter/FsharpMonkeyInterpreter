namespace Monkey
open Token
open System

module Lexer =

    type LexerPosition = { Pos : int; ReadPos : int; CurrentChar : char }

    let tokenizeInput (input:string) =
        let emptyChar = (char 0)

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

        let peekChar lexerPos =
            if lexerPos.ReadPos >= input.Length then
                emptyChar
            else
                input.[lexerPos.ReadPos]

        let rec nextTokenRec lexerPos tokens =
            if lexerPos.CurrentChar = emptyChar then
                List.rev tokens
            else 
                let wsPos = skipWhitespace lexerPos
                let partialToken = newToken (wsPos.CurrentChar.ToString())
                let oneCharPosIncrement = incrementPosition wsPos
                let rec multipleCharPosIncrement number currentPos =
                    if number > 0 then
                        multipleCharPosIncrement (number - 1) (incrementPosition currentPos)
                    else 
                        currentPos

                let newLexerPos, token =
                    match wsPos.CurrentChar with
                    | '=' -> 
                        if peekChar wsPos = '=' then
                            (multipleCharPosIncrement 2 wsPos, newToken "==" EQ)
                        else
                            (oneCharPosIncrement, partialToken ASSIGN)
                    | ';' -> (oneCharPosIncrement, partialToken SEMICOLON)
                    | '(' -> (oneCharPosIncrement, partialToken LPAREN)
                    | ')' -> (oneCharPosIncrement, partialToken RPAREN)
                    | ',' -> (oneCharPosIncrement, partialToken COMMA)
                    | '+' -> (oneCharPosIncrement, partialToken PLUS)
                    | '{' -> (oneCharPosIncrement, partialToken LBRACE)
                    | '}' -> (oneCharPosIncrement, partialToken RBRACE)
                    | '-' -> (oneCharPosIncrement, partialToken MINUS)
                    | '!' -> 
                        if peekChar wsPos = '=' then
                            (multipleCharPosIncrement 2 wsPos, newToken "!=" NOT_EQ)
                        else
                            (oneCharPosIncrement, partialToken BANG)
                    | '/' -> (oneCharPosIncrement, partialToken SLASH)
                    | '*' -> (oneCharPosIncrement, partialToken ASTERISK)
                    | '>' -> (oneCharPosIncrement, partialToken GT)
                    | '<' -> (oneCharPosIncrement, partialToken LT)
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
                            (oneCharPosIncrement, partialToken EOF)
                        else
                            (oneCharPosIncrement, partialToken ILLEGAL)
                nextTokenRec newLexerPos (token::tokens)
                
        let initialLexerPosition = { Pos = 0; ReadPos = 1; CurrentChar = input.[0] }
        nextTokenRec initialLexerPosition []


