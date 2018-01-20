namespace Monkey

open Monkey.Ast
open Monkey.Lexer2
open Monkey.Token


module Parser =

    exception ParseError of string

    type ParserOutput =
        | Statements of Statement List
        | Errors of string List

    type OperatorPrecedence = Lowest = 1 | Equals = 2 | LessGreater = 3 | Sum = 4 | Product = 5 | Prefix = 6 | Call = 7

    let parseProgram tokens =

        let precedences = dict[ EQ, OperatorPrecedence.Equals;
                                NOT_EQ, OperatorPrecedence.Equals;
                                LT, OperatorPrecedence.LessGreater;
                                GT, OperatorPrecedence.LessGreater;
                                PLUS, OperatorPrecedence.Sum;
                                MINUS, OperatorPrecedence.Sum;
                                SLASH, OperatorPrecedence.Product;
                                ASTERISK, OperatorPrecedence.Product;]

        let getPrecedence token =
            let found, p = precedences.TryGetValue token.Type
            if found then
                p
            else
                OperatorPrecedence.Lowest

        let parseStatement currentToken (remainingTokens: Token list) =

            let expectPeek token tokenType =
                if token.Type = tokenType then
                    []
                else
                    [sprintf "Expected next token to be %s, got %s instead" tokenType token.Type]

            let rec iterateUntil (iterableTokens: Token list) (tokenType:string) =
                if iterableTokens.Head.Type = tokenType then
                    iterableTokens                    
                else
                    iterateUntil iterableTokens.Tail tokenType

            let parseIdentifier currentToken remainingTokens _ =
                let identifier = {Identifier.Token = currentToken; Value = currentToken.Literal} :> Expression
                (identifier, remainingTokens)

            let parseIntegerLiteral currentToken remainingTokens _ =
                let parsed, value = System.Int64.TryParse(currentToken.Literal)
                if parsed then
                    let intLiteral = {IntegerLiteral.Token = currentToken; Value = value} :> Expression
                    (intLiteral, remainingTokens)
                else
                    let errorMessage = sprintf "Could not parse %s as an integer" currentToken.Literal
                    raise (ParseError errorMessage)

            let parsePrefixExpression currentToken remainingTokens parseNext =
                let (right, newRemaining) = parseNext OperatorPrecedence.Prefix (List.head remainingTokens) (List.tail remainingTokens)
                let prefixExpression = {PrefixExpression.Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                (prefixExpression, newRemaining)

            let parseInfixExpression left currentToken remainingTokens parseNext =
                let precedence = getPrecedence currentToken
                let (right, newRemaining) = parseNext precedence (List.head remainingTokens) (List.tail remainingTokens)
                let infixExpression = {InfixExpression.Left = left; Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                (infixExpression, newRemaining)

            // todo: there has to be a more elegant way of doing this:
            //      we are mapping strings to funcs
            let prefixParseFunctionMap = dict [ IDENT, parseIdentifier;
                                                INT, parseIntegerLiteral;
                                                BANG, parsePrefixExpression;
                                                MINUS, parsePrefixExpression;]

            let infixParseFunctionMap = dict [  PLUS, parseInfixExpression;
                                                MINUS, parseInfixExpression;
                                                SLASH, parseInfixExpression;
                                                ASTERISK, parseInfixExpression;
                                                EQ, parseInfixExpression;
                                                NOT_EQ, parseInfixExpression;
                                                LT, parseInfixExpression;
                                                GT, parseInfixExpression;]

            match currentToken.Type with
            | LET -> 
                let parserErrors = List.append (expectPeek remainingTokens.Head IDENT) (expectPeek remainingTokens.[1] ASSIGN)

                match parserErrors with
                | [] ->
                    let identifierToken = remainingTokens.Head
                    let nextRemainingTokens = iterateUntil remainingTokens.[2..] SEMICOLON
                    let name = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    // todo: create an actual expression
                    let value = EmptyExpression()
                    // notice we are downcasting to statement here!!!
                    let letStatement = {Token = currentToken; Name = name; Value = value} :> Statement
                    // skip over the semicolon by just taking the tail
                    (letStatement, nextRemainingTokens.Tail, [])
                | errors ->
                    (EmptyStatement() :> Statement, remainingTokens, errors)

            | RETURN ->
                let nextRemainingTokens = iterateUntil remainingTokens.[1..] SEMICOLON
                // todo: create an actual expression
                let value = EmptyExpression()
                let returnStatement = {Token = currentToken; ReturnValue = value} :> Statement
                (returnStatement, nextRemainingTokens.Tail, [])
            | _ ->
                // everything else is considered an expression statement

                let rec parseExpression precedence currentToken remainingTokens =
                    let reachedEndCondition token =
                        token.Type = SEMICOLON || token |> getPrecedence >= precedence

                    let found, prefixFunction = prefixParseFunctionMap.TryGetValue currentToken.Type
                    if found then
                        let seedLeft, seedRemaining = prefixFunction currentToken remainingTokens parseExpression
                        if (List.isEmpty seedRemaining) || seedRemaining.Head |> reachedEndCondition then
                            seedLeft, seedRemaining
                        else
                            let rec applyInfix token remainingTokens' currentLeft =
                                match remainingTokens' with
                                | [] -> (currentLeft, [])
                                | x::xs when x |> reachedEndCondition -> (currentLeft, remainingTokens')
                                | x::xs ->
                                    let found, infixFunction = infixParseFunctionMap.TryGetValue x.Type
                                    if found then
                                        let newCurrentInfix, newCurrentRemaining = infixFunction currentLeft x xs parseExpression
                                        applyInfix (List.head newCurrentRemaining) (List.tail newCurrentRemaining) newCurrentInfix
                                    else
                                        (currentLeft, remainingTokens')

                            applyInfix seedRemaining.Head seedRemaining.Tail seedLeft
                    else
                        raise (ParseError (sprintf "Unable to find prefix parser function for token type %s" currentToken.Type))


                let (expression, updatedRemainingTokens) = parseExpression OperatorPrecedence.Lowest currentToken remainingTokens
                let nextRemainingTokens =
                    match updatedRemainingTokens with
                    | [] ->
                        []
                    | x::xs when x.Type = SEMICOLON -> 
                        (List.tail updatedRemainingTokens)
                    | (tokens) ->
                        tokens
                let expressionStatement = {ExpressionStatement.Token = currentToken; Expression = expression} :> Statement
                (expressionStatement, nextRemainingTokens, [])


        let rec parseStatements remainingTokens statements errors =
            let (|EndOfTokens|_|) (t: Token list) = 
                if List.isEmpty t || t.Head.Type = EOF then 
                    Some () 
                else 
                    None

            match remainingTokens with
            | EndOfTokens -> 
                if List.isEmpty errors then
                    Statements(List.rev statements)
                else
                    Errors(errors)
            | _ ->
                let newStatement, nextRemainingTokens, statementErrors = parseStatement remainingTokens.Head remainingTokens.Tail
                parseStatements nextRemainingTokens (newStatement::statements) (errors@statementErrors)

        parseStatements tokens [] []
