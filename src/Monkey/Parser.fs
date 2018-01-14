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

            let parseIdentifier currentToken =
                {Identifier.Token = currentToken; Value = currentToken.Literal} :> Expression

            let parseIntegerLiteral currentToken =
                let parsed, value = System.Int64.TryParse(currentToken.Literal)
                if parsed then
                    {IntegerLiteral.Token = currentToken; Value = value} :> Expression
                else
                    let errorMessage = sprintf "Could not parse %s as an integer" currentToken.Literal
                    raise (ParseError errorMessage)

            // todo: there has to be a more elegant way of doing this:
            //      we are mapping strings to funcs
            let prefixParseFunctionMap = dict [(IDENT, parseIdentifier); (INT, parseIntegerLiteral)]

            let parseExpression precedence currentToken=
                let prefixFunction = prefixParseFunctionMap.[currentToken.Type]
                prefixFunction currentToken

            match currentToken.Type with
            | LET -> 
                let parserErrors = List.append (expectPeek remainingTokens.Head IDENT) (expectPeek remainingTokens.[1] ASSIGN)

                if parserErrors.IsEmpty then
                    let identifierToken = remainingTokens.Head
                    let nextRemainingTokens = iterateUntil remainingTokens.[2..] SEMICOLON
                    let name = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    // todo: create an actual expression
                    let value = EmptyExpression()
                    // notice we are downcasting to statement here!!!
                    let letStatement = {Token = currentToken; Name = name; Value = value} :> Statement
                    // skip over the semicolon by just taking the tail
                    (letStatement, nextRemainingTokens.Tail, [])
                else
                    (EmptyStatement() :> Statement, remainingTokens, parserErrors)
            | RETURN ->
                let nextRemainingTokens = iterateUntil remainingTokens.[1..] SEMICOLON
                // todo: create an actual expression
                let value = EmptyExpression()
                let returnStatement = {Token = currentToken; ReturnValue = value} :> Statement
                (returnStatement, nextRemainingTokens.Tail, [])
            | _ ->
                // everything else is considered an expression statement
                let expression = parseExpression OperatorPrecedence.Lowest currentToken
                let nextRemainingTokens =
                    if remainingTokens.Head.Type = SEMICOLON then
                        remainingTokens.Tail
                    else
                        remainingTokens
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
