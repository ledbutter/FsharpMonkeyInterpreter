namespace Monkey

open Monkey.Ast
open Monkey.Lexer2
open Monkey.Token


module Parser =

    exception ParseError of string

    type ParserOutput = {Statements: Statement List; Errors: string List}

    let parseProgram tokens =

        let parseToken currentToken (remainingTokens: Token list) =

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
            | _ -> raise (ParseError("Unexpected token type"))

        let rec parseTokens remainingTokens statements errors =
            let (|EndOfTokens|_|) (t: Token list) = 
                if List.isEmpty t || t.Head.Type = EOF then 
                    Some () 
                else None

            match remainingTokens with
            | EndOfTokens -> List.rev statements
            | _ ->
                let newStatement, nextRemainingTokens, statementErrors = parseToken remainingTokens.Head remainingTokens.Tail
                parseTokens nextRemainingTokens (newStatement::statements) (statementErrors::errors)

        parseTokens tokens [] []
