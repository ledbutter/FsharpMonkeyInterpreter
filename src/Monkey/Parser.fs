namespace Monkey

open Monkey.Ast
open Monkey.Lexer2
open Monkey.Token


module Parser =

    exception ParseError of string

    let parseProgram tokens =

        let parseToken currentToken (remainingTokens: Token list) =

            let rec iterateUntil (iterableTokens: Token list) (tokenType:string) =
                if iterableTokens.Head.Type = tokenType then
                    iterableTokens                    
                else
                    iterateUntil iterableTokens.Tail tokenType

            match currentToken.Type with
            | LET -> 
                if remainingTokens.Head.Type <> IDENT then
                    raise (ParseError("Looking for IDENT in let statement"))
                else if remainingTokens.[1].Type <> ASSIGN then
                    raise (ParseError("Looking for ASSIGN in let statement"))
                else
                    let identifierToken = remainingTokens.Head
                    let nextRemainingTokens = iterateUntil remainingTokens.[2..] SEMICOLON
                    let name = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    // todo: create an actual expression
                    let value = {EmptyExpression.Value = ""}
                    // notice we are downcasting to statement here!!!
                    let letStatement = {Token = currentToken; Name = name; Value = value} :> Statement
                    // skip over the semicolon by just taking the tail
                    (letStatement, nextRemainingTokens.Tail)
            | _ -> raise (ParseError("Unexpected token type"))

        let rec parseTokens remainingTokens statements =
            let (|EndOfTokens|_|) (t: Token list) = 
                if List.isEmpty t || t.Head.Type = EOF then 
                    Some () 
                else None

            match remainingTokens with
            | EndOfTokens -> List.rev statements
            | _ ->
                let newStatement, nextRemainingTokens = parseToken remainingTokens.Head remainingTokens.Tail
                parseTokens nextRemainingTokens (newStatement::statements)

        parseTokens tokens []
