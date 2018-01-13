namespace Monkey

open Monkey.Token

module Ast =
    
    // todo: initial work based on source Go and porting as directly as possible to F#
    //  i imagine there's a more elegant way of doing this in F#
    // could it be done using a DU???

    type Node =
        abstract member TokenLiteral: unit -> string

    type Statement =
        inherit Node

    type Expression =
        inherit Node

    type Program =
        {
            Statements: Statement list
        }
        interface Statement with
            member this.TokenLiteral() = 
                if this.Statements.Length > 0 then
                    this.Statements.[0].TokenLiteral()
                else
                    ""
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()

    type Identifier = 
        {
            Token: Token; // the IDENT token
            Value: string
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()

    type LetStatement = 
        {   
            Token: Token // the LET token
            Name: Identifier 
            Value: Expression
        }
        interface Statement with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()

    type ReturnStatement =
        {
            Token: Token // the return token
            ReturnValue: Expression
        }
        interface Statement with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()

    type EmptyExpression =
        interface Expression with
            member this.TokenLiteral() =
                ""
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        new() = {}

    type EmptyStatement = 
        interface Statement with
            member this.TokenLiteral() =
                ""
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        new() = {}