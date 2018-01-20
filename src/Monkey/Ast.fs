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
        override x.ToString() =
            x.Statements
            |> Seq.map(fun s -> s.ToString())
            |> Seq.fold(fun (sb:System.Text.StringBuilder) s ->
                sb.Append(s)) (new System.Text.StringBuilder())
            |> fun x -> x.ToString()

    type Identifier = 
        {
            Token: Token; // the IDENT token
            Value: string
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            x.Value

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
        override x.ToString() =
            let sb = new System.Text.StringBuilder()
            sprintf "%s %s = %s;" (x.TokenLiteral()) (x.Name.ToString()) (x.Value.ToString())

    type ReturnStatement =
        {
            Token: Token // the return token
            ReturnValue: Expression
        }
        interface Statement with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        override x.ToString() =
            sprintf "%s %s;" (x.TokenLiteral()) (x.ReturnValue.ToString())

    type ExpressionStatement =
        {
            Token: Token // the first token of the expression
            Expression: Expression
        }
        interface Statement with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        override x.ToString() =
            x.Expression.ToString()

    type IntegerLiteral = 
        {
            Token: Token
            Value: int64
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            x.Token.Literal

    type PrefixExpression =
        {
            Token: Token // the prefix token, e.g. !
            Operator: string
            Right: Expression
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            sprintf "(%s %s)" x.Operator (x.Right.ToString())

    type InfixExpression =
        {
            Token: Token // The operator token, e.g. +
            Left: Expression
            Operator: string
            Right: Expression
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            sprintf "(%s %s %s)" (x.Left.ToString()) x.Operator (x.Right.ToString())

    // dummy types

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