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
            sprintf "(%s%s)" x.Operator (x.Right.ToString())

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

    type Boolean =
        {
            Token: Token
            Value: bool
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            sprintf "%s" (x.TokenLiteral())

    type BlockStatement =
        {
            Token: Token
            Statements: Statement list
        }
        interface Statement with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        override x.ToString() =
            x.Statements
            |> Seq.map(fun s -> s.ToString())
            |> Seq.fold(fun (sb:System.Text.StringBuilder) s ->
                sb.Append(s)) (new System.Text.StringBuilder())
            |> fun x -> x.ToString()

    type IfExpression =
        {
            Token: Token
            Condition: Expression
            Consequence: BlockStatement
            Alternative: BlockStatement
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            sprintf "if %s %s else %s" (x.Condition.ToString()) (x.Consequence.ToString()) (x.Alternative.ToString())

    type FunctionLiteral =
        {
            Token: Token
            Parameters: Identifier list
            Body: BlockStatement
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            let parameterValues = 
                x.Parameters
                |> Seq.map(fun s -> s.ToString())
                |> fun x -> x |> String.concat ", "

            sprintf "%s (%s) %s" (x.TokenLiteral()) parameterValues (x.Body.ToString())

    type CallExpression =
        {
            Token: Token
            Function: Expression
            Arguments: Expression list
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            let argumentValues = 
                x.Arguments
                |> Seq.map(fun s -> s.ToString())
                |> fun x -> x |> String.concat ", "

            sprintf "%s(%s)" (x.Function.ToString()) argumentValues

    type StringLiteral =
        {
            Token: Token
            Value: string
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            x.Token.Literal

    type ArrayLiteral =
        {
            Token: Token
            Elements: Expression list
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            let elementValues = 
                x.Elements
                |> Seq.map(fun s -> s.ToString())
                |> fun x -> x |> String.concat ", "

            sprintf "[%s]" elementValues

    type IndexExpression =
        {
            Token: Token
            Left: Expression
            Index: Expression
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            sprintf "(%s[%s])" (x.Left.ToString()) (x.Index.ToString())

    type HashLiteral =
        {
            Token: Token
            Pairs: System.Collections.Generic.IDictionary<Expression, Expression>
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            let pairValues =
                x.Pairs
                |> Seq.map(fun kvp -> kvp.Key.ToString() + ":" + kvp.Value.ToString())
                |> fun x -> x |> String.concat ", "

            sprintf "{%s}" pairValues

    type MacroLiteral =
        {
            Token: Token
            Parameters: Identifier list
            Body: BlockStatement
        }
        interface Expression with
            member this.TokenLiteral() =
                this.Token.Literal
        member this.TokenLiteral() = (this :> Expression).TokenLiteral()
        override x.ToString() =
            let parameterValues = 
                x.Parameters
                |> Seq.map(fun s -> s.ToString())
                |> fun x -> x |> String.concat ", "

            sprintf "%s (%s) %s" (x.TokenLiteral()) parameterValues (x.Body.ToString())

    // dummy types
    // todo: figure out a way to get rid of this
    type EmptyStatement = 
        interface Statement with
            member __.TokenLiteral() =
                ""
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        new() = {}

    