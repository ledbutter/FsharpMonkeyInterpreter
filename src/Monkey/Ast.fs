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
            Pairs: System.Collections.Generic.Dictionary<Expression, Expression>
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

    let rec modify (node:Node) (modifier:Node->Node) : Node =

        let modifiedNode = match node with
        | :? Program as p ->
            // todo: this is the worst stuff i've written yet
            let modifiedStatements = new System.Collections.Generic.List<Statement>(p.Statements.Length)

            for s in p.Statements do
                let newMod = (modify s modifier) :?> Statement
                modifiedStatements.Add(newMod)

            {p with Statements = List.ofSeq modifiedStatements} :> Node
        | :? ExpressionStatement as es ->
            let modExpression = (modify es.Expression modifier) :?> Expression
            {es with Expression = modExpression} :> Node
        | :? InfixExpression as ie ->
            let newLeft = (modify ie.Left modifier) :?> Expression
            let newRight = (modify ie.Right modifier) :?> Expression
            {ie with Left = newLeft; Right = newRight} :> Node
        | :? PrefixExpression as pe ->
            let newRight = (modify pe.Right modifier) :?> Expression
            {pe with Right = newRight} :> Node
        | :? IndexExpression as ie ->
            let newLeft = (modify ie.Left modifier) :?> Expression
            let newIndex = (modify ie.Index modifier) :?> Expression
            {ie with Left = newLeft; Index = newIndex} :> Node
        | :? IfExpression as ie ->
            let newCondition = (modify ie.Condition modifier) :?> Expression
            let newConsequence = (modify ie.Consequence modifier) :?> BlockStatement
            let newAlternative = match ie.Alternative.Statements with
            | [] -> ie.Alternative// :?> Expression
            | _ -> (modify ie.Alternative modifier) :?> BlockStatement
            {ie with Condition = newCondition; Consequence = newConsequence; Alternative = newAlternative} :> Node
        | :? BlockStatement as bs ->
            // todo: this is the worst stuff i've written yet
            let modifiedStatements = new System.Collections.Generic.List<Statement>(bs.Statements.Length)
            for s in bs.Statements do
                let newMod = (modify s modifier) :?> Statement
                modifiedStatements.Add(newMod)
            {bs with Statements = List.ofSeq modifiedStatements} :> Node
        | :? ReturnStatement as rs ->
            let newValue = (modify rs.ReturnValue modifier) :?> Expression
            {rs with ReturnValue = newValue} :> Node
        | :? LetStatement as ls ->
            let newValue = (modify ls.Value modifier) :?> Expression
            {ls with Value = newValue} :> Node
        | :? FunctionLiteral as fl ->
            // todo: this is the worst stuff i've written yet
            let modifiedParameters = new System.Collections.Generic.List<Identifier>(fl.Parameters.Length)
            for p in fl.Parameters do
                let newMod = (modify p modifier) :?> Identifier
                modifiedParameters.Add(newMod)
            let modifiedBody = (modify fl.Body modifier) :?> BlockStatement
            {fl with Parameters = List.ofSeq modifiedParameters; Body = modifiedBody} :> Node
        | _ ->
            node
        
        modifier modifiedNode

    // dummy types
    // todo: figure out a way to get rid of this
    type EmptyStatement = 
        interface Statement with
            member __.TokenLiteral() =
                ""
        member this.TokenLiteral() = (this :> Statement).TokenLiteral()
        new() = {}

    