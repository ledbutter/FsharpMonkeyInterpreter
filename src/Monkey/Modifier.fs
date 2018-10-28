namespace Monkey

module Modifier = 
    
    open Object
    open Ast

    let rec modify (node:Node) (modifier:Node->Node*Environment) : Node =

        let modifyNodeCollection (nodes:list<'a>) =
            nodes |> List.map (fun n -> (modify n modifier) :?> 'a)

        let modifiedNode = 
            match node with
            | :? Program as p ->
                let modifiedStatements = p.Statements |> modifyNodeCollection
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
                let newAlternative = 
                    match ie.Alternative.Statements with
                    | [] -> ie.Alternative
                    | _ -> (modify ie.Alternative modifier) :?> BlockStatement
                {ie with Condition = newCondition; Consequence = newConsequence; Alternative = newAlternative} :> Node
            | :? BlockStatement as bs ->
                let modifiedStatements = bs.Statements |> modifyNodeCollection
                {bs with Statements = List.ofSeq modifiedStatements} :> Node
            | :? ReturnStatement as rs ->
                let newValue = (modify rs.ReturnValue modifier) :?> Expression
                {rs with ReturnValue = newValue} :> Node
            | :? LetStatement as ls ->
                let newValue = (modify ls.Value modifier) :?> Expression
                {ls with Value = newValue} :> Node
            | :? FunctionLiteral as fl ->
                let modifiedParameters = fl.Parameters |> modifyNodeCollection
                let modifiedBody = (modify fl.Body modifier) :?> BlockStatement
                {fl with Parameters = List.ofSeq modifiedParameters; Body = modifiedBody} :> Node
            | :? ArrayLiteral as al ->
                let modifiedExpressions = al.Elements |> modifyNodeCollection
                {al with Elements = List.ofSeq modifiedExpressions} :> Node
            | :? HashLiteral as hl ->
                let modifiedPairs = 
                    hl.Pairs 
                    |> Seq.map (fun kvp -> ((modify kvp.Key modifier) :?> Expression), ((modify kvp.Value modifier) :?> Expression)) 
                    |> dict
                {hl with Pairs = modifiedPairs} :> Node

            | _ ->
                node
        
        modifier modifiedNode

