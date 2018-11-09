namespace Monkey

module Modifier = 
    
    open Object
    open Ast

    let rec modify (node:Node) (env:Environment) (modifier:Node->Environment->Node*Environment) : Node*Environment =

        let modifyNodeCollection (nodes:list<'a>) (currentEnv:Environment) =
            let rec modifyNodeCollection' currentNodes env modifiedNodes =
                match currentNodes with
                | x::xs ->
                    let modified, newEnv = modify x env modifier
                    modifyNodeCollection' xs newEnv (modified :?> 'a::modifiedNodes)
                | [] ->
                    List.rev modifiedNodes, env

            modifyNodeCollection' nodes currentEnv []
            //nodes |> List.map (fun n -> (modify n currentEnv modifier) :?> 'a)

        let modifiedNode, newCurrentEnv = 
            match node with
            | :? Program as p ->
                let modifiedStatements, newEnv = p.Statements |> modifyNodeCollection <| env
                {p with Statements = List.ofSeq modifiedStatements} :> Node, newEnv
            | :? ExpressionStatement as es ->
                //let modExpression = (modify es.Expression modifier) :?> Expression
                let modExpression, newEnv = modify es.Expression env modifier
                {es with Expression = modExpression :?> Expression} :> Node, newEnv
            | :? InfixExpression as ie ->
                let newLeft, newEnv = modify ie.Left env modifier
                let newRight, newEnv = modify ie.Right newEnv modifier
//                let newLeft = (modify ie.Left modifier) :?> Expression
//                let newRight = (modify ie.Right modifier) :?> Expression
                {ie with Left = newLeft :?> Expression; Right = newRight :?> Expression} :> Node, newEnv
            | :? PrefixExpression as pe ->
                //let newRight = (modify pe.Right modifier) :?> Expression
                let newRight, newEnv = modify pe.Right env modifier
                {pe with Right = newRight :?> Expression} :> Node, newEnv
            | :? IndexExpression as ie ->
//                let newLeft = (modify ie.Left modifier) :?> Expression
//                let newIndex = (modify ie.Index modifier) :?> Expression
                let newLeft, newEnv = modify ie.Left env modifier
                let newIndex, newEnv = modify ie.Index newEnv modifier
                {ie with Left = newLeft :?> Expression; Index = newIndex :?> Expression} :> Node, newEnv
            | :? IfExpression as ie ->
//                let newCondition = (modify ie.Condition modifier) :?> Expression
//                let newConsequence = (modify ie.Consequence modifier) :?> BlockStatement
                let newCondition, newEnv = modify ie.Condition env modifier
                let newConsequence, newEnv = modify ie.Consequence newEnv modifier
                let newAlternative, newEnv = 
                    match ie.Alternative.Statements with
                    | [] -> 
                        ie.Alternative, newEnv
                    | _ ->
                        let tempMod, tempEnv = modify ie.Alternative newEnv modifier
                        tempMod :?> BlockStatement, tempEnv
                        //(modify ie.Alternative modifier) :?> BlockStatement
                {ie with Condition = newCondition :?> Expression; Consequence = newConsequence :?> BlockStatement; Alternative = newAlternative} :> Node, newEnv
            | :? BlockStatement as bs ->
                let modifiedStatements, newEnv = bs.Statements |> modifyNodeCollection <| env
                //let modifiedStatements = bs.Statements |> modifyNodeCollection
                {bs with Statements = List.ofSeq modifiedStatements} :> Node, newEnv
            | :? ReturnStatement as rs ->
                let newReturnValue, newEnv = modify rs.ReturnValue env modifier
                //let newValue = (modify rs.ReturnValue modifier) :?> Expression
                {rs with ReturnValue = newReturnValue :?> Expression} :> Node, newEnv
            | :? LetStatement as ls ->
                let newValue, newEnv = modify ls.Value env modifier
                //let newValue = (modify ls.Value modifier) :?> Expression
                {ls with Value = newValue :?> Expression} :> Node, newEnv
            | :? FunctionLiteral as fl ->
                let modifiedParameters, newEnv = fl.Parameters |> modifyNodeCollection <| env
                let modifiedBody, newEnv = modify fl.Body newEnv modifier
                //let modifiedBody = (modify fl.Body modifier) :?> BlockStatement
                {fl with Parameters = List.ofSeq modifiedParameters; Body = modifiedBody :?> BlockStatement} :> Node, newEnv
            | :? ArrayLiteral as al ->
                let modifiedExpressions, newEnv = al.Elements |> modifyNodeCollection <| env
                {al with Elements = List.ofSeq modifiedExpressions} :> Node, newEnv
            | :? HashLiteral as hl ->
                let modifiedPairs, newEnv =
                    hl.Pairs
                    |> Seq.mapFold (fun currentEnv kvp -> 
                        let modifiedKey, tempEnv = modify kvp.Key currentEnv modifier
                        let modifiedValue, tempEnv = modify kvp.Value tempEnv modifier
                        (modifiedKey :?> Expression, modifiedValue :?> Expression), tempEnv
                        ) env
                {hl with Pairs = modifiedPairs |> dict} :> Node, newEnv
//                let rec modifyPairs originalPairs currentEnv modifiedPairs =
//                    match originalPairs with
//                    | x::xs ->
//                        let modifiedKey, newEnv = modify x.Key currentEnv modifier
//                        
//                        //modifyPairs xs newEnv (modifiedPair :?> Expression::modifiedPairs)
//                    | [] ->
//                        modifiedPairs |> dict, currentEnv
//
//                let modifiedPairs, newEnv = modifyPairs hl.Pairs env []
//                {hl with Pairs = modifiedPairs} :> Node, newEnv
//                let modifiedPairs = 
//                    hl.Pairs 
//                    |> Seq.map (fun kvp -> ((modify kvp.Key modifier) :?> Expression), ((modify kvp.Value modifier) :?> Expression)) 
//                    |> dict
//                {hl with Pairs = modifiedPairs} :> Node

            | _ ->
                node, env
        
        modifier modifiedNode newCurrentEnv

