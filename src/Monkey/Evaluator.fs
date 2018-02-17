namespace Monkey

open Ast
open Object

module Evaluator =
    open Object.ObjectTypes

    let rec eval (node:Node) : Object =

        let TRUE = {Object.Boolean.Value = true} :> Object
        let FALSE = {Object.Boolean.Value = false} :> Object
        let NULL = Null() :> Object

        match node with
        | :? Program as p ->
            let rec evalStatements (unevaluatedStatements : Statement List) (results : Object List) =
                match unevaluatedStatements with
                | [] -> 
                    List.head results
                | x::xs ->
                    let result = eval x
                    evalStatements xs (result::results)
            evalStatements (p.Statements) []
        | :? ExpressionStatement as es ->
            es.Expression |> eval
        | :? IntegerLiteral as il ->
            {Object.Value = il.Value} :> Object
        | :? Ast.Boolean as bl ->
            if bl.Value then
                TRUE
            else
                FALSE
        | :? PrefixExpression as pe ->
            let right = pe.Right |> eval
            let evalPrefixExpression operator (right':Object) =
                match operator with
                | "!" ->
                    if right' = TRUE then
                        FALSE
                    else if right' = FALSE then
                        TRUE
                    else if right' = NULL then
                        TRUE
                    else
                        FALSE
                | "-" ->
                    match right' with
                    | :? Integer as i ->
                        {Integer.Value = -i.Value} :> Object
                    | _ -> 
                        NULL
                | _ -> NULL
            evalPrefixExpression pe.Operator right
        | _ -> NULL