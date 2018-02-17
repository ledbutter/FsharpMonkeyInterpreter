namespace Monkey

open Ast
open Object

module Evaluator =

    let rec eval (node:Node) : Object =

        let TRUE = {Object.Boolean.Value = true} :> Object
        let FALSE = {Object.Boolean.Value = false} :> Object
        let NULL = Null() :> Object

        let boolToBooleanObject boolVal =
            if boolVal then
                TRUE
            else
                FALSE

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
            boolToBooleanObject bl.Value
        | :? PrefixExpression as pe ->
            let right = pe.Right |> eval
            match pe.Operator with
            | "!" ->
                if right = TRUE then
                    FALSE
                else if right = FALSE then
                    TRUE
                else if right = NULL then
                    TRUE
                else
                    FALSE
            | "-" ->
                match right with
                | :? Integer as i ->
                    {Integer.Value = -i.Value} :> Object
                | _ -> 
                    NULL
            | _ -> NULL
        | :? InfixExpression as ie ->
            let left = ie.Left |> eval
            let right = ie.Right |> eval
            match left with
            | :? Integer as li ->
                match right with
                | :? Integer as ri ->
                    match ie.Operator with
                    | "+" ->
                        {Integer.Value = li.Value + ri.Value} :> Object
                    | "-" ->
                        {Integer.Value = li.Value - ri.Value} :> Object
                    | "*" ->
                        {Integer.Value = li.Value * ri.Value} :> Object
                    | "/" ->
                        {Integer.Value = li.Value / ri.Value} :> Object
                    | "<" ->
                        boolToBooleanObject (li.Value < ri.Value)
                    | ">" ->
                        boolToBooleanObject (li.Value > ri.Value)
                    | "==" ->
                        boolToBooleanObject (li.Value = ri.Value)
                    | "!=" ->
                        boolToBooleanObject (li.Value <> ri.Value)
                    | _ ->
                        NULL
                | _ -> NULL
            | :? Boolean as lb ->
                match right with
                | :? Boolean as rb ->
                    match ie.Operator with
                    | "==" ->
                        boolToBooleanObject (lb.Value = rb.Value)
                    | "!=" ->
                        boolToBooleanObject (lb.Value <> rb.Value)
                    | _ ->
                        NULL
                | _ ->
                    NULL
            | _ -> NULL
        | _ -> NULL