namespace Monkey

open Ast
open Object

module Evaluator =

    let rec eval (node:Node) =
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
        | _ -> Null() :> Object
