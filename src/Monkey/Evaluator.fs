﻿namespace Monkey

open Ast
open Object

module Evaluator =
    open Object.ObjectTypes

    let TRUE = {Object.Boolean.Value = true} :> Object
    let FALSE = {Object.Boolean.Value = false} :> Object
    let NULL = Null() :> Object

    let rec eval (node:Node) : Object =

        let boolToBooleanObject boolVal =
            if boolVal then
                TRUE
            else
                FALSE

        let rec evalProgram (unevaluatedStatements : Statement List) (results : Object List) =
            match unevaluatedStatements with
            | [] -> 
                List.head results
            | x::xs ->
                let result = eval x
                match result with
                | :? ReturnValue as rv ->
                    rv.Value
                | :? Error as __ ->
                    result
                | _ ->
                    evalProgram xs (result::results)

        let rec evalBlockStatement (unevaluatedStatements : Statement List) (results : Object List) =
            match unevaluatedStatements with
            | [] ->
                List.head results
            | x::xs ->
                let result = eval x
                let resultType = result.Type()
                if resultType = Object.ObjectTypes.RETURN_VALUE_OBJ || resultType = Object.ObjectTypes.ERROR_OBJ then
                    result
                else
                    evalBlockStatement xs (result::results)
        
        let isTruthy object =
            if object = NULL then
                false
            else if object = TRUE then
                true
            else if object = FALSE then
                false
            else
                true

        let newError errorMessage =
            {Error.Message = errorMessage} :> Object

        let unwrapObjectType (obj:Object) =
            let unwrapObjectTypeValue (ObjectType ot) = ot
            obj.Type() |> unwrapObjectTypeValue

        let isError (obj:Object) =
            obj.Type() = ERROR_OBJ

        match node with
        | :? Program as p ->
            evalProgram p.Statements []
        | :? ExpressionStatement as es ->
            es.Expression |> eval
        | :? IntegerLiteral as il ->
            {Object.Value = il.Value} :> Object
        | :? Ast.Boolean as bl ->
            boolToBooleanObject bl.Value
        | :? PrefixExpression as pe ->
            let right = pe.Right |> eval

            if isError right then
                right
            else
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
                        sprintf "unknown operator: -%s" (unwrapObjectType right) |> newError
                        //NULL
                | _ ->
                    sprintf "unknown operator: %s%s" pe.Operator (unwrapObjectType right) |> newError
                //NULL
        | :? InfixExpression as ie ->
            let left = ie.Left |> eval
            if isError left then
                left
            else
                let right = ie.Right |> eval
                if isError right then
                    right
                else
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
                                sprintf "unknown operator: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                                //NULL
                        | _ ->
                            sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                            //NULL
                    | :? Boolean as lb ->
                        match right with
                        | :? Boolean as rb ->
                            match ie.Operator with
                            | "==" ->
                                boolToBooleanObject (lb.Value = rb.Value)
                            | "!=" ->
                                boolToBooleanObject (lb.Value <> rb.Value)
                            | _ ->
                                sprintf "unknown operator: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                        | _ ->
                            sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                            //NULL
                    | _ ->
                        sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                        //NULL
        | :? BlockStatement as bs ->
            evalBlockStatement bs.Statements []
        | :? IfExpression as ie ->
            let condition = ie.Condition |> eval
            if isError condition then
                condition
            else
                match isTruthy condition with
                | true ->
                    ie.Consequence |> eval
                | false ->
                    // in the go implementation, Alternative would be null;
                    // we instead have an empty alternative with no statements
                    match ie.Alternative.Statements with
                    | [] ->
                        NULL
                    | _ ->
                        ie.Alternative |> eval
        | :? ReturnStatement as rs ->
            let value = rs.ReturnValue |> eval
            if isError value then
                value
            else
                {ReturnValue.Value = value} :> Object
        | _ -> NULL