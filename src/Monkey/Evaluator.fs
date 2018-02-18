namespace Monkey

open Ast
open Object

module Evaluator =

    let TRUE = {Object.Boolean.Value = true} :> Object
    let FALSE = {Object.Boolean.Value = false} :> Object
    let NULL = Null() :> Object

    let eval node : Object =

        let boolToBooleanObject boolVal =
                if boolVal then
                    TRUE
                else
                    FALSE
        
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
            obj.Type() = Object.ObjectTypes.ERROR_OBJ


        let env = {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>()}

        let rec evalRec (node:Node) : Object =

            let rec evalProgram (unevaluatedStatements : Statement List) (results : Object List)  =
                match unevaluatedStatements with
                | [] -> 
                    List.head results
                | x::xs ->
                    let result = evalRec x 
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
                    let result = evalRec x 
                    let resultType = result.Type()
                    if resultType = Object.ObjectTypes.RETURN_VALUE_OBJ || resultType = Object.ObjectTypes.ERROR_OBJ then
                        result
                    else
                        evalBlockStatement xs (result::results)

            match node with
            | :? Program as p ->
                evalProgram p.Statements []
            | :? ExpressionStatement as es ->
                evalRec es.Expression
            | :? IntegerLiteral as il ->
                {Object.Value = il.Value} :> Object
            | :? Ast.Boolean as bl ->
                boolToBooleanObject bl.Value
            | :? PrefixExpression as pe ->
                let right = evalRec pe.Right

                //todo: all this isError stuff should probably be switched to a railroad-style failure system?
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
                    | _ ->
                        sprintf "unknown operator: %s%s" pe.Operator (unwrapObjectType right) |> newError
            | :? InfixExpression as ie ->
                let left = evalRec ie.Left
                if isError left then
                    left
                else
                    let right = evalRec ie.Right
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
                            | _ ->
                                sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
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
                        | _ ->
                            sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
            | :? BlockStatement as bs ->
                evalBlockStatement bs.Statements []
            | :? IfExpression as ie ->
                let condition = evalRec ie.Condition
                if isError condition then
                    condition
                else
                    match isTruthy condition with
                    | true ->
                        evalRec ie.Consequence
                    | false ->
                        // in the go implementation, Alternative would be null;
                        // we instead have an empty alternative with no statements
                        match ie.Alternative.Statements with
                        | [] ->
                            NULL
                        | _ ->
                            evalRec ie.Alternative
            | :? ReturnStatement as rs ->
                let value = evalRec rs.ReturnValue
                if isError value then
                    value
                else
                    {ReturnValue.Value = value} :> Object
            | :? LetStatement as ls ->
                let value = evalRec ls.Value
                if isError value then
                    value
                else
                    env.Set ls.Name.Value value
            | :? Identifier as i ->
                let envValue = env.Get i.Value
                match envValue with
                | Some(v) ->
                    v
                | None ->
                    sprintf "identifier not found: %s" i.Value |> newError
            | _ -> NULL

        evalRec node

        