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

        let env = {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = None}

        let rec evalRec (node:Node) (currentEnv:Environment) : Object =

            let rec evalProgram (unevaluatedStatements : Statement List) (results : Object List) programEnv =
                match unevaluatedStatements with
                | [] -> 
                    List.head results
                | x::xs ->
                    let result = evalRec x programEnv
                    match result with
                    | :? ReturnValue as rv ->
                        rv.Value
                    | :? Error as __ ->
                        result
                    | _ ->
                        evalProgram xs (result::results) programEnv

            let rec evalBlockStatement (unevaluatedStatements : Statement List) (results : Object List) blockEnv =
                match unevaluatedStatements with
                | [] ->
                    List.head results
                | x::xs ->
                    let result = evalRec x blockEnv
                    let resultType = result.Type()
                    if resultType = Object.ObjectTypes.RETURN_VALUE_OBJ || resultType = Object.ObjectTypes.ERROR_OBJ then
                        result
                    else
                        evalBlockStatement xs (result::results) blockEnv

            match node with
            | :? Program as p ->
                evalProgram p.Statements [] currentEnv
            | :? ExpressionStatement as es ->
                evalRec es.Expression currentEnv
            | :? IntegerLiteral as il ->
                {Object.Value = il.Value} :> Object
            | :? Ast.Boolean as bl ->
                boolToBooleanObject bl.Value
            | :? PrefixExpression as pe ->
                let right = evalRec pe.Right currentEnv

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
                let left = evalRec ie.Left currentEnv
                if isError left then
                    left
                else
                    let right = evalRec ie.Right currentEnv
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
                evalBlockStatement bs.Statements [] currentEnv
            | :? IfExpression as ie ->
                let condition = evalRec ie.Condition currentEnv
                if isError condition then
                    condition
                else
                    match isTruthy condition with
                    | true ->
                        evalRec ie.Consequence currentEnv
                    | false ->
                        // in the go implementation, Alternative would be null;
                        // we instead have an empty alternative with no statements
                        match ie.Alternative.Statements with
                        | [] ->
                            NULL
                        | _ ->
                            evalRec ie.Alternative currentEnv
            | :? ReturnStatement as rs ->
                let value = evalRec rs.ReturnValue currentEnv
                if isError value then
                    value
                else
                    {ReturnValue.Value = value} :> Object
            | :? LetStatement as ls ->
                let value = evalRec ls.Value currentEnv
                if isError value then
                    value
                else
                    currentEnv.Set ls.Name.Value value
            | :? Identifier as i ->
                let envValue = currentEnv.Get i.Value
                match envValue with
                | Some(v) ->
                    v
                | None ->
                    sprintf "identifier not found: %s" i.Value |> newError
            | :? FunctionLiteral as fl ->
                {Function.Body = fl.Body; Parameters = fl.Parameters; Env = env} :> Object
            | :? CallExpression as ce ->
                let funcObject = evalRec ce.Function currentEnv
                if isError funcObject then
                    funcObject
                else
                    let rec evalExpressions expressions results env'' =
                        match expressions with
                        | [] ->
                            results
                        | x::xs ->
                            let evaluated = evalRec x env''
                            if isError evaluated then
                                [evaluated]
                            else
                                evalExpressions xs (evaluated::results) env''
                    let args = evalExpressions ce.Arguments [] currentEnv
                    match args with
                    | x::xs when isError x && List.isEmpty xs ->
                        x
                    | _ ->
                        let func = funcObject :?> Function
                        let argDict = new System.Collections.Generic.Dictionary<string, Object>()
                        for i in 0..func.Parameters.Length-1 do
                            let argValue = args.[i]
                            argDict.Add((func.Parameters.[i].Value), argValue)

                        let funcEnvironment = {Environment.Store = argDict; Outer = Some(currentEnv)}
                        let evaluated = evalRec func.Body funcEnvironment
                        match evaluated with
                        | :? ReturnValue as rv ->
                            rv.Value
                        | _ ->
                            evaluated

            | _ -> NULL

        evalRec node env

        