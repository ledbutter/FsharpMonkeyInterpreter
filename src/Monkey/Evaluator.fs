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

        let rec evalRec (node:Node) (currentEnv:Environment) : (Object*Environment) =

            let rec evalProgram (unevaluatedStatements : Statement List) (results : Object List) programEnv =
                match unevaluatedStatements with
                | [] -> 
                    ((List.head results), programEnv)
                | x::xs ->
                    let result, env = evalRec x programEnv
                    match result with
                    | :? ReturnValue as rv ->
                        (rv.Value, env)
                    | :? Error as __ ->
                        (result, env)
                    | _ ->
                        evalProgram xs (result::results) env

            let rec evalBlockStatement (unevaluatedStatements : Statement List) (results : Object List) blockEnv =
                match unevaluatedStatements with
                | [] ->
                    ((List.head results), blockEnv)
                | x::xs ->
                    let result, env = evalRec x blockEnv
                    let resultType = result.Type()
                    if resultType = Object.ObjectTypes.RETURN_VALUE_OBJ || resultType = Object.ObjectTypes.ERROR_OBJ then
                        result, env
                    else
                        evalBlockStatement xs (result::results) env

            match node with
            | :? Program as p ->
                evalProgram p.Statements [] currentEnv
            | :? ExpressionStatement as es ->
                evalRec es.Expression currentEnv
            | :? IntegerLiteral as il ->
                {Object.Value = il.Value} :> Object, currentEnv
            | :? Ast.Boolean as bl ->
                ((boolToBooleanObject bl.Value), currentEnv)
            | :? PrefixExpression as pe ->
                let right, env = evalRec pe.Right currentEnv

                //todo: all this isError stuff should probably be switched to a railroad-style failure system?
                if isError right then
                    right, env
                else
                    match pe.Operator with
                    | "!" ->
                        if right = TRUE then
                            FALSE, env
                        else if right = FALSE then
                            TRUE, env
                        else if right = NULL then
                            TRUE, env
                        else
                            FALSE, env
                    | "-" ->
                        match right with
                        | :? Integer as i ->
                            {Integer.Value = -i.Value} :> Object, env
                        | _ ->
                            let error = sprintf "unknown operator: -%s" (unwrapObjectType right) |> newError
                            error, env
                    | _ ->
                        let error = sprintf "unknown operator: %s%s" pe.Operator (unwrapObjectType right) |> newError
                        error, env
            | :? InfixExpression as ie ->
                let left, env = evalRec ie.Left currentEnv
                if isError left then
                    left, env
                else
                    let right, env = evalRec ie.Right env
                    if isError right then
                        right, env
                    else
                        match left with
                        | :? Integer as li ->
                            match right with
                            | :? Integer as ri ->
                                match ie.Operator with
                                | "+" ->
                                    {Integer.Value = li.Value + ri.Value} :> Object, env
                                | "-" ->
                                    {Integer.Value = li.Value - ri.Value} :> Object, env
                                | "*" ->
                                    {Integer.Value = li.Value * ri.Value} :> Object, env
                                | "/" ->
                                    {Integer.Value = li.Value / ri.Value} :> Object, env
                                | "<" ->
                                    boolToBooleanObject (li.Value < ri.Value), env
                                | ">" ->
                                    boolToBooleanObject (li.Value > ri.Value), env
                                | "==" ->
                                    boolToBooleanObject (li.Value = ri.Value), env
                                | "!=" ->
                                    boolToBooleanObject (li.Value <> ri.Value), env
                                | _ ->
                                    let error = sprintf "unknown operator: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                                    error, env
                            | _ ->
                                let error = sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                                error, env
                        | :? Boolean as lb ->
                            match right with
                            | :? Boolean as rb ->
                                match ie.Operator with
                                | "==" ->
                                    ((boolToBooleanObject (lb.Value = rb.Value)), env)
                                | "!=" ->
                                    ((boolToBooleanObject (lb.Value <> rb.Value)), env)
                                | _ ->
                                    let error = sprintf "unknown operator: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                                    error, env
                            | _ ->
                                let error = sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                                error, env
                        | _ ->
                            let error = sprintf "type mismatch: %s %s %s" (unwrapObjectType left) ie.Operator (unwrapObjectType right) |> newError
                            error, env
            | :? BlockStatement as bs ->
                evalBlockStatement bs.Statements [] currentEnv
            | :? IfExpression as ie ->
                let condition, env = evalRec ie.Condition currentEnv
                if isError condition then
                    condition, env
                else
                    match isTruthy condition with
                    | true ->
                        evalRec ie.Consequence env
                    | false ->
                        // in the go implementation, Alternative would be null;
                        // we instead have an empty alternative with no statements
                        match ie.Alternative.Statements with
                        | [] ->
                            NULL, env
                        | _ ->
                            evalRec ie.Alternative env
            | :? ReturnStatement as rs ->
                let value, env = evalRec rs.ReturnValue currentEnv
                if isError value then
                    value, env
                else
                    {ReturnValue.Value = value} :> Object, env
            | :? LetStatement as ls ->
                let value, env = evalRec ls.Value currentEnv
                if isError value then
                    value, env
                else
                    ((currentEnv.Set ls.Name.Value value), env)
            | :? Identifier as i ->
                let envValue = currentEnv.Get i.Value
                match envValue with
                | Some(v) ->
                    v, currentEnv
                | None ->
                    let error = sprintf "identifier not found: %s" i.Value |> newError
                    error, currentEnv
            | :? FunctionLiteral as fl ->
                {Function.Body = fl.Body; Parameters = fl.Parameters; Env = currentEnv} :> Object, currentEnv
            | :? CallExpression as ce ->
                let funcObject, env = evalRec ce.Function currentEnv
                if isError funcObject then
                    funcObject, env
                else
                    let rec evalExpressions expressions results env' =
                        match expressions with
                        | [] ->
                            results, env'
                        | x::xs ->
                            let evaluated, env'' = evalRec x env'
                            if isError evaluated then
                                [evaluated], env''
                            else
                                evalExpressions xs (evaluated::results) env''
                    let args, env = evalExpressions ce.Arguments [] currentEnv
                    match args with
                    | x::xs when isError x && List.isEmpty xs ->
                        x, env
                    | _ ->
                        let func = funcObject :?> Function
                        let funcEnvironment = {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = Some(currentEnv)}
                        for i in 0..func.Parameters.Length-1 do
                            let argValue = args.[i]
                            let param = func.Parameters.[i]
                            funcEnvironment.Set param.Value argValue |> ignore
                        
                        let evaluated, env = evalRec func.Body funcEnvironment
                        match evaluated with
                        | :? ReturnValue as rv ->
                            rv.Value, env
                        | _ ->
                            evaluated, env
            | _ -> 
                NULL, currentEnv

        let res, _ = evalRec node {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = None}
        res

        