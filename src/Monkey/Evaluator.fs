namespace Monkey

open Ast
open Object
open Modifier

module Evaluator =

    let TRUE = {Object.Boolean.Value = true} :> Object
    let FALSE = {Object.Boolean.Value = false} :> Object
    let NULL = Null() :> Object

    type HashOutput =
        | Success of System.Collections.Generic.Dictionary<HashKey, HashPair>
        | Error of Object
        | BombOut of Object

    let eval node : Object =

        let newError errorMessage =
            {Error.Message = errorMessage} :> Object

        let unwrapObjectType (obj:Object) =
            let unwrapObjectTypeValue (ObjectType ot) = ot
            obj.Type() |> unwrapObjectTypeValue

        let (|SingleElementArray|_|) arr =
            if List.length arr = 1 then Some(List.head arr)
            else None

        let len (args: Object list) =
            match args with
            | SingleElementArray e ->
                match e with
                | :? String as s ->
                    {Integer.Value = int64(s.Value.Length)} :> Object
                | :? Array as arr ->
                    {Integer.Value = int64(arr.Elements.Length)} :> Object
                | _ ->
                    let errorMsg = sprintf @"argument to ""len"" not supported, got %s" (e |> unwrapObjectType)
                    errorMsg |> newError
            | _ ->
                let errorMsg = sprintf "wrong number of arguments. got=%i, want=1" args.Length
                errorMsg |> newError

        let first (args: Object list) =
            match args with
            | SingleElementArray e ->
                match e with 
                | :? Array as arr ->
                    match arr.Elements with
                    | x::_ -> 
                        x
                    | _ ->
                        NULL
                | _ ->
                    let errorMsg = sprintf @"argument to ""first"" must be ARRAY, got %s" ( e |> unwrapObjectType)
                    errorMsg |> newError
            | _ ->
                let errorMsg = sprintf "wrong number of arguments. got=%i, want=1" args.Length
                errorMsg |> newError

        let last (args: Object list) =
            match args with
            | SingleElementArray e ->
                match e with 
                | :? Array as arr ->
                    match arr.Elements with
                    | _::xs -> 
                        xs.[xs.Length-1]
                    | _ ->
                        NULL
                | _ ->
                    let errorMsg = sprintf @"argument to ""last"" must be ARRAY, got %s" ( e |> unwrapObjectType)
                    errorMsg |> newError
            | _ ->
                let errorMsg = sprintf "wrong number of arguments. got=%i, want=1" args.Length
                errorMsg |> newError

        let rest (args: Object list) =
            match args with
            | SingleElementArray e ->
                match e with
                | :? Array as arr ->
                    match arr.Elements with
                    | _::xs ->
                        {Array.Elements = xs} :> Object
                    | _ ->
                        NULL
                | _ ->
                    let errorMsg = sprintf @"argument to ""rest"" must be ARRAY, got %s" ( e |> unwrapObjectType)
                    errorMsg |> newError
            | _ ->
                let errorMsg = sprintf "wrong number of arguments. got=%i, want=1" args.Length
                errorMsg |> newError

        let push (args: Object list) =
            match args with
            | x::xs when (List.length xs) = 1 ->
                match x with
                | :? Array as arr ->
                    let newElements = (List.head xs)::arr.Elements
                    {Array.Elements = newElements} :> Object
                | _ ->
                    let errorMsg = sprintf @"argument to ""push"" must be ARRAY, got %s" ( x |> unwrapObjectType)
                    errorMsg |> newError
            | _ ->
                let errorMsg = sprintf "wrong number of arguments. got=%i, want=2" args.Length
                errorMsg |> newError

        let puts (args: Object list) =
            let rec putsRec (args': Object list) =
                match args' with
                | [] ->
                    NULL
                | x::xs ->
                    printfn "%s" (x.Inspect())
                    putsRec xs
            putsRec args


        let builtIns = dict [ "len", {BuiltIn.Fn = len};
                              "first", {BuiltIn.Fn = first};
                              "last", {BuiltIn.Fn = last};
                              "rest", {BuiltIn.Fn = rest};
                              "push", {BuiltIn.Fn = push};
                              "puts", {BuiltIn.Fn = puts};]


        let boolToBooleanObject boolVal =
                if boolVal then
                    TRUE
                else
                    FALSE
        
        let isTruthy obj =
            if obj = NULL then
                false
            else if obj = TRUE then
                true
            else if obj = FALSE then
                false
            else
                true

        let isError (obj:Object) =
            obj.Type() = Object.ObjectTypes.ERROR_OBJ

        let (|HasError|_|) (obj:Object list) =
            match obj with
            | x::xs when isError x && List.isEmpty xs ->
                Some(x)
            | _ ->
                None

        let rec evalRec (node:Node) (currentEnv:Environment) : (Object*Environment) =

            let rec evalExpressions expressions results env' =
                match expressions with
                | [] ->
                    (List.rev results), env'
                | x::xs ->
                    let evaluated, env'' = evalRec x env'
                    if isError evaluated then
                        [evaluated], env''
                    else
                        evalExpressions xs (evaluated::results) env''

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
                {Integer.Value = il.Value} :> Object, currentEnv
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
                        | :? String as ls ->
                            match right with
                            | :? String as rs ->
                                match ie.Operator with
                                | "+" ->
                                    {String.Value = ls.Value + rs.Value} :> Object , env
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

                    let found, builtIn = builtIns.TryGetValue(i.Value)
                    if found then
                        builtIn :> Object, currentEnv
                    else
                        let error = sprintf "identifier not found: %s" i.Value |> newError
                        error, currentEnv
            | :? FunctionLiteral as fl ->
                {Function.Body = fl.Body; Parameters = fl.Parameters; Env = currentEnv} :> Object, currentEnv
            | :? CallExpression as ce ->

                let modifier (node:Node) env =
                    match node with
                    | :? CallExpression as ce when ce.Function.TokenLiteral() <> "unquote" ->
                        node, env
                    | :? CallExpression as ce when ce.Arguments.Length <> 1 ->
                        node, env
                    | :? CallExpression as ce ->
                        let convertObjectToAstNode (obj:Object) =
                            match obj with
                            | :? Integer as i ->
                                let token = {Token.Type = Token.INT; Token.Literal = (sprintf "%i" i.Value)}
                                {IntegerLiteral.Token = token; Value = i.Value} :> Node
                            | :? Boolean as b ->
                                let token = 
                                    if b.Value then
                                        {Token.Type = Token.TRUE; Token.Literal = "true"}
                                    else
                                        {Token.Type = Token.FALSE; Token.Literal = "false"}
                                {Ast.Boolean.Token = token; Ast.Boolean.Value = b.Value} :> Node
                            | :? Quote as q ->
                                q.Node :> Node
                            | _ ->
                                EmptyStatement() :> Node
                                    
                        let unquoted, newEnv = evalRec ce.Arguments.Head env
                        let unquotedNode = unquoted |> convertObjectToAstNode
                        unquotedNode, newEnv
                    | _ ->
                        node, env

                let evalUnquoteCalls quoted env =
                    modify quoted env modifier

                if ce.Function.TokenLiteral() = "quote" then
                    let node, newEnv = evalUnquoteCalls ce currentEnv
                    {Quote.Node = node} :> Object, newEnv
                else
                    let funcObject, env = evalRec ce.Function currentEnv
                    if isError funcObject then
                        funcObject, env
                    else
                        let args, env = evalExpressions ce.Arguments [] currentEnv
                        match funcObject with
                        | :? Function as fn ->
                            match args with
                            | HasError e ->
                                e, env
                            | _ ->
                                let funcEnvironment = {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = Some(currentEnv)}
                                for i in 0..fn.Parameters.Length-1 do
                                    let argValue = args.[i]
                                    let param = fn.Parameters.[i]
                                    funcEnvironment.Set param.Value argValue |> ignore
                        
                                let evaluated, env = evalRec fn.Body funcEnvironment
                                match evaluated with
                                | :? ReturnValue as rv ->
                                    rv.Value, env
                                | _ ->
                                    evaluated, env
                        | :? BuiltIn as bn ->
                            let evaluated = bn.Fn args
                            evaluated, env
                        | _ ->
                            let errorMessage = sprintf "not a function: %A" funcObject
                            errorMessage |> newError, env
            | :? StringLiteral as sl ->
                {String.Value = sl.Value} :> Object, currentEnv
            | :? ArrayLiteral as al ->
                let elements, env = evalExpressions al.Elements [] currentEnv
                match elements with
                | HasError e ->
                    e, env
                | _ ->
                    {Array.Elements = elements} :> Object, currentEnv
            | :? IndexExpression as ie ->
                let left, env = evalRec ie.Left currentEnv
                if isError left then
                    left, env
                else
                    let index, env = evalRec ie.Index env
                    if isError index then
                        index, env
                    else
                        match left, index with
                        | (:? Array as arr), (:? Integer as i) ->
                            let idx = int32(i.Value)
                            if idx < 0 || idx >= arr.Elements.Length then
                                NULL, env
                            else
                                arr.Elements.[idx], env
                        | (:? Hash as h), _ ->
                            match index with
                            | :? Hashable as key ->
                                let found, pair = h.Pairs.TryGetValue(key.HashKey())
                                if found then
                                    pair.Value, currentEnv
                                else
                                    NULL, currentEnv
                            | _ ->
                                let errorMsg = sprintf "unusable as hash key: %s" (unwrapObjectType index)
                                errorMsg |> newError, currentEnv
                        | _, _ ->
                            let errorMsg = sprintf "index operator not supported: %s" (unwrapObjectType left)
                            errorMsg |> newError, currentEnv
            | :? HashLiteral as h ->
                // todo: fix this, really ugly
                let parseHash() =
                    let pairs = new System.Collections.Generic.Dictionary<HashKey, HashPair>()
                    let bombOuts = new System.Collections.Generic.List<Object>()
                    let errors = new System.Collections.Generic.List<Object>()
                    for kvp in h.Pairs do
                        let key, env = evalRec kvp.Key currentEnv
                        if isError key then
                            bombOuts.Add(key)
                        else
                            match key with
                            | :? Hashable as hsh ->
                                let value, env = evalRec kvp.Value env
                                if isError value then
                                    bombOuts.Add(value)
                                else
                                    let hashed = hsh.HashKey()
                                    pairs.Add(hashed, {HashPair.Key = key; Value = value})
                            | _ ->
                                let errorMsg = sprintf "unusable as hash key: %s" (unwrapObjectType key)
                                let error = errorMsg |> newError
                                errors.Add(error)
                    pairs, bombOuts, errors

                let pairs, bombOuts, errors = parseHash()
                match errors.Count with
                | 0 ->
                    match bombOuts.Count with
                    | 0 ->
                        {Hash.Pairs = pairs} :> Object, currentEnv
                    | _ ->
                        bombOuts.[0], currentEnv
                | _ ->
                    errors.[0], currentEnv
            | _ -> 
                NULL, currentEnv

        let res, _ = evalRec node {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = None}
        res

        