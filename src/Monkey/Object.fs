namespace Monkey

module Object =
    open System
    open Ast

    type ObjectType = ObjectType of string

    module ObjectTypes =
        let INTEGER_OBJ = "INTEGER" |> ObjectType
        let BOOLEAN_OBJ = "BOOLEAN" |> ObjectType
        let NULL_OBJ = "NULL" |> ObjectType
        let RETURN_VALUE_OBJ = "RETURN_VALUE" |> ObjectType
        let ERROR_OBJ = "ERROR" |> ObjectType
        let FUNCTION_OBJ = "FUNCTION" |> ObjectType
        let STRING_OBJ = "STRING" |> ObjectType
        let BUILTIN_OBJ = "BUILTIN" |> ObjectType
        let ARRAY_OBJ = "ARRAY" |> ObjectType
        let HASH_OBJ = "HASH" |> ObjectType
        let QUOTE_OBJ = "QUOTE" |> ObjectType
        let MACRO_OBJ = "MACRO" |> ObjectType

    type Object =
        abstract member Type: unit -> ObjectType
        abstract member Inspect: unit -> string

    type HashKey =
        {
            Type: ObjectType
            Value: int64
        }

    type Hashable =
        inherit Object
        abstract member HashKey: unit -> HashKey

    type HashPair =
        {
            Key: Object
            Value: Object
        }

    type Hash =
        {
            Pairs: System.Collections.Generic.Dictionary<HashKey, HashPair>
        }
        interface Object with
            member this.Inspect() =
                let pairValues =
                    this.Pairs
                    |> Seq.map(fun kvp -> (kvp.Value.Key.Inspect()) + ":" + (kvp.Value.Value.Inspect()))
                    |> fun x -> x |> String.concat ", "
                
                sprintf "{%s}" pairValues
            
            member __.Type() =
                ObjectTypes.HASH_OBJ

    type Integer = 
        { 
            Value: int64 
        }
        interface Hashable with
            member this.Inspect() =
                sprintf "%d" this.Value
            member __.Type() =
                ObjectTypes.INTEGER_OBJ
            member this.HashKey() =
                {HashKey.Type = ObjectTypes.INTEGER_OBJ; Value = this.Value}

    type Boolean =
        {
            Value: bool
        }
        interface Hashable with
            member this.Inspect() =
                sprintf "%b" this.Value
            member __.Type() =
                ObjectTypes.BOOLEAN_OBJ
            member this.HashKey() =
                let hashValue = 
                    match this.Value with
                    | true -> 1L
                    | false -> 0L

                {HashKey.Type = ObjectTypes.BOOLEAN_OBJ; Value = hashValue}

    type Null =
        interface Object with
            member __.Inspect() =
                sprintf "null"
            member __.Type() =
                ObjectTypes.NULL_OBJ
        new() = {}

    type ReturnValue =
        {
            Value: Object
        }
        interface Object with
            member this.Inspect() =
                sprintf "%s" (this.Value.Inspect())
            member __.Type() =
                ObjectTypes.RETURN_VALUE_OBJ

    type Error =
        {
            Message: string
        }
        interface Object with
            member this.Inspect() =
                sprintf "%s" this.Message
            member __.Type() =
                ObjectTypes.ERROR_OBJ

    type Environment =
        {
            Store: System.Collections.Generic.Dictionary<string, Object>
            Outer: Environment option
        }
        member this.Get name =
           let exists, value = this.Store.TryGetValue(name)
           if exists then
            Some(value)
           else
            match this.Outer with
            | Some(e) ->
                let outerExists, outerValue = e.Store.TryGetValue(name)
                if outerExists then
                    Some(outerValue)
                else
                    None
            | None ->
                None
        member this.Set name value =
            this.Store.[name] <- value
            value

    type Function =
        {
            Parameters: Ast.Identifier list
            Body: Ast.BlockStatement
            Env: Environment
        }
        interface Object with
            member this.Inspect() =
                let parameterValues = 
                    this.Parameters
                    |> Seq.map(fun s -> s.ToString())
                    |> fun x -> x |> String.concat ", "
                
                sprintf "fn (%s) {%s %s %s}" parameterValues Environment.NewLine (this.Body.ToString()) Environment.NewLine

            member __.Type() =
                ObjectTypes.FUNCTION_OBJ

    type String = 
        { 
            Value: string 
        }
        interface Hashable with
            member this.Inspect() =
                sprintf "%s" this.Value
            member __.Type() =
                ObjectTypes.STRING_OBJ
            member this.HashKey() =
                {HashKey.Type = ObjectTypes.STRING_OBJ; Value = int64((this.Value.GetHashCode()))}

    type BuiltIn = 
        {
            Fn: Object list -> Object
        }
        interface Object with
            member __.Inspect() =
                sprintf "builtin function"
            member __.Type() =
                ObjectTypes.BUILTIN_OBJ

    type Array =
        {
            Elements: Object list
        }
        interface Object with
            member x.Inspect() =
                let parameterValues = 
                    x.Elements
                    |> Seq.map(fun s -> s.Inspect())
                    |> fun x -> x |> String.concat ", "

                sprintf "[%s]" parameterValues
            member __.Type() =
                ObjectTypes.ARRAY_OBJ

    type Quote =
        {
            Node: Node
        }
        interface Object with
            member x.Inspect() =
                sprintf "QUOTE(%s)" (x.Node.ToString())
            member __.Type() =
                ObjectTypes.QUOTE_OBJ

    type Macro =
        {
            Parameters: Identifier list
            Body: BlockStatement
            Env: Environment
        }
        interface Object with
            member this.Inspect() =
                let parameterValues = 
                    this.Parameters
                    |> Seq.map(fun s -> s.ToString())
                    |> fun x -> x |> String.concat ", "
                
                sprintf "macro (%s) {%s %s %s}" parameterValues Environment.NewLine (this.Body.ToString()) Environment.NewLine

            member __.Type() =
                ObjectTypes.MACRO_OBJ

    let newEnv outer =
        {Environment.Store = new System.Collections.Generic.Dictionary<string, Object>(); Outer = outer}

    let newEmptyEnv() =
        newEnv None