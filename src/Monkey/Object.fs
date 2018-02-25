namespace Monkey

module Object =
    open System.Collections.Generic
    open System

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

    type Object =
        abstract member Type: unit -> ObjectType
        abstract member Inspect: unit -> string

    type Integer = 
        { 
            Value: int64 
        }
        interface Object with
            member this.Inspect() =
                sprintf "%d" this.Value
            member __.Type() =
                ObjectTypes.INTEGER_OBJ

    type Boolean =
        {
            Value: bool
        }
        interface Object with
            member this.Inspect() =
                sprintf "%b" this.Value
            member __.Type() =
                ObjectTypes.BOOLEAN_OBJ

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
            Store: Dictionary<string, Object>
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
        interface Object with
            member this.Inspect() =
                sprintf "%s" this.Value
            member __.Type() =
                ObjectTypes.STRING_OBJ

    type BuiltIn = 
        {
            Fn: Object list -> Object
        }
        interface Object with
            member __.Inspect() =
                sprintf "builtin function"
            member __.Type() =
                ObjectTypes.BUILTIN_OBJ
    