namespace Monkey

module Object =
    type ObjectType = ObjectType of string

    module ObjectTypes =
        let INTEGER_OBJ = "INTEGER" |> ObjectType
        let BOOLEAN_OBJ = "BOOLEAN" |> ObjectType
        let NULL_OBJ = "NULL" |> ObjectType
        let RETURN_VALUE_OBJ = "RETURN_VALUE" |> ObjectType
        let ERROR_OBJ = "ERROR" |> ObjectType

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