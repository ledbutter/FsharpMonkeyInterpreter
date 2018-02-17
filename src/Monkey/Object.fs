﻿namespace Monkey

module Object =
    type ObjectType = ObjectType of string

    module ObjectTypes =
        let INTEGER_OBJ = "INTEGER" |> ObjectType
        let BOOLEAN_OBJ = "BOOLEAN" |> ObjectType
        let NULL_OBJ = "NULL" |> ObjectType

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