namespace Monkey

open System
open System.IO
open Lexer
open Parser
open Evaluator

module Repl =

    [<Literal>]
    let PROMPT = ">> "

    type IPrinter =
        abstract Print : Printf.TextWriterFormat<'a> -> 'a

    let rec start (reader:TextReader) (output:IPrinter) =
        // todo: async!
        Console.Write(PROMPT) |> ignore
        //output.Print ">> " <-- this results in a newline, can I do printfn without a newline?

        let (|NotNull|_|) value =
            if obj.ReferenceEquals(value, null) then None
            else Some()

        let input = reader.ReadLine()
        match input with
        | NotNull ->
            let parserOutput = input |> tokenizeInput |> parseProgram

            match parserOutput with
            | Errors e -> 
                for i in 0..e.Length-1 do
                    output.Print "%A" e.[i]
            | Program p ->
                let evaluated = eval p
                output.Print "%s" (evaluated.Inspect())

            start reader output    
        | _ -> ignore