namespace Monkey

open System
open System.IO
open Lexer
open Parser

module Repl =

    [<Literal>]
    let PROMPT = ">> "

    type IPrinter =
        abstract Print : Printf.TextWriterFormat<'a> -> 'a

    let rec start (reader:TextReader) (output:IPrinter) =
        // todo: async!
        Console.Write(PROMPT) |> ignore
        //output.Print ">> " <-- this results in a newline, can I do printfn without a newline?

        let parserOutput = reader.ReadLine() |> tokenizeInput |> parseProgram

        match parserOutput with
        | Errors e -> 
            for i in 0..e.Length-1 do
                output.Print "%A" e.[i]
        | Program p ->
            output.Print "%A" (p.ToString())

        start reader output    
    
        