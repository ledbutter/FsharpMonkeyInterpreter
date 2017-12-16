namespace Monkey

open System
open System.IO
open Token
open Lexer2

module Repl =

    [<Literal>]
    let PROMPT = ">> "

    type IPrinter =
        abstract Print : Printf.TextWriterFormat<'a> -> 'a

    let rec start (reader:TextReader) (output:IPrinter) =
        // todo: async!
        Console.Write(PROMPT) |> ignore
        //output.Print ">> " <-- this results in a newline, can I do printfn without a newline?
        let scanned = reader.ReadLine()
        let tokens = tokenizeInput scanned
        for i in 0..tokens.Length-1 do
            output.Print "%A" tokens.[i]
        start reader output    
    
        