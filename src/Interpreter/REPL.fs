namespace Monkey

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
        output.Print ">> "
        let scanned = reader.ReadLine()
        let tokens = tokenizeInput scanned
        for i in 0..tokens.Length-1 do
            output.Print "%A" tokens.[i]
        start reader output