namespace Monkey

open System.IO
open Token
open Lexer2

module Repl =

    [<Literal>]
    let PROMPT = ">> "

    let start (reader:TextReader) (writer:TextWriter) =
        let rec startRec =
            // todo: async!
            writer.Write(PROMPT) |> ignore
            let scanned = reader.ReadLine()
            let tokens = tokenizeInput scanned
            for i in 0..tokens.Length-1 do
                writer.WriteLine(tokens.[i])
        startRec