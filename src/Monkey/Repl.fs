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

    let start (reader:TextReader) (output:IPrinter) =
        
        let env = Monkey.Object.newEmptyEnv()
        let macroEnv =  Monkey.Object.newEmptyEnv()

        let rec startRec currentEnv currentMacroEnv =
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
                    startRec currentEnv currentMacroEnv
                | Program p ->
                    let mp, met = defineMacros p (Some(currentMacroEnv))
                    let expanded, me = expandMacros mp met

                    let evalEnv = Some(currentEnv)
                    let evaluated, e = eval evalEnv expanded
                    output.Print "%s" (evaluated.Inspect())
                    startRec e me
            | _ -> 
                currentEnv, currentMacroEnv

        startRec env macroEnv