﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Monkey.Repl

[<EntryPoint>]
let main _ = 
    
    printfn "Hello! Welcome to the Monkey Programming Language!"
    printfn "Feel free to type in commands\n"

    let output = {new IPrinter with member this.Print(s) = printfn s}
    
    start stdin (output) |> ignore

    0 // return an integer exit code
