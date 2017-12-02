// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Monkey.Repl

[<EntryPoint>]
let main argv = 
    
    printfn "Hello! Welcome to the Monkey Programming Language!"
    printfn "Feel free to type in commands\n"
    
    start stdin stdout

    0 // return an integer exit code
