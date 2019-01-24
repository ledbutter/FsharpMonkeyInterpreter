namespace Monkey

open Ast
open Object

module Macro_Expansion =
    
    let defineMacros (program:Program) env =
        
        let isMacroDefinition node =
            match node with
            | :? LetStatement as ls ->
                match ls.Value with
                | :? MacroLiteral as ml ->
                    true
                | _ ->
                    false
            | _ ->
                false

        let addMacro statement = 
            let ls = statement :?> LetStatement
            let macroLiteral = ls.Value :?> MacroLiteral

            let macro = {Macro.Parameters = macroLiteral.Parameters; Env = env; Body = macroLiteral.Body}

            env.Set(ls.Name.Value, macro)

        let rec buildDefinitions currentStatements definitions index =
            match currentStatements with
            | x::xs ->
                let newDefinitions = match isMacroDefinition x with
                | true ->
                    addMacro x
                    index :: definitions
                | false ->
                    definitions
                buildDefinitions xs newDefinitions index + 1
            | _ ->
                // this will be in reverse order, allowing us to go through it without starting at the end
                definitions

        //let definitions = buildDefinitions program.Statements [] 0
//
//        let rec removeMacroStatements defs currentStatements =
//            match defs with
//            | x::xs ->
                

                //let newStatements = currentStatements.Skip(x).Take(currentStatements.Length - x).Concat(currentStatements.[])

