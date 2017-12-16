namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer2

module Parser_Tests =

    [<Test>]
    let testLetStatements() =
        let input = @"
            let x = 5;
            let y = 10;
            let foobar = 838383;
            "
        let tokens = input |> tokenizeInput
        let program = tokens |> parseProgram
        Assert.AreEqual(3, program.Statements.Length, "Statement Length")

