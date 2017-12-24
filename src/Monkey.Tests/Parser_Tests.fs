namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer2
open Monkey.Ast

module Parser_Tests =

    let testLetStatement (statement:Statement) (expectedName:string) =
        Assert.AreEqual("let", statement.TokenLiteral())
        let letStatement = statement :?> LetStatement
        Assert.AreEqual(expectedName, letStatement.Name.Value, "name value")
        Assert.AreEqual(expectedName, letStatement.Name.TokenLiteral(), "name token literal")

    [<Test>]
    let testLetStatements() =
        let input = @"
            let x = 5;
            let y = 10;
            let foobar = 838383;
            "
        let tokens = input |> tokenizeInput
        let statements = tokens |> parseProgram
        Assert.AreEqual(3, statements.Length, "Statement Length")

        let expectedResults = [
            "x"
            "y"
            "foobar"
        ]

        for i in 0..expectedResults.Length-1 do
            let statement = statements.Item(i)
            testLetStatement statement (expectedResults.Item(i)) |> ignore

    