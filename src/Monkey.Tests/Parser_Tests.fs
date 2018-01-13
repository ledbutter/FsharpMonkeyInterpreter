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
        let parserResults = tokens |> parseProgram
        match parserResults with
        | Errors e -> Assert.Fail("Should not fail")
        | Statements s -> 
            Assert.AreEqual(3, s.Length, "Unexpected number of statements")
            let expectedResults = [
                "x"
                "y"
                "foobar"
            ]

            let statements = s
            for i in 0..expectedResults.Length-1 do
                let statement = s.Item(i)
                testLetStatement statement (expectedResults.Item(i)) |> ignore
    