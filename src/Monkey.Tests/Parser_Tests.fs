﻿namespace Tests

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

    let assertErrors (errors:string list) =
        sprintf "Parser had %i errors" errors.Length |> ignore
        for error in errors do
            sprintf "Parser error: %s" error |> ignore
        Assert.Fail("Parsing failed")

    let testReturnStatement (statement:Statement) expectedReturnValue =
        let returnStatement = statement :?> ReturnStatement
        Assert.AreEqual("return", returnStatement.TokenLiteral())


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
        | Errors e -> 
            e |> assertErrors
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

    [<Test>]
    let testReturnStatements() =
        let input = @"
            return 5;
            return 10;
            return 993322;
            "
        let tokens = input |> tokenizeInput
        let parserResults = tokens |> parseProgram
        match parserResults with
        | Errors e -> 
            e |> assertErrors
        | Statements s -> 
            Assert.AreEqual(3, s.Length, "Unexpected number of statements")
            let expectedReturnValues = [
                "5"
                "10"
                "993322"
            ]

            for i in 0..expectedReturnValues.Length-1 do
                let statement = s.Item(1)
                testReturnStatement statement (expectedReturnValues.Item(1)) |> ignore
    