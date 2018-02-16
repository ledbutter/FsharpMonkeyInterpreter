namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer
open Monkey.Object
open Monkey.Evaluator

module Evaluator_Tests =

    let assertErrors (errors:string list) =
        sprintf "Parser had %i errors" errors.Length |> ignore
        for error in errors do
            sprintf "Parser error: %s" error |> ignore
        Assert.Fail("Parsing failed")
    
    let generateProgram input =
        input
        |> tokenizeInput
        |> parseProgram

    let assertIntegerObject (object:Object) expected =
        let integerObject = object :?> Integer
        Assert.AreEqual(expected, integerObject.Value)

    [<TestCase("5", 5)>]
    [<TestCase("10", 10)>]
    let testEvalIntegerExpression input (expected:int64) =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> eval
            assertIntegerObject evaluated expected
        | Errors e ->
            e |> assertErrors

    let assertBooleanObject (object:Object) expected =
        let booleanObject = object :?> Boolean
        Assert.AreEqual(expected, booleanObject.Value)

    [<TestCase("true", true)>]
    [<TestCase("false", false)>]
    let testEvalBooleanExpression input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> eval
            assertBooleanObject evaluated expected
        | Errors e ->
            e |> assertErrors
