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
    [<TestCase("-5", -5)>]
    [<TestCase("-10", -10)>]
    [<TestCase("5 + 5 + 5 + 5 - 10", 10)>]
    [<TestCase("2 * 2 * 2 * 2 * 2", 32)>]
    [<TestCase("-50 + 100 + -50", 0)>] 
    [<TestCase("5 * 2 + 10", 20)>] 
    [<TestCase("5 + 2 * 10", 25)>] 
    [<TestCase("20 + 2 * -10", 0)>] 
    [<TestCase("50 / 2 * 2 + 10", 60)>] 
    [<TestCase("2 * (5 + 10)", 30)>] 
    [<TestCase("3 * 3 * 3 + 10", 37)>] 
    [<TestCase("3 * (3 * 3) + 10", 37)>] 
    [<TestCase("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)>]
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
    [<TestCase("1 < 2", true)>]
    [<TestCase("1 > 2", false)>]
    [<TestCase("1 < 1", false)>]
    [<TestCase("1 > 1", false)>]
    [<TestCase("1 == 1", true)>]
    [<TestCase("1 != 1", false)>]
    [<TestCase("1 == 2", false)>]
    [<TestCase("1 != 2", true)>]
    [<TestCase("true == true", true)>] 
    [<TestCase("false == false", true)>] 
    [<TestCase("true == false", false)>] 
    [<TestCase("true != false", true)>] 
    [<TestCase("false != true", true)>] 
    [<TestCase("(1 < 2) == true", true)>] 
    [<TestCase("(1 < 2) == false", false)>] 
    [<TestCase("(1 > 2) == true", false)>] 
    [<TestCase("(1 > 2) == false", true)>]
    let testEvalBooleanExpression input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> eval
            assertBooleanObject evaluated expected
        | Errors e ->
            e |> assertErrors

    [<TestCase("!true", false)>]
    [<TestCase("!false", true)>]
    [<TestCase("!5", false)>]
    [<TestCase("!!true", true)>]
    [<TestCase("!!false", false)>]
    [<TestCase("!!5", true)>]
    let testBangOperator input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> eval
            assertBooleanObject evaluated expected
        | Errors e ->
            e |> assertErrors

    //let testIfElseExpressions input expected =
