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

    let evaluateProgram p =
        eval p

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
            let evaluated = p |> evaluateProgram
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
            let evaluated = p |> evaluateProgram
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
            let evaluated = p |> evaluateProgram
            assertBooleanObject evaluated expected
        | Errors e ->
            e |> assertErrors


    let ifElseExpressionTestCases() =
        seq {
            yield new TestCaseData("if (true) { 10 }", Some(10))
            yield new TestCaseData("if (false) { 10 }", None)
            yield new TestCaseData("if (1) { 10 }", Some(10))
            yield new TestCaseData("if (1 < 2) { 10 }", Some(10))
            yield new TestCaseData("if (1 > 2) { 10 }", None)
            yield new TestCaseData("if (1 > 2) { 10 } else { 20 }", Some(20))
            yield new TestCaseData("if (1 < 2) { 10 } else { 20 }", Some(10))
        } 

    let assertNullObject (object:Object) =
        match object with
        | :? Null as __ ->
            Assert.Pass("Was null")
        | _ ->
            Assert.Fail((sprintf "Was not null, was %A" object))
        

    [<TestCaseSource("ifElseExpressionTestCases")>]
    let testIfElseExpressions input (expected: int option) =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match expected with
            | Some(i) ->
                assertIntegerObject evaluated i
            | None ->
                assertNullObject evaluated
        | Errors e ->
            e |> assertErrors

    [<TestCase("return 10;", 10)>]
    [<TestCase("return 10; 9;", 10)>]
    [<TestCase("return 2 * 5; 9;", 10)>]
    [<TestCase("9; return 2 * 5; 9;", 10)>]
    [<TestCase(@"if (10 > 1) { 
                    if (10 > 1) { 
                        return 10; 
                    } 
                    return 1; 
                 }", 10)>]
    let testReturnStatements input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            assertIntegerObject evaluated expected
        | Errors e ->
            e |> assertErrors

    [<TestCase("5 + true;", "type mismatch: INTEGER + BOOLEAN")>]
    [<TestCase("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN")>]
    [<TestCase("-true", "unknown operator: -BOOLEAN")>]
    [<TestCase("true + false;", "unknown operator: BOOLEAN + BOOLEAN")>]
    [<TestCase("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN")>]
    [<TestCase("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN")>]
    [<TestCase(@"if (10 > 1) { 
                    if (10 > 1) { 
                        return true + false; 
                    } 
                    return 1; 
                 }", "unknown operator: BOOLEAN + BOOLEAN")>]
    [<TestCase("foobar", "identifier not found: foobar")>]
    let testErrorHandling input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Error as err ->
                Assert.AreEqual(expected, err.Message)
            | _ ->
                let errorMessage = sprintf "Expected error, but was %A" evaluated
                Assert.Fail(errorMessage)
        | Errors e ->
            e |> assertErrors

    [<TestCase("let a = 5; a;", 5)>]
    [<TestCase("let a = 5 * 5; a;", 25)>]
    [<TestCase("let a = 5; let b = a; b;", 5)>]
    [<TestCase("let a = 5; let b = a; let c = a + b + 5; c;", 15)>]
    let testLetStatements input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            assertIntegerObject evaluated expected
        | Errors e ->
            e |> assertErrors