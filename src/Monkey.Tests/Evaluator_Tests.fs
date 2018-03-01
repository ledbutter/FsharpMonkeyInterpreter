namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer
open Monkey.Object
open Monkey.Evaluator

module Evaluator_Tests =
    open Monkey.Ast

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
        match object with
        | :? Integer as i ->
            Assert.AreEqual(expected, i.Value)
        | :? Error as e ->
            Assert.Fail(e.Message)
        | _ ->
            sprintf "Unexpected object %A" object |> ignore
            Assert.Fail("Wrong object type")

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
        let booleanObject = object :?> Monkey.Object.Boolean
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
    [<TestCase(@"""Hello"" - ""World""", "unknown operator: STRING - STRING")>]
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

    [<Test>]
    let testFunctionObject() =
        let input = "fn(x) { x + 2; };"
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            let functionObject = evaluated :?> Function
            Assert.AreEqual(1, functionObject.Parameters.Length)
            Assert.AreEqual("x", functionObject.Parameters.[0].ToString())
            Assert.AreEqual("(x + 2)", functionObject.Body.ToString())
        | Errors e ->
            e |> assertErrors

    [<TestCase("let identity = fn(x) { x; }; identity(5);", 5)>] 
    [<TestCase("let identity = fn(x) { return x; }; identity(5);", 5)>] 
    [<TestCase("let double = fn(x) { x * 2; }; double(5);", 10)>] 
    [<TestCase("let add = fn(x, y) { x + y; }; add(5, 5);", 10)>] 
    [<TestCase("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)>] 
    [<TestCase("fn(x) { x; }(5)", 5)>]
    let testFunctionApplication input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            assertIntegerObject evaluated expected
        | Errors e ->
            e |> assertErrors

    [<Test>]
    let testClosures() =
        let input = @"
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            
            let addTwo = newAdder(2);
            addTwo(2);"
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            assertIntegerObject evaluated 4
        | Errors e ->
            e |> assertErrors

    [<Test>]
    let testStringLiteral() =
        let input = @"""Hello World!"""
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            let stringLiteral = evaluated :?> String
            Assert.AreEqual("Hello World!", stringLiteral.Value)
        | Errors e ->
            e |> assertErrors

    [<Test>]
    let testStringConcatenation() =
        let input = @"""Hello"" + "" "" + ""World!"""
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            let stringLiteral = evaluated :?> String
            Assert.AreEqual("Hello World!", stringLiteral.Value)
        | Errors e ->
            e |> assertErrors

    type IntOrString = I of int | S of string | A of int[]

    let builtInFunctionTestCases() =
        seq {
            yield new TestCaseData(@"len("""")", Some(I 0))
            yield new TestCaseData(@"len(""four"")", Some(I 4))
            yield new TestCaseData(@"len(""hello world"")", Some(I 11))
            yield new TestCaseData(@"len(1)", Some(S @"argument to ""len"" not supported, got INTEGER"))
            yield new TestCaseData(@"len(""one"", ""two"")", Some(S "wrong number of arguments. got=2, want=1"))
            yield new TestCaseData("len([1, 2, 3])", Some(I 3))
            yield new TestCaseData("len([])", Some(I 0))
            yield new TestCaseData("first([1, 2, 3])", Some(I 1))
            yield new TestCaseData("first([])", None)
            yield new TestCaseData("first(1)", Some(S @"argument to ""first"" must be ARRAY, got INTEGER"))
            yield new TestCaseData("last([1, 2, 3])", Some(I 3))
            yield new TestCaseData("last([])", None)
            yield new TestCaseData("last(1)", Some(S @"argument to ""last"" must be ARRAY, got INTEGER"))
            yield new TestCaseData("rest([1, 2, 3])", Some(A [|2; 3|]))
            yield new TestCaseData("rest([])", None)
            yield new TestCaseData("push([], 1)", Some(A [|1|]))
            yield new TestCaseData("push(1, 1)", Some(S @"argument to ""push"" must be ARRAY, got INTEGER"))
        }

    [<TestCaseSource("builtInFunctionTestCases")>]
    let testBuiltInFunctions input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match expected with
            | Some e ->
                match e with
                | I i ->
                    assertIntegerObject evaluated i
                | S s ->
                    match evaluated with
                    | :? Monkey.Object.Error as e ->
                        Assert.AreEqual(s, e.Message)
                    | _ ->
                        let errorMessage = sprintf "Expected error, got %A" evaluated
                        Assert.Fail(errorMessage)
                | A a ->
                    let arr = evaluated :?> Array
                    Assert.AreEqual(a.Length, arr.Elements.Length)
                    for i in 0..a.Length - 1 do
                        assertIntegerObject arr.Elements.[i] a.[i]
            | None ->
                assertNullObject evaluated
        | Errors e ->
            e |> assertErrors

    [<Test>]
    let testArrayLiterals() =
        let input = "[1, 2 * 2, 3 + 3]"
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Array as a ->
                Assert.AreEqual(3, a.Elements.Length)
                assertIntegerObject a.Elements.[0] 1L
                assertIntegerObject a.Elements.[1] 4L
                assertIntegerObject a.Elements.[2] 6L
            | _ ->
                Assert.Fail(sprintf "Expected array literal, got %A" evaluated)
        | Errors e ->
            e |> assertErrors

    
    let arrayIndexExpressionTestCases() =
        seq {
            yield new TestCaseData("[1, 2, 3][0]", Some(1L))
            yield new TestCaseData("[1, 2, 3][1]", Some(2L))
            yield new TestCaseData("[1, 2, 3][2]", Some(3L))
            yield new TestCaseData("let myArray = [1, 2, 3]; myArray[2]", Some(3L))
            yield new TestCaseData("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]", Some(6L))
            yield new TestCaseData("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Some(2L))
            yield new TestCaseData("[1, 2, 3][3]", None)
            yield new TestCaseData("[1, 2, 3][-1]", None)
        }
    
    [<TestCaseSource("arrayIndexExpressionTestCases")>]
    let testArrayIndexExpressions input (expected: int64 option) =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match expected with
            | Some i ->
                assertIntegerObject evaluated i
            | None ->
                assertNullObject evaluated
        | Errors e ->
            e |> assertErrors