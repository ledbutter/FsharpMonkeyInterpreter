﻿namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer
open Monkey.Object
open Monkey.Evaluator
open Monkey.Ast

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

    let assertIntegerObject (obj:Object) expected =
        match obj with
        | :? Integer as i ->
            Assert.AreEqual(expected, i.Value)
        | :? Error as e ->
            Assert.Fail(e.Message)
        | _ ->
            sprintf "Unexpected object %A" obj |> ignore
            Assert.Fail("Wrong object type")

    let evaluateProgram p =
        let o, _ = eval None p
        o

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

    let assertBooleanObject (obj:Object) expected =
        let booleanObject = obj :?> Monkey.Object.Boolean
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

    let assertNullObject (obj:Object) =
        match obj with
        | :? Null as __ ->
            Assert.Pass("Was null")
        | _ ->
            Assert.Fail((sprintf "Was not null, was %A" obj))
        

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
    [<TestCase(@"{""name"": ""Monkey""}[fn(x) { x }];", "unusable as hash key: FUNCTION")>]
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

    [<Test>]
    let testPuts() =
        let programResult = generateProgram @"puts(""hello"", ""world!"")"
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Null as n -> Assert.Pass()
            | _ -> Assert.Fail("Expected Null!")
        | Errors e ->
            e |> assertErrors

    type IntOrStringOrNull = I of int | S of string | A of int[]

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

    [<Test>]
    let testHashLiterals() =
        let input = @"let two = ""two"";
        {
            ""one"": 10 - 9,
            two: 1 + 1,
            ""thr"" + ""ee"": 6/2,
            4: 4,
            true: 5,
            false: 6
        }"
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Hash as h ->
                let expected = dict [ ({String.Value = "one"} :> Hashable).HashKey(), 1L;
                                      ({String.Value = "two"} :> Hashable).HashKey(), 2L;
                                      ({String.Value = "three"} :> Hashable).HashKey(), 3L;
                                      ({Integer.Value = 4L} :> Hashable).HashKey(), 4L;
                                      ({Monkey.Object.Boolean.Value = true} :> Hashable).HashKey(), 5L;
                                      ({Monkey.Object.Boolean.Value = false} :> Hashable).HashKey(), 6L;]
                Assert.AreEqual(expected.Count, h.Pairs.Count)
                for kvp in expected do
                    let found, pair = h.Pairs.TryGetValue(kvp.Key)
                    Assert.IsTrue(found)
                    assertIntegerObject pair.Value kvp.Value
            | _ -> 
                Assert.Fail(sprintf "Expected a hash, but got a %A" evaluated)
        | Errors e ->
            e |> assertErrors

    let hashIndexExpressionTestCases() =
        seq {
            yield new TestCaseData(@"{""foo"": 5}[""foo""]", Some(5L))
            yield new TestCaseData(@"{""foo"": 5}[""bar""]", None)
        }

    [<TestCaseSource("hashIndexExpressionTestCases")>]
    let testHashIndexExpressions input (expected: int64 option) =
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

    [<TestCase("quote(5)", "5")>]
    [<TestCase("quote(5 + 8)", "(5 + 8)")>]
    [<TestCase("quote(foobar)", "foobar")>]
    [<TestCase("quote(foobar + barfoo)", "(foobar + barfoo)")>]
    let testQuote input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Quote as q ->
                Assert.AreEqual(expected, q.Node.ToString())
            | _ ->
                  Assert.Fail(sprintf "Expected a quote, but got a %A" evaluated)  
        | Errors e ->
            e |> assertErrors

    [<TestCase("quote(unquote(4))", "4")>]
    [<TestCase("quote(unquote(4 + 4))", "8")>]
    [<TestCase("quote(8 + unquote(4 + 4))", "(8 + 8)")>]
    [<TestCase("quote(unquote(4 + 4) + 8)", "(8 + 8)")>]
    [<TestCase("let foobar = 8; quote(foobar)", "foobar")>]
    [<TestCase("let foobar = 8; quote(unquote(foobar))", "8")>]
    [<TestCase("quote(unquote(true))", "true")>]
    [<TestCase("quote(unquote(true == false))", "false")>]
    [<TestCase("quote(unquote(quote(4 + 4)))", "(4 + 4)")>]
    [<TestCase("let q = quote(4 + 4); quote(unquote(4 + 4) + unquote(q))", "(8 + (4 + 4))")>]
    let testQuoteUnquote input expected =
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let evaluated = p |> evaluateProgram
            match evaluated with
            | :? Quote as q ->
                Assert.AreEqual(expected, q.Node.ToString())
            | _ ->
                  Assert.Fail(sprintf "Expected a quote, but got a %A" evaluated)  
        | Errors e ->
            e |> assertErrors

    [<Test>]
    let testDefineMacros() =
        let input = @"
            let number = 1;
            let function = fn(x, y) { x + y};
            let mymacro = macro(x, y) { x + y };"
        let programResult = generateProgram input
        match programResult with
        | Program p -> 
            let p', env = p |> defineMacros <| None
            Assert.AreEqual(2, p'.Statements.Length)
            match env.Get("mymacro") with
            | Some obj ->
                match obj with
                | :? Macro as m ->
                    Assert.AreEqual(2, m.Parameters.Length)
                    Assert.AreEqual("(x + y)", m.Body.ToString())
                | _ ->
                    Assert.Fail("Wrong type, fool!")
            | None ->
                Assert.Fail("Unable to find macro!")
        | Errors e ->
            e |> assertErrors

    [<TestCase("let infixExpression = macro() { quote(1 + 2); }; infixExpression();", "(1 + 2)")>]
    [<TestCase("let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); }; reverse(2 + 2, 10 - 5);", "(10 - 5) - (2 + 2)")>]
    [<TestCase(@"
            let unless = macro(condition, consequence, alternative) {
                quote(if (!(unquote(condition))) {
                    unquote(consequence);
                } else {
                    unquote(alternative);
                });
            };

            unless(10 > 5, puts(""not greater""), puts(""greater""));
        ", @"if (!(10 > 5)) { puts(""not greater"") } else { puts(""greater"") }")>]
    let testExpandMacros input expected =
        let programResult = generateProgram input
        let expectedResult = generateProgram expected

        let expectedString =
            match expectedResult with
            | Program p ->
                p.ToString()
            | _ ->
                "Bad"

        match programResult with
        | Program p -> 
            let p', env = p |> defineMacros <| None
            let expanded, _ = expandMacros p' env

            Assert.AreEqual(expectedString, expanded.ToString())
        | _ ->
            Assert.Fail("Not a program!")
