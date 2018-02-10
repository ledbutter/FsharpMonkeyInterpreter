namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer
open Monkey.Ast

module Parser_Tests =
    let assertLetStatement (statement:Statement) (expectedName:string) =
        Assert.AreEqual("let", statement.TokenLiteral())
        let letStatement = statement :?> LetStatement
        Assert.AreEqual(expectedName, letStatement.Name.Value, "name value")
        Assert.AreEqual(expectedName, letStatement.Name.TokenLiteral(), "name token literal")

    let assertErrors (errors:string list) =
        sprintf "Parser had %i errors" errors.Length |> ignore
        for error in errors do
            sprintf "Parser error: %s" error |> ignore
        Assert.Fail("Parsing failed")

    let assertReturnStatement (statement:Statement) expectedReturnValue =
        let returnStatement = statement :?> ReturnStatement
        Assert.AreEqual("return", returnStatement.TokenLiteral())
        Assert.AreEqual(expectedReturnValue, returnStatement.ReturnValue.TokenLiteral())

    let assertIntegerLiteral (intLiteral:IntegerLiteral) (expectedValue:int64) =
        Assert.AreEqual(expectedValue, intLiteral.Value)
        Assert.AreEqual(expectedValue.ToString(), intLiteral.TokenLiteral())

    let generateResults input =
        let tokens = input |> tokenizeInput
        tokens |> parseProgram

    [<Test>]
    let testLetStatements() =
        let input = @"
            let x = 5;
            let y = 10;
            let foobar = 838383;
            "
        let parserResults = input |> generateResults 
        match parserResults with
        | Errors e -> 
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(3, p.Statements.Length, "Unexpected number of statements")
            let expectedResults = [
                "x"
                "y"
                "foobar"
            ]

            for i in 0..expectedResults.Length-1 do
                let statement = p.Statements.[i]
                assertLetStatement statement (expectedResults.[i])

    [<Test>]
    let testReturnStatements() =
        let input = @"
            return 5;
            return 10;
            return 993322;
            "
        let parserResults = input |> generateResults 
        match parserResults with
        | Errors e -> 
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(3, p.Statements.Length, "Unexpected number of statements")
            let expectedReturnValues = [
                "5"
                "10"
                "993322"
            ]

            for i in 0..expectedReturnValues.Length-1 do
                let statement = p.Statements.[i]
                assertReturnStatement statement (expectedReturnValues.[i])

    [<Test>]
    let testIdentifierExpression() =
        let input = "foobar;"
        let parserResults = input |> generateResults 
        match parserResults with
        | Errors e -> 
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let identifier = expressionStatement.Expression :?> Identifier
            Assert.AreEqual("foobar", identifier.Value)
            Assert.AreEqual("foobar", identifier.TokenLiteral())

    [<Test>]
    let testIntegerLiteralExpression() =
        let input = "5;"
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let literal = expressionStatement.Expression :?> IntegerLiteral
            assertIntegerLiteral literal 5L |> ignore

    [<TestCase("!5", "!", 5)>]
    [<TestCase("-15", "-", 15)>]
    let testParsingPrefixExpressions input operator integerValue =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let prefixExpression = expressionStatement.Expression :?> PrefixExpression
            Assert.AreEqual(operator, prefixExpression.Operator)
            let integerLiteral = prefixExpression.Right :?> IntegerLiteral
            assertIntegerLiteral integerLiteral integerValue |> ignore

    [<TestCase("5 + 5;", 5, "+", 5)>]
    [<TestCase("5 - 5;", 5, "-", 5)>]
    [<TestCase("5 * 5;", 5, "*", 5)>]
    [<TestCase("5 / 5;", 5, "/", 5)>]
    [<TestCase("5 > 5;", 5, ">", 5)>]
    [<TestCase("5 < 5;", 5, "<", 5)>]
    [<TestCase("5 == 5;", 5, "==", 5)>]
    [<TestCase("5 != 5;", 5, "!=", 5)>]
    let testParsingInfixExpressions input expectedLeftValue expectedOperator expectedRightValue =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let infixExpression = expressionStatement.Expression :?> InfixExpression
            let leftValue = infixExpression.Left :?> IntegerLiteral
            assertIntegerLiteral leftValue expectedLeftValue |> ignore
            let rightValue = infixExpression.Right :?> IntegerLiteral
            assertIntegerLiteral rightValue expectedRightValue |> ignore
            Assert.AreEqual(expectedOperator, infixExpression.Operator)

    [<TestCase("true", true)>]
    [<TestCase("false", false)>]
    let testBooleanExpression input expectedResult =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let booleanExpression = expressionStatement.Expression :?> Boolean
            Assert.AreEqual(expectedResult, booleanExpression.Value)

    [<TestCase("-a * b", "((-a) * b)")>]
    [<TestCase("!-a", "(!(-a))")>]
    [<TestCase("a + b + c", "((a + b) + c)")>]
    [<TestCase("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")>]
    [<TestCase("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")>]
    [<TestCase("(5 + 5) * 2", "((5 + 5) * 2)")>]
    [<TestCase("a + add(b * c) + d", "((a + add((b * c))) + d)")>]
    let testOperatorPrecedenceParsing input expectedResult =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(expectedResult, p.ToString())

    let assertInfixExpression (expression:Expression) expectedLeftValue expectedOperator expectedRightValue =
        let infixExpression = expression :?> InfixExpression
        Assert.AreEqual(expectedLeftValue, infixExpression.Left.TokenLiteral(), "left")
        Assert.AreEqual(expectedOperator, infixExpression.Operator, "operator")
        Assert.AreEqual(expectedRightValue, infixExpression.Right.TokenLiteral(), "right")

    [<Test>]
    let testIfExpression() =
        let input = "if (x < y) { x }"
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let ifExpression = expressionStatement.Expression :?> IfExpression
            assertInfixExpression ifExpression.Condition "x" "<" "y"
            Assert.AreEqual(1, ifExpression.Consequence.Statements.Length)
            let consequence = ifExpression.Consequence.Statements.[0] :?> ExpressionStatement
            Assert.AreEqual("x", consequence.TokenLiteral())
            Assert.AreEqual(0, ifExpression.Alternative.Statements.Length)

    [<Test>]
    let testIfElseExpression() =
        let input = "if (x < y) { x } else { y }"
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let ifExpression = expressionStatement.Expression :?> IfExpression
            assertInfixExpression ifExpression.Condition "x" "<" "y"
            Assert.AreEqual(1, ifExpression.Consequence.Statements.Length)
            let consequence = ifExpression.Consequence.Statements.[0] :?> ExpressionStatement
            Assert.AreEqual("x", consequence.TokenLiteral())
            Assert.AreEqual(1, ifExpression.Alternative.Statements.Length)

    [<Test>]
    let testFunctionLiteralParsing() =
        let input = "fn(x, y) { x + y; }"
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.[0] :?> ExpressionStatement
            let functionLiteral = expressionStatement.Expression :?> FunctionLiteral
            Assert.AreEqual(2, functionLiteral.Parameters.Length, "parameter length")
            let parameterOne = functionLiteral.Parameters.[0]
            Assert.AreEqual("x", parameterOne.TokenLiteral())
            let parameterTwo = functionLiteral.Parameters.[1]
            Assert.AreEqual("y", parameterTwo.TokenLiteral())
            Assert.AreEqual(1, functionLiteral.Body.Statements.Length)
            let bodyStatement = functionLiteral.Body.Statements.[0] :?> ExpressionStatement
            assertInfixExpression bodyStatement.Expression "x" "+" "y"

    [<Test>]
    let testFunctionParameterParsing() =
        // can't do arrays as actual TestCase members apparently
        let testCases = [
            "fn() {};", []
            "fn(x) {};", ["x"]
            "fn(x, y, z) {};", ["x"; "y"; "z"]
        ]

        for i in 0..testCases.Length-1 do
            let input, expectedParams = testCases.[i]
            let parserResults = input |> generateResults
            match parserResults with
            | Errors e ->
                e |> assertErrors
            | Program p ->
                Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
                let statement = p.Statements.[0] :?> ExpressionStatement
                let functionLiteral = statement.Expression :?> FunctionLiteral
                Assert.AreEqual(expectedParams.Length, functionLiteral.Parameters.Length, "param count off")
                for p in 0..expectedParams.Length-1 do
                    Assert.AreEqual(expectedParams.[p], functionLiteral.Parameters.[p].TokenLiteral())

    [<Test>]
    let testCallExpressionParsing() =
        let input = "add(1, 2 * 3, 4 + 5);"
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let statement = p.Statements.[0] :?> ExpressionStatement
            let callExpression = statement.Expression :?> CallExpression
            Assert.AreEqual("add", callExpression.Function.TokenLiteral())
            Assert.AreEqual(3, callExpression.Arguments.Length)
            assertIntegerLiteral (callExpression.Arguments.[0] :?> IntegerLiteral) 1L
            assertInfixExpression callExpression.Arguments.[1] "2" "*" "3"
            assertInfixExpression callExpression.Arguments.[2] "4" "+" "5"