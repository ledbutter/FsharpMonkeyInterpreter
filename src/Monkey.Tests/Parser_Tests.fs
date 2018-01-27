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

    let assertErrors (errors:string list) =
        sprintf "Parser had %i errors" errors.Length |> ignore
        for error in errors do
            sprintf "Parser error: %s" error |> ignore
        Assert.Fail("Parsing failed")

    let testReturnStatement (statement:Statement) expectedReturnValue =
        let returnStatement = statement :?> ReturnStatement
        Assert.AreEqual("return", returnStatement.TokenLiteral())

    let testIntegerLiteral (intLiteral:IntegerLiteral) (expectedValue:int64) =
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
                let statement = p.Statements.Item(i)
                testLetStatement statement (expectedResults.Item(i)) |> ignore

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
                let statement = p.Statements.Item(1)
                testReturnStatement statement (expectedReturnValues.Item(1)) |> ignore

    [<Test>]
    let testIdentifierExpression() =
        let input = "foobar;"
        let parserResults = input |> generateResults 
        match parserResults with
        | Errors e -> 
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.Item(0) :?> ExpressionStatement
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
            let expressionStatement = p.Statements.Item(0) :?> ExpressionStatement
            let literal = expressionStatement.Expression :?> IntegerLiteral
            testIntegerLiteral literal 5L |> ignore

    [<TestCase("!5", "!", 5)>]
    [<TestCase("-15", "-", 15)>]
    let testParsingPrefixExpressions input operator integerValue =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p -> 
            Assert.AreEqual(1, p.Statements.Length, "Unexpected number of statements")
            let expressionStatement = p.Statements.Item(0) :?> ExpressionStatement
            let prefixExpression = expressionStatement.Expression :?> PrefixExpression
            Assert.AreEqual(operator, prefixExpression.Operator)
            let integerLiteral = prefixExpression.Right :?> IntegerLiteral
            testIntegerLiteral integerLiteral integerValue |> ignore

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
            let expressionStatement = p.Statements.Item(0) :?> ExpressionStatement
            let infixExpression = expressionStatement.Expression :?> InfixExpression
            let leftValue = infixExpression.Left :?> IntegerLiteral
            testIntegerLiteral leftValue expectedLeftValue |> ignore
            let rightValue = infixExpression.Right :?> IntegerLiteral
            testIntegerLiteral rightValue expectedRightValue |> ignore
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
            let expressionStatement = p.Statements.Item(0) :?> ExpressionStatement
            let booleanExpression = expressionStatement.Expression :?> Boolean
            Assert.AreEqual(expectedResult, booleanExpression.Value)

    //[<TestCase("-a * b", "((-a) * b)")>]
    //[<TestCase("!-a", "(!(-a))")>]
    //[<TestCase("a + b + c", "((a + b) + c)")>]
    [<TestCase("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")>]
    //[<TestCase("1 + ( 2 + 3) + 4", "((1 + (2 + 3)) + 4)")>]
    //[<TestCase("(5 + 5) * 2", "((5 + 5) * 2)")>]
    let testOperatorPrecedenceParsing input expectedResult =
        let parserResults = input |> generateResults
        match parserResults with
        | Errors e ->
            e |> assertErrors
        | Program p ->
            Assert.AreEqual(expectedResult, p.ToString())