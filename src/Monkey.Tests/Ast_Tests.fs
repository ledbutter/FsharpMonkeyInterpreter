namespace Tests

open NUnit.Framework
open Monkey.Ast
open Monkey.Token
open Monkey.Object
open Monkey.Modifier

module Ast_Tests =
    
    let dummyToken = {Token.Literal = "foo"; Type = INT}

    let dummyEnvironment = {Environment.Outer = None; Store = new System.Collections.Generic.Dictionary<string, Object>()}

    [<Test>]
    let testString() =
        let letStatement = {
            Token = {Type = LET; Literal = "let"}
            Name = {Token = {Type = IDENT; Literal = "myVar"}; Value = "myVar"}
            Value = {Identifier.Token = {Type = IDENT; Literal = "anotherVar"}; Value = "anotherVar"}
        }
        let program = {Statements = [letStatement]; Token = dummyToken}
        Assert.AreEqual("let myVar = anotherVar;", program.ToString())

    let one() =
        {IntegerLiteral.Value = 1L; Token = dummyToken} :> Expression
    let two() =
        {IntegerLiteral.Value = 2L; Token = dummyToken} :> Expression

    let turnOneIntoTwo (node :Node) env =
        match node with
        | :? IntegerLiteral as il ->
            if il.Value = 1L then
                {il with Value = 2L} :> Node, env
            else
                {il with Value = 1L} :> Node, env
        | _ ->
            node, env

    let runModify node =
        let result, _ = modify node dummyEnvironment turnOneIntoTwo
        result
   
    [<Test>]
    let testModifySimple() =
        let result = runModify (one())
        //let result, _ = modify (one()) dummyEnvironment turnOneIntoTwo
        match result with
        | :? IntegerLiteral as il ->
            Assert.AreEqual(2, il.Value)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyComplex() =
        let input = {Program.Statements = [{ExpressionStatement.Expression = one(); Token = dummyToken}]}
        let expected = {Program.Statements = [{ExpressionStatement.Expression = two(); Token = dummyToken}]}

        let result = runModify input
        //let result, _ = modify input dummyEnvironment turnOneIntoTwo

        match result with
        | :? Program as p ->
            let actualStatement = p.Statements.[0] :?> ExpressionStatement
            let actualIntegerLiteral = actualStatement.Expression :?> IntegerLiteral
            Assert.AreEqual(2, actualIntegerLiteral.Value)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyInfix() =
        let input = {InfixExpression.Left = one(); Token = dummyToken; Operator= "+"; Right= two()}
        let expected = {InfixExpression.Left = two(); Token = dummyToken; Operator= "+"; Right= one()}

        let result = runModify input//modify input dummyEnvironment turnOneIntoTwo

        match result with
        | :? InfixExpression as actualInfix ->
            Assert.AreEqual(expected.Left, actualInfix.Left)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyPrefix() =
        let input = {PrefixExpression.Operator = "-"; Right = one(); Token = dummyToken}
        let expected = {PrefixExpression.Operator = "-"; Right = two(); Token = dummyToken}

        let result = runModify input
        //let result, _ = modify input dummyEnvironment turnOneIntoTwo

        match result with
        | :? PrefixExpression as actualPrefix ->
            Assert.AreEqual(expected.Right, actualPrefix.Right)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyIndex() =
        let input = {IndexExpression.Left = one(); Index = one(); Token = dummyToken}
        let expected = {IndexExpression.Left = two(); Index = two(); Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? IndexExpression as actualIndex ->
            Assert.AreEqual(expected.Left, actualIndex.Left)
            Assert.AreEqual(expected.Index, actualIndex.Index)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyIf() =
        let inputStatements = {BlockStatement.Statements = [{ExpressionStatement.Expression = one(); Token = dummyToken}]; Token = dummyToken}
        let input = {IfExpression.Condition = one(); Consequence = inputStatements; Alternative = inputStatements; Token = dummyToken}
        let expectedStatements = {BlockStatement.Statements = [{ExpressionStatement.Expression = two(); Token = dummyToken}]; Token = dummyToken}
        let expected = {IfExpression.Condition = two(); Consequence = expectedStatements; Alternative = expectedStatements; Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? IfExpression as ifExp ->
            Assert.AreEqual(expected.Condition, ifExp.Condition)
            Assert.AreEqual(expectedStatements, ifExp.Consequence)
            Assert.AreEqual(expectedStatements, ifExp.Alternative)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testReturn() =
        let input = {ReturnStatement.ReturnValue = one(); Token = dummyToken}
        let expected = {ReturnStatement.ReturnValue = two(); Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? ReturnStatement as ret ->
            Assert.AreEqual(expected.ReturnValue, ret.ReturnValue)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testLet() =
        let input = {LetStatement.Value = one(); Token = dummyToken; Name = {Identifier.Token = dummyToken; Value = "Input"}}
        let expected = {LetStatement.Value = two(); Token = dummyToken; Name = {Identifier.Token = dummyToken; Value = "Expected"}}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? LetStatement as ls ->
            Assert.AreEqual(expected.Value, ls.Value)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testFunctionLiteral() =
        let inputStatements = {BlockStatement.Statements = [{ExpressionStatement.Expression = one(); Token = dummyToken}]; Token = dummyToken}
        let expectedStatements = {BlockStatement.Statements = [{ExpressionStatement.Expression = two(); Token = dummyToken}]; Token = dummyToken}
        let input = {FunctionLiteral.Parameters = []; Body = inputStatements; Token = dummyToken}
        let expected = {FunctionLiteral.Parameters = []; Body = expectedStatements; Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? FunctionLiteral as fl ->
            Assert.AreEqual(expectedStatements, fl.Body)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testArrayLiteral() =
        let input = {ArrayLiteral.Elements = [one(); two()]; Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? ArrayLiteral as al ->
            Assert.AreEqual(2, al.Elements.Length)
            Assert.AreEqual(two(), al.Elements.[0])
            Assert.AreEqual(one(), al.Elements.[1])
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testHashLiteral() =
        let pairs = new System.Collections.Generic.Dictionary<Expression, Expression>()
        pairs.Add(one(), one()) |> ignore
        let input = {HashLiteral.Pairs = pairs; Token = dummyToken}

        let result = runModify input
        //let result = modify input turnOneIntoTwo

        match result with
        | :? HashLiteral as hl ->
            Assert.AreEqual(true, hl.Pairs
            |> Seq.forall(fun kvp -> kvp.Key = two() && kvp.Value = two()))
        | _ ->
            Assert.Fail("Wrong type, fool")