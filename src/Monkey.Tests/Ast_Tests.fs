namespace Tests

open NUnit.Framework
open Monkey.Ast
open Monkey.Token

module Ast_Tests =
    
    let dummyToken = {Token.Literal = "foo"; Type = INT}

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

    let turnOneIntoTwo (node :Node) =
        match node with
        | :? IntegerLiteral as il ->
            if il.Value = 1L then
                {il with Value = 2L} :> Node
            else
                node
        | _ ->
            node
   
    [<Test>]
    let testModifySimple() =
        let result = modify (one()) turnOneIntoTwo
        match result with
        | :? IntegerLiteral as il ->
            Assert.AreEqual(2, il.Value)
        | _ ->
            Assert.Fail("Wrong type, fool")

    [<Test>]
    let testModifyComplex() =
        let input = {Program.Statements = [{ExpressionStatement.Expression = one(); Token = dummyToken}]}
        let expected = {Program.Statements = [{ExpressionStatement.Expression = two(); Token = dummyToken}]}

        let result = modify input turnOneIntoTwo

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

        let result = modify input turnOneIntoTwo

        match result with
        | :? InfixExpression as actualInfix ->
            Assert.AreEqual(expected.Left, actualInfix.Left)
        | _ ->
            Assert.Fail("Wrong type, fool")
