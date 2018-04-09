namespace Tests

open NUnit.Framework
open Monkey.Ast
open Monkey.Token

module Ast_Tests =
    
    [<Test>]
    let testString() =
        let letStatement = {
            Token = {Type = LET; Literal = "let"}
            Name = {Token = {Type = IDENT; Literal = "myVar"}; Value = "myVar"}
            Value = {Identifier.Token = {Type = IDENT; Literal = "anotherVar"}; Value = "anotherVar"}
        }
        let program = {Statements = [letStatement]}
        Assert.AreEqual("let myVar = anotherVar;", program.ToString())
    
    let dummyToken = {Token.Literal = "foo"; Type = INT}

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
