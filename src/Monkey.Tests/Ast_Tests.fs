namespace Tests

open NUnit.Framework
open Monkey.Parser
open Monkey.Lexer2
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