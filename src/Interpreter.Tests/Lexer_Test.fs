namespace Tests

open NUnit.Framework
open Monkey.Token
open Monkey.TokenConsts
open Monkey.Lexer

module LexerTests =

    let nextTokenTestCases =
        [
            ASSIGN, "="
            PLUS, "+"
            LPAREN, "("
            RPAREN, ")"
            LBRACE, "{"
            RBRACE, "}"
            COMMA, ","
            SEMICOLON, ";"
            EOF, ""
        ] |> List.map(fun (t, l) -> TestCaseData(t, l))
    
    [<TestCaseSource("nextTokenTestCases")>]
    let testNextToken(expectedTokenType:TokenType, expectedLiteral:string) =
        let input = "=+(){},;"

        let lexer = createLexer input
        let token = nextToken lexer
        Assert.AreEqual(expectedTokenType, token.Type)
        Assert.AreEqual(expectedLiteral, token.Literal)

