namespace Tests

open NUnit.Framework
open Monkey.Token
open Monkey.TokenConsts
open Monkey.Lexer

module LexerTests =

    let nextTokenTestCases =
        [
            "=", ASSIGN, "="
            "+", PLUS, "+"
            "(", LPAREN, "("
            ")", RPAREN, ")"
            "{", LBRACE, "{"
            "}", RBRACE, "}"
            ",", COMMA, ","
            ";", SEMICOLON, ";"
            (emptyChar.ToString()), EOF, (emptyChar.ToString())
        ] |> List.map(fun (i, t, l) -> TestCaseData(i, t, l))
    
    [<TestCaseSource("nextTokenTestCases")>]
    let testNextToken(input:string, expectedTokenType:TokenType, expectedLiteral:string) =
        //let input = "=+(){},;"

        let lexer = createLexer input
        let token = nextToken lexer
        Assert.AreEqual(expectedTokenType, token.Type, "type")
        Assert.AreEqual(expectedLiteral, token.Literal, "literal")