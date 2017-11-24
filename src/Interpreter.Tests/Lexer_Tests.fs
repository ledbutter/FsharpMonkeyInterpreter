namespace Tests

open NUnit.Framework
open Monkey.Token
open Monkey.Lexer

module LexerTests =

    [<Test>]
    let testNextTokenAndReadChar() =
        let input = @"let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            "

        let expectedResults = [
            LET, "let"
            IDENT, "five"
            ASSIGN, "="
            INT, "5"
            SEMICOLON, ";"
            LET, "let"
            IDENT, "ten"
            ASSIGN, "="
            INT, "10"
            SEMICOLON, ";"
            LET, "let"
            IDENT, "add"
            ASSIGN, "="
            FUNCTION, "fn"
            LPAREN, "("
            IDENT, "x"
            COMMA, ","
            IDENT, "y"
            RPAREN, ")"
            LBRACE, "{"
            IDENT, "x"
            PLUS, "+"
            IDENT, "y"
            SEMICOLON, ";"
            RBRACE, "}"
            SEMICOLON, ";"
            LET, "let"
            IDENT, "result"
            ASSIGN, "="
            IDENT, "add"
            LPAREN, "("
            IDENT, "five"
            COMMA, ","
            IDENT, "ten"
            RPAREN, ")"
            SEMICOLON, ";"
            EOF, (char 0).ToString()
        ]

        let lexer = createLexer input

        let rec runTestCaseRec lex expected =
            match expected with
            | [] -> ignore
            | x::xs -> 
                let nextLexer, token = nextToken lex
                let expectedType, expectedLiteral = x
                Assert.AreEqual(expectedType, token.Type, "type")
                Assert.AreEqual(expectedLiteral, token.Literal, "literal")
                runTestCaseRec nextLexer xs
        runTestCaseRec lexer expectedResults |> ignore