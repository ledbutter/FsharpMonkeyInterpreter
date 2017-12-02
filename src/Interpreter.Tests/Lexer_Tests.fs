namespace Tests

open NUnit.Framework
open Monkey.Token
open Monkey.Lexer
open Monkey.Lexer2

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


    [<Test>]
    let testNextTokenAndReadChar_2() =
        let input = @"let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
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
            BANG, "!"
            MINUS, "-"
            SLASH, "/"
            ASTERISK, "*"
            INT, "5"
            SEMICOLON, ";"
            INT, "5"
            LT, "<"
            INT, "10"
            GT, ">"
            INT, "5"
            SEMICOLON, ";"
            EOF, (char 0).ToString()
        ]

        let tokens = tokenizeInput input

        Assert.AreEqual(expectedResults.Length, tokens.Length, "not right number of results")

        for i in 0..expectedResults.Length-1 do
            let expectedType, expectedLiteral = expectedResults.[i]
            let actualToken = tokens.[i]
            Assert.AreEqual(expectedType, actualToken.Type, "type")
            Assert.AreEqual(expectedLiteral, actualToken.Literal, "literal")