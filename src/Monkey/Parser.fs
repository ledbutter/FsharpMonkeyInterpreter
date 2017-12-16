namespace Monkey

open Monkey.Ast
open Monkey.Lexer2
open Monkey.Token


module Parser =
    //type Parser = {Lexer: Lexer2; CurrentToken: Token; PeekToken: Token} <-- can't do this since we have the lexer just return all tokens
    //type Parser = {Tokens: Token[]; CurrentPosition: int}

    let parseProgram tokens =
        //todo: pick up at page 41
        let statements : Statement list = []
        let p = { Statements = statements }
        p
