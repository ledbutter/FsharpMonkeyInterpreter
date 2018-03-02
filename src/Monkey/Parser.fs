namespace Monkey

open Monkey.Ast
open Monkey.Token


module Parser =
    open System

    type ParserOutput =
        | Program of Program
        | Errors of string List

    type ExpressionOutput =
                | ParsedExpression of Expression * Token List
                | ExpressionErrors of string List

    type OperatorPrecedence = Lowest = 1 | Equals = 2 | LessGreater = 3 | Sum = 4 | Product = 5 | Prefix = 6 | Call = 7 | Index = 8

    let parseProgram tokens =

        let precedences = dict[ EQ, OperatorPrecedence.Equals;
                                NOT_EQ, OperatorPrecedence.Equals;
                                LT, OperatorPrecedence.LessGreater;
                                GT, OperatorPrecedence.LessGreater;
                                PLUS, OperatorPrecedence.Sum;
                                MINUS, OperatorPrecedence.Sum;
                                SLASH, OperatorPrecedence.Product;
                                ASTERISK, OperatorPrecedence.Product;
                                LPAREN, OperatorPrecedence.Call;
                                LBRACKET, OperatorPrecedence.Index;]

        let getPrecedence token =
            let found, p = precedences.TryGetValue token.Type
            if found then
                p
            else
                OperatorPrecedence.Lowest

        let rec parseStatement currentToken (remainingTokens: Token list) =

            //todo: convert this to a Some/None construct
            let expectPeek token tokenType =
                if token.Type = tokenType then
                    []
                else
                    [sprintf "Expected next token to be %s, got %s instead" tokenType token.Type]

            let combineStrings strings =
                strings
                |> Seq.fold(fun (sb:System.Text.StringBuilder) s ->
                    sb.AppendLine(s)) (new System.Text.StringBuilder())
                |> fun x -> x.ToString()

            let rec iterateUntil (iterableTokens: Token list) (tokenType:string) =
                if iterableTokens.Head.Type = tokenType then
                    iterableTokens                    
                else
                    iterateUntil iterableTokens.Tail tokenType

            let parseIdentifier currentToken remainingTokens _ =
                let identifier = {Identifier.Token = currentToken; Value = currentToken.Literal} :> Expression
                ParsedExpression(identifier, remainingTokens)

            let parseIntegerLiteral currentToken remainingTokens _ =
                let parsed, value = System.Int64.TryParse(currentToken.Literal)
                if parsed then
                    let intLiteral = {IntegerLiteral.Token = currentToken; Value = value} :> Expression
                    ParsedExpression(intLiteral, remainingTokens)
                else
                    let errorMessage = sprintf "Could not parse %s as an integer" currentToken.Literal
                    ExpressionErrors([errorMessage])

            let parseStringLiteral currentToken remainingTokens _ =
                let stringLiteral = {StringLiteral.Token = currentToken; Value = currentToken.Literal} :> Expression
                ParsedExpression(stringLiteral, remainingTokens)

            let parsePrefixExpression currentToken remainingTokens parseNext =
                let rightResult = parseNext OperatorPrecedence.Prefix (List.head remainingTokens) (List.tail remainingTokens)
                match rightResult with
                | ParsedExpression(right, newRemaining) ->
                    let prefixExpression = {PrefixExpression.Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                    ParsedExpression(prefixExpression, newRemaining)
                | ExpressionErrors _ -> rightResult

            let parseInfixExpression left currentToken remainingTokens parseNext =
                let precedence = getPrecedence currentToken
                let parseRightResult = parseNext precedence (List.head remainingTokens) (List.tail remainingTokens)
                match parseRightResult with
                | ParsedExpression (right, newRemaining) ->
                    let infixExpression = {InfixExpression.Left = left; Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                    ParsedExpression(infixExpression, newRemaining)
                | ExpressionErrors _ -> parseRightResult

            let parseBoolean currentToken remainingTokens _ =
                let parsed, value = System.Boolean.TryParse(currentToken.Literal)
                if parsed then
                    let boolean = {Boolean.Token = currentToken; Value = value} :> Expression
                    ParsedExpression(boolean, remainingTokens)
                else
                    let errorMessage = sprintf "Could not parse %s as a boolean" currentToken.Literal
                    ExpressionErrors([errorMessage])

            let parseGroupedExpression _ remainingTokens parseNext =
                let expressionResult = parseNext OperatorPrecedence.Lowest (List.head remainingTokens) (List.tail remainingTokens)
                match expressionResult with
                | ParsedExpression(expression, newRemaining) ->
                    let peekResult = expectPeek (List.head newRemaining) RPAREN
                    match peekResult with
                    | [] -> ParsedExpression(expression, (List.tail newRemaining))
                    | errors -> 
                        let errorMessage = combineStrings errors
                        ExpressionErrors([errorMessage])
                | ExpressionErrors _ -> expressionResult

            let parseBlockStatement currentToken remainingTokens =
                let reachedEndCondition token =
                    token.Type = RBRACE || token.Type = EOF
                
                let rec parseBlockStatementRec current remaining statements errors =
                    if (List.isEmpty remaining) || reachedEndCondition current then
                        ({BlockStatement.Token = currentToken; Statements = statements}, remaining, errors)
                    else
                        let (nextStatement, remaining', currentErrors) = parseStatement current remaining
                        let updatedStatements = nextStatement::statements
                        let updatedErrors = currentErrors@errors

                        match remaining' with
                        | [] ->
                            ({BlockStatement.Token = currentToken; Statements = updatedStatements}, [], updatedErrors)
                        | x::xs ->
                            parseBlockStatementRec x xs updatedStatements updatedErrors

                parseBlockStatementRec currentToken remainingTokens [] []

            let parseIfExpression currentToken remainingTokens parseNext =
                let peekResult = expectPeek (List.head remainingTokens) LPAREN
                match peekResult with
                | [] ->
                    let emptyAlternative = {BlockStatement.Token = {Type = ""; Literal = ""}; Statements = List.empty}
                    let conditionResult = parseNext OperatorPrecedence.Lowest remainingTokens.[1] remainingTokens.[2..]
                    match conditionResult with
                    | ParsedExpression(condition, newRemaining) ->
                        let rParenPeekResult = expectPeek (List.head newRemaining) RPAREN
                        match rParenPeekResult with
                        | [] ->
                            let consequenceStart = iterateUntil (List.tail newRemaining) LBRACE
                            let (consequence, nextRemaining, errors) = parseBlockStatement consequenceStart.[1] consequenceStart.[2..]
                            match errors with
                            | [] ->
                                match nextRemaining with
                                | [] -> 
                                    let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = emptyAlternative}
                                    ParsedExpression(ifExpression :> Expression, [])
                                | x::xs when x.Type = ELSE && (List.head xs).Type = LBRACE ->
                                    let (alternative, finalRemaining, errors) = parseBlockStatement xs.[1] xs.[2..]
                                    match errors with
                                    | [] ->
                                        let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = alternative}
                                        ParsedExpression(ifExpression :> Expression, finalRemaining)
                                    | _ -> ExpressionErrors(errors)
                                | _ ->
                                    let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = emptyAlternative}
                                    ParsedExpression(ifExpression :> Expression, [])
                            | _ -> ExpressionErrors(errors)
                    
                        | errors -> 
                            let errorMessage = combineStrings errors
                            ExpressionErrors([errorMessage])
                    | ExpressionErrors _ -> conditionResult
                | errors -> 
                    let errorMessage = combineStrings errors
                    ExpressionErrors([errorMessage])

            let parseFunctionParameters remainingTokens =
                let identifierToken = (List.head remainingTokens)
                if identifierToken.Type = RPAREN then
                    ([], (List.tail remainingTokens))
                else
                    let identifier = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    let rec parseFunctionParametersRec remainingTokens' identifiers =
                        match remainingTokens' with
                        | x::xs when x.Type = COMMA -> 
                            let nextIdentifierToken = (List.head xs)
                            let nextIdentifier = {Identifier.Token = nextIdentifierToken; Value = nextIdentifierToken.Literal}
                            parseFunctionParametersRec (List.tail xs) (nextIdentifier::identifiers)
                        | _ ->
                            ((List.rev identifiers), (List.tail remainingTokens'))
                    parseFunctionParametersRec remainingTokens.[1..] [identifier]

            let parseFunctionLiteral currentToken remainingTokens _ =
                let peekResult = expectPeek (List.head remainingTokens) LPAREN
                match peekResult with
                | [] ->
                    let (parameters, remainingTokens') = parseFunctionParameters (List.tail remainingTokens)
                    match remainingTokens' with
                    | [] -> ExpressionErrors(["No tokens for function body!"])
                    | x::_ when x.Type <> LBRACE -> ExpressionErrors(["Found token other then left brace for function body!"])
                    | _::xs ->
                        let (body, finalRemaining, errors) = parseBlockStatement (List.head xs) (List.tail xs)
                        match errors with
                        | [] ->
                            let functionLiteral = {FunctionLiteral.Parameters = parameters; Token = currentToken; Body = body} :> Expression
                            ParsedExpression(functionLiteral, finalRemaining)
                        | _ -> ExpressionErrors(errors)
                | errors -> 
                    let errorMessage = combineStrings errors
                    ExpressionErrors([errorMessage])

            let parseExpressionList remainingTokens endTokenType parse =
                let rec parseExpressionListRec remainingTokens' expressions =
                    match remainingTokens' with
                    | x::xs when x.Type = COMMA ->
                        let nextExpressionResult = parse OperatorPrecedence.Lowest (List.head xs) (List.tail xs)
                        match nextExpressionResult with
                        | ParsedExpression(nextExpression, newRemaining) ->
                            parseExpressionListRec newRemaining (nextExpression::expressions)
                        | ExpressionErrors err -> ([], [], err)
                    | _ ->
                        (List.rev expressions, remainingTokens', [])

                match remainingTokens with
                | x::xs when x.Type = endTokenType ->
                    ([], xs, [])
                | x::xs ->
                    let firstExpressionResult = parse OperatorPrecedence.Lowest x xs
                    match firstExpressionResult with
                    | ParsedExpression(firstExpression, remainingAfterFirst) ->
                        let (expressions, newRemaining, errors) = parseExpressionListRec remainingAfterFirst [firstExpression]
                        match errors with
                        | [] ->
                            // make sure we are at the end
                            match newRemaining with
                            | x::xs when x.Type = endTokenType ->
                                (expressions, xs, [])
                            | [] ->
                                (expressions, [], [])
                            | _ ->
                                let errorMsg = sprintf "Did not end on expected end token type %A instead on %A" endTokenType (List.head newRemaining)
                                ([], [], [errorMsg])
                        | _ -> 
                            ([], [], errors)
                    | ExpressionErrors err -> 
                        ([], [], err)
                | _ -> 
                    ([], [], ["Somehow got in empty expression list"])

            let parseCallExpression left currentToken remainingTokens parseNext =
                let (arguments, newRemaining, errors) = parseExpressionList remainingTokens RPAREN parseNext
                match errors with
                | [] ->
                    let callExpression = {CallExpression.Token = currentToken; Function = left; Arguments = arguments} :> Expression
                    ParsedExpression(callExpression, newRemaining)
                | _ -> ExpressionErrors(errors)

            let parseArrayLiteral currentToken remainingTokens parseNext =
                let expressions, newRemaining, errors = parseExpressionList remainingTokens RBRACKET parseNext
                match errors with
                | [] ->
                    let arrayLiteral = {ArrayLiteral.Token = currentToken; Elements = expressions} :> Expression
                    ParsedExpression(arrayLiteral, newRemaining)
                | _ ->
                    ExpressionErrors(errors)
                
            let parseIndexExpression left currentToken remainingTokens parseNext = 
                let indexResult = parseNext OperatorPrecedence.Lowest (List.head remainingTokens) (List.tail remainingTokens)
                match indexResult with
                | ParsedExpression(index, remainingAfterIndex) ->
                    let indexExpression = {IndexExpression.Token = currentToken; Left = left; Index = index} :> Expression
                    // do a tail to skip over trailing ]
                    ParsedExpression(indexExpression, (List.tail remainingAfterIndex))
                | _ ->
                    indexResult

            let parseHashLiteral currentToken remainingTokens parseNext =


            // todo: there has to be a more elegant way of doing this:
            //      we are mapping strings to funcs
            let prefixParseFunctionMap = dict [ IDENT, parseIdentifier;
                                                INT, parseIntegerLiteral;
                                                BANG, parsePrefixExpression;
                                                MINUS, parsePrefixExpression;
                                                TRUE, parseBoolean;
                                                FALSE, parseBoolean;
                                                LPAREN, parseGroupedExpression;
                                                IF, parseIfExpression;
                                                FUNCTION, parseFunctionLiteral;
                                                STRING, parseStringLiteral;
                                                LBRACKET, parseArrayLiteral;]

            let infixParseFunctionMap = dict [  PLUS, parseInfixExpression;
                                                MINUS, parseInfixExpression;
                                                SLASH, parseInfixExpression;
                                                ASTERISK, parseInfixExpression;
                                                EQ, parseInfixExpression;
                                                NOT_EQ, parseInfixExpression;
                                                LT, parseInfixExpression;
                                                GT, parseInfixExpression;
                                                LPAREN, parseCallExpression;
                                                LBRACKET, parseIndexExpression;]

            // this parseExpression algorithm is an example of Pratt parsing
            let rec parseExpression precedence currentToken remainingTokens =
                let canContinue token =
                    token.Type <> SEMICOLON && (token |> getPrecedence > precedence)

                let rec applyInfix remainingTokens' currentInfix =
                    let token = (List.head remainingTokens')
                    if token |> canContinue then
                        let found, infixFunction = infixParseFunctionMap.TryGetValue token.Type

                        if found then
                            let infixResult = infixFunction currentInfix (List.head remainingTokens') (List.tail remainingTokens') parseExpression
                            match infixResult with
                            | ParsedExpression (exp, newRemaining) ->
                                match newRemaining with
                                | [] -> ParsedExpression(exp, [])
                                | _ -> applyInfix newRemaining exp
                            | ExpressionErrors err -> 
                                ExpressionErrors(err)
                                
                        else
                            ParsedExpression(currentInfix, remainingTokens')
                    else
                        ParsedExpression(currentInfix, remainingTokens')

                let found, prefixFunction = prefixParseFunctionMap.TryGetValue currentToken.Type
                if found then
                    let prefixResult = prefixFunction currentToken remainingTokens parseExpression
                    match prefixResult with
                    | ParsedExpression (exp, remaining) ->
                        match remaining with
                        | [] -> ParsedExpression(exp, [])
                        | x::_ when x |> canContinue ->
                            applyInfix remaining exp
                        | _ -> ParsedExpression(exp, remaining)
                    | ExpressionErrors _ -> prefixResult
                            
                else
                    let errorMessage = (sprintf "Unable to find prefix parser function for token type %s" currentToken.Type)
                    ExpressionErrors([errorMessage])

            match currentToken.Type with
            | LET -> 
                let identifierToken = (List.head remainingTokens)
                let parserErrors = List.append (expectPeek identifierToken IDENT) (expectPeek remainingTokens.[1] ASSIGN)

                match parserErrors with
                | [] ->
                    let name = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    let letStatementResult = parseExpression OperatorPrecedence.Lowest remainingTokens.[2] remainingTokens.[3..]
                    match letStatementResult with
                    | ParsedExpression(value, nextRemainingTokens) ->
                        let letStatement = {Token = currentToken; Name = name; Value = value} :> Statement
                        let nextRemainingTokens' =
                            match nextRemainingTokens with
                            | x::xs when x.Type = SEMICOLON -> xs
                            | _ -> nextRemainingTokens
                        (letStatement, nextRemainingTokens', [])
                    | ExpressionErrors err -> (EmptyStatement() :> Statement, [], err)
                | errors ->
                    (EmptyStatement() :> Statement, remainingTokens, errors)

            | RETURN ->
                let returnStatementResult = parseExpression OperatorPrecedence.Lowest (List.head remainingTokens) (List.tail remainingTokens)
                match returnStatementResult with
                | ParsedExpression(value, nextRemainingTokens) ->
                    let returnStatement = {Token = currentToken; ReturnValue = value} :> Statement
                    let nextRemainingTokens' =
                            match nextRemainingTokens with
                            | x::xs when x.Type = SEMICOLON -> xs
                            | _ -> nextRemainingTokens
                    (returnStatement, nextRemainingTokens', [])
                | ExpressionErrors err -> (EmptyStatement() :> Statement, [], err)
            | _ ->
                // everything else is considered an expression statement
                let expressionStatementResult = parseExpression OperatorPrecedence.Lowest currentToken remainingTokens
                match expressionStatementResult with
                | ParsedExpression(expression, updatedRemainingTokens) ->
                    let nextRemainingTokens =
                        match updatedRemainingTokens with
                        | [] ->
                            []
                        | x::_ when x.Type = SEMICOLON -> 
                            (List.tail updatedRemainingTokens)
                        | _ ->
                            updatedRemainingTokens
                    let expressionStatement = {ExpressionStatement.Token = currentToken; Expression = expression} :> Statement
                    (expressionStatement, nextRemainingTokens, [])
                | ExpressionErrors err -> (EmptyStatement() :> Statement, [], err)

        let rec parseStatements remainingTokens statements errors =
            let (|EndOfTokens|_|) (t: Token list) = 
                if List.isEmpty t || t.Head.Type = EOF then 
                    Some () 
                else 
                    None

            match remainingTokens with
            | EndOfTokens -> 
                if List.isEmpty errors then
                    Program({Program.Statements = (List.rev statements)})
                else
                    Errors(errors)
            | _ ->
                let newStatement, nextRemainingTokens, statementErrors = parseStatement remainingTokens.Head remainingTokens.Tail
                parseStatements nextRemainingTokens (newStatement::statements) (errors@statementErrors)

        parseStatements tokens [] []
