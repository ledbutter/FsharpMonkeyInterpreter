namespace Monkey

open Monkey.Ast
open Monkey.Token


module Parser =

    exception ParseError of string

    type ParserOutput =
        | Program of Program
        | Errors of string List

    type OperatorPrecedence = Lowest = 1 | Equals = 2 | LessGreater = 3 | Sum = 4 | Product = 5 | Prefix = 6 | Call = 7

    let parseProgram tokens =

        let precedences = dict[ EQ, OperatorPrecedence.Equals;
                                NOT_EQ, OperatorPrecedence.Equals;
                                LT, OperatorPrecedence.LessGreater;
                                GT, OperatorPrecedence.LessGreater;
                                PLUS, OperatorPrecedence.Sum;
                                MINUS, OperatorPrecedence.Sum;
                                SLASH, OperatorPrecedence.Product;
                                ASTERISK, OperatorPrecedence.Product;]

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
                (identifier, remainingTokens)

            let parseIntegerLiteral currentToken remainingTokens _ =
                let parsed, value = System.Int64.TryParse(currentToken.Literal)
                if parsed then
                    let intLiteral = {IntegerLiteral.Token = currentToken; Value = value} :> Expression
                    (intLiteral, remainingTokens)
                else
                    let errorMessage = sprintf "Could not parse %s as an integer" currentToken.Literal
                    raise (ParseError errorMessage)

            let parsePrefixExpression currentToken remainingTokens parseNext =
                let (right, newRemaining) = parseNext OperatorPrecedence.Prefix (List.head remainingTokens) (List.tail remainingTokens)
                let prefixExpression = {PrefixExpression.Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                (prefixExpression, newRemaining)

            let parseInfixExpression left currentToken remainingTokens parseNext =
                let precedence = getPrecedence currentToken
                let (right, newRemaining) = parseNext precedence (List.head remainingTokens) (List.tail remainingTokens)
                let infixExpression = {InfixExpression.Left = left; Token = currentToken; Operator = currentToken.Literal; Right = right} :> Expression
                (infixExpression, newRemaining)

            let parseBoolean currentToken remainingTokens _ =
                let parsed, value = System.Boolean.TryParse(currentToken.Literal)
                if parsed then
                    let boolean = {Boolean.Token = currentToken; Value = value} :> Expression
                    (boolean, remainingTokens)
                else
                    let errorMessage = sprintf "Could not parse %s as a boolean" currentToken.Literal
                    raise (ParseError errorMessage)

            let parseGroupedExpression _ remainingTokens parseNext =
                let (expression, newRemaining) = parseNext OperatorPrecedence.Lowest (List.head remainingTokens) (List.tail remainingTokens)
                let peekResult = expectPeek (List.head newRemaining) RPAREN
                match peekResult with
                | [] -> (expression, (List.tail newRemaining))
                | errors -> 
                    let errorMessage = combineStrings errors
                    raise (ParseError(errorMessage))

            let parseBlockStatement currentToken remainingTokens =
                let reachedEndCondition token =
                    token.Type = RBRACE || token.Type = EOF
                
                let rec parseBlockStatementRec current remaining statements =
                    if (List.isEmpty remaining) || reachedEndCondition current then
                        ({BlockStatement.Token = currentToken; Statements = statements}, remaining)
                    else
                        // ignoring any errors here, so what
                        let (nextStatement, remaining', _) = parseStatement current remaining
                        parseBlockStatementRec (List.head remaining') (List.tail remaining') (nextStatement::statements)

                parseBlockStatementRec currentToken remainingTokens []

            let parseIfExpression currentToken remainingTokens parseNext =
                let peekResult = expectPeek (List.head remainingTokens) LPAREN
                match peekResult with
                | [] ->
                    let emptyAlternative = {BlockStatement.Token = {Type = ""; Literal = ""}; Statements = List.empty}
                    let (condition, newRemaining) = parseNext OperatorPrecedence.Lowest remainingTokens.[1] remainingTokens.[2..]
                    let rParenPeekResult = expectPeek (List.head newRemaining) RPAREN
                    match rParenPeekResult with
                    | [] ->
                        let consequenceStart = iterateUntil (List.tail newRemaining) LBRACE
                        let (consequence, nextRemaining) = parseBlockStatement consequenceStart.[1] consequenceStart.[2..]
                        match nextRemaining with
                        | [] -> 
                            let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = emptyAlternative}
                            (ifExpression :> Expression, [])
                        | x::xs when x.Type = ELSE && (List.head xs).Type = LBRACE ->
                            let (alternative, finalRemaining) = parseBlockStatement xs.[1] xs.[2..]
                            let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = alternative}
                            (ifExpression :> Expression, finalRemaining)
                        | _ ->
                            let ifExpression = {IfExpression.Token = currentToken; Condition = condition; Consequence = consequence; Alternative = emptyAlternative}
                            (ifExpression :> Expression, [])
                    | errors -> 
                        let errorMessage = combineStrings errors
                        raise (ParseError(errorMessage))
                | errors -> 
                    let errorMessage = combineStrings errors
                    raise (ParseError(errorMessage))

            let parseFunctionParameters remainingTokens =
                let identifierToken = (List.head remainingTokens)
                if identifierToken.Type = RPAREN then
                    ([], (List.tail remainingTokens))
                else
                    let identifier = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    let rec parseFunctionParametersRec remainingTokens' identifiers =
                        match (List.head remainingTokens').Type with
                        | COMMA -> 
                            let nextIdentifierToken = remainingTokens'.[1]
                            let nextIdentifier = {Identifier.Token = nextIdentifierToken; Value = nextIdentifierToken.Literal}
                            parseFunctionParametersRec remainingTokens'.[2..] (nextIdentifier::identifiers)
                        | _ ->
                            ((List.rev identifiers), (List.tail remainingTokens'))
                    parseFunctionParametersRec remainingTokens.[1..] [identifier]

            let parseFunctionLiteral currentToken remainingTokens _ =
                let peekResult = expectPeek (List.head remainingTokens) LPAREN
                match peekResult with
                | [] ->
                    let (parameters, remainingTokens') = parseFunctionParameters (List.tail remainingTokens)
                    match remainingTokens' with
                    | [] -> raise (ParseError("No tokens for function body!"))
                    | x::_ when x.Type <> LBRACE -> raise (ParseError("Found token other then left brace for function body!"))
                    | _::xs ->
                        let (body, finalRemaining) = parseBlockStatement (List.head xs) (List.tail xs)
                        let functionLiteral = {FunctionLiteral.Parameters = parameters; Token = currentToken; Body = body} :> Expression
                        (functionLiteral, finalRemaining)
                | errors -> 
                    let errorMessage = combineStrings errors
                    raise (ParseError(errorMessage))

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
                                                FUNCTION, parseFunctionLiteral;]

            let infixParseFunctionMap = dict [  PLUS, parseInfixExpression;
                                                MINUS, parseInfixExpression;
                                                SLASH, parseInfixExpression;
                                                ASTERISK, parseInfixExpression;
                                                EQ, parseInfixExpression;
                                                NOT_EQ, parseInfixExpression;
                                                LT, parseInfixExpression;
                                                GT, parseInfixExpression;]

            match currentToken.Type with
            | LET -> 
                let identifierToken = (List.head remainingTokens)
                let parserErrors = List.append (expectPeek identifierToken IDENT) (expectPeek remainingTokens.[1] ASSIGN)

                match parserErrors with
                | [] ->
                    let nextRemainingTokens = iterateUntil remainingTokens.[2..] SEMICOLON
                    let name = {Identifier.Token = identifierToken; Value = identifierToken.Literal}
                    // todo: create an actual expression
                    let value = EmptyExpression()
                    // notice we are downcasting to statement here!!!
                    let letStatement = {Token = currentToken; Name = name; Value = value} :> Statement
                    // skip over the semicolon by just taking the tail
                    (letStatement, (List.tail nextRemainingTokens), [])
                | errors ->
                    (EmptyStatement() :> Statement, remainingTokens, errors)

            | RETURN ->
                let nextRemainingTokens = iterateUntil remainingTokens.[1..] SEMICOLON
                // todo: create an actual expression
                let value = EmptyExpression()
                let returnStatement = {Token = currentToken; ReturnValue = value} :> Statement
                (returnStatement, nextRemainingTokens.Tail, [])
            | _ ->
                // everything else is considered an expression statement

                // this parseExpression algorithm is an example of Pratt parsing
                let rec parseExpression precedence currentToken remainingTokens =
                    let reachedEndCondition token =
                        token.Type = SEMICOLON || (token |> getPrecedence <= precedence)

                    let rec applyInfix remainingTokens' currentInfix =
                        let token = (List.head remainingTokens')
                        if token |> reachedEndCondition then
                            (currentInfix, remainingTokens') 
                        else
                            let found, infixFunction = infixParseFunctionMap.TryGetValue token.Type

                            if found then
                                let newCurrentInfix, newCurrentRemaining = infixFunction currentInfix (List.head remainingTokens') (List.tail remainingTokens') parseExpression
                                match newCurrentRemaining with
                                | [] -> (newCurrentInfix, [])
                                | _ -> applyInfix newCurrentRemaining newCurrentInfix
                            else
                                (currentInfix, remainingTokens')

                    let found, prefixFunction = prefixParseFunctionMap.TryGetValue currentToken.Type
                    if found then
                        let left, remaining = prefixFunction currentToken remainingTokens parseExpression
                        match remaining with
                        | [] -> 
                            (left, [])
                        | x::_ when x |> reachedEndCondition -> 
                            (left, remaining)
                        | _ ->
                            applyInfix remaining left
                    else
                        raise (ParseError (sprintf "Unable to find prefix parser function for token type %s" currentToken.Type))

                let (expression, updatedRemainingTokens) = parseExpression OperatorPrecedence.Lowest currentToken remainingTokens
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
