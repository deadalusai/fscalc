module Calculator.Engine

open System.Text.RegularExpressions
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Calculator.Ast

let private ws = spaces
let private ws1 = spaces1
let private str_ws s = skipString s >>? ws
let private str_ws1 s = skipString s >>? ws1
let private ws_str_ws s = ws >>. skipString s .>> ws
let private betweenBrackets p = between (ws_str_ws "(") (ws_str_ws ")") p

type ParserState = { Functions : Set<string> }

//matches "names"
let private pname = regexL "[a-zA-Z_][a-zA-Z_0-9]*" "name"

let pvariable = pname |>> (fun name -> { Key = name })

//a placeholder for the pexpr parser which will parse BEDMAS operations
let pexpr, private pexprRef = createParserForwardedToRef<Expr, _> ()

//parses any expression surrounded with brackets
let pbracketedExpr = betweenBrackets pexpr <?> "bracketed expression"

//parse a float (constant) or variable (name) and convert it to a Term expression
let pterm = (pfloat |>> Constant <?> "constant") <|> (pvariable |>> Variable <?> "variable")

//parse any supported Functions
let pfunction =
    let functionArguments = (ws1 >>? pterm) <|> pbracketedExpr
    let functionName: Parser<Name, ParserState> =
        (fun stream ->
            //Attempt to parse a Name
            let reply = (pname stream)
            //if successful, assert that that name appears in the available function set
            if reply.Status = Ok then
                if Set.contains reply.Result stream.UserState.Functions then Reply({ Key = reply.Result })
                else Reply(Error, messageError "not a function name")
            //fail on error or missing function
            else Reply(Error, messageError "expected function name"))

    attempt (functionName .>>. functionArguments) |>> FunctionCall <?> "function call"
    
//parse an assignment command
// let Name = Expr [, Name2 = Expr2]
let passignment = 
    let binding = pipe2 (pvariable .>> ws_str_ws "=") pexpr (fun name expr -> (name, expr))
    let manyBindings = sepBy1 binding (ws_str_ws ",")
    str_ws1 "let" >>. manyBindings <?> "variable binding"

//attempts to parse a "negative" expression
//expression can be any term or bracketed expression
let pnegativeExpr =
    let negSymbols = (many1Chars (pchar '-')) 
    let expression = (pterm <|> pbracketedExpr)
    pipe2 negSymbols expression (fun s e -> if s.Length % 2 = 0 then e else Negative e) <?> "negative expression"

//implement the pexpr parser
do pexprRef :=
    let opp = new OperatorPrecedenceParser<Expr, unit, _> ()
    let expr = opp.ExpressionParser
    opp.TermParser <- (pfunction <|> pterm <|> pnegativeExpr <|> pbracketedExpr) .>> ws
    //BEDMAS
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Subtract (x, y)))
    opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> Add (x, y)))
    opp.AddOperator(InfixOperator("*", ws, 3, Associativity.Left, fun x y -> Multiply (x, y)))
    opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun x y -> Divide (x, y)))
    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, fun x y -> Power (x, y)))
    opp.AddOperator(InfixOperator("mod", ws, 6, Associativity.Left, fun x y -> Modulo (x, y)))
    expr

let pcommand_eof = ((passignment |>> Assignment) <|> (pexpr |>> Single)).>> eof