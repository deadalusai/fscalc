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

//matches variable "names" like the following regex: [a-z_][a-z_0-9]*
let pname : Parser<Name, unit> =
    let nameChar1 c = isAsciiLetter c || isAnyOf "_" c
    let nameChar c = nameChar1 c || isDigit c
    many1Satisfy2L nameChar1 nameChar "variable name" |>> Name

//a placeholder for the pexpr parser which will parse BEDMAS operations
let pexpr, private pexprRef = createParserForwardedToRef<Expr, unit> ()

//parses any expression surrounded with brackets
let pbracketedExpr = betweenBrackets pexpr <?> "bracketed expression"

//parse a float (constant) or variable (name) and convert it to a Term expression
let pterm = (pfloat |>> Constant <?> "constant") <|> (pname |>> Variable <?> "variable name") |>> Term

//parse any supported functions
let pfunction =
    let args = (ws1 >>? pterm) <|> pbracketedExpr
    pname .>>.? args |>> FunctionCall <?> "function call"
    
//parse an assignment command
// let Name = Expr [, Name2 = Expr2]
let passignment = 
    let binding = (pname .>> ws_str_ws "=") .>>. pexpr |>> Assignment
    let manyBindings = sepBy1 binding (ws_str_ws ",")
    str_ws1 "let" >>. manyBindings <?> "variable binding"

//parse a deletion command
// del Name [, Name2]
let pdeletion = 
    let name = (pname |>> Deletion)
    let manyNames = sepBy1 name (ws_str_ws ",")
    str_ws1 "del" >>. manyNames <?> "variable deletion"
    
//attempts to parse a "negative" expression
//expression can be any term or bracketed expression
let pnegativeExpr =
    let negSymbols = (many1Chars (pchar '-')) 
    let expression = (pterm <|> pbracketedExpr)
    pipe2 negSymbols expression (fun s e -> if s.Length % 2 = 0 then e else Negative e) <?> "negative expression"

//implement the pexpr parser
do pexprRef :=
    let opp = new OperatorPrecedenceParser<Expr, unit, unit> ()
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

let pcommand_eof = ((passignment |>> Update) <|> (pdeletion |>> Update) <|> (pexpr |>> Expr)).>> eof