module Calculator.FParsec.Engine

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Calculator.FParsec.Ast

let private ws = spaces
let private ws1 = spaces1
let private str_ws s = pstring s >>. ws
let private str_ws1 s = pstring s >>. ws1

//matches variable "names" like the following regex: [a-z_][a-z_0-9]*
let pname =
    let isAsciiLetterOrUnderscore c = isAsciiLetter c || isAnyOf "_" c
    let isAsciiLetterOrUnderscoreOrDigit c = isAsciiLetterOrUnderscore c || isDigit c
    many1Satisfy2 isAsciiLetterOrUnderscore isAsciiLetterOrUnderscoreOrDigit

//parse a float (constant) or variable (name) and convert it to a Term expression 
let pterm = ((pfloat |>> Constant) <|> (pname |>> Variable)) |>> Term

//implement the pexpr parser
let pexpr =
    let opp = new OperatorPrecedenceParser<Expr, unit, unit> ()
    let expr = opp.ExpressionParser
    opp.TermParser <- (pterm .>> ws) <|> between (str_ws "(") (str_ws ")") expr
    //BEDMAS
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Subtract (x, y)))
    opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> Add (x, y)))
    opp.AddOperator(InfixOperator("*", ws, 3, Associativity.Left, fun x y -> Multiply (x, y)))
    opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun x y -> Divide (x, y)))
    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, fun x y -> Power (x, y)))
    opp.AddOperator(InfixOperator("r", ws, 6, Associativity.Left, fun x y -> Root (x, y)))
    expr