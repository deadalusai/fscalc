module Calculator.Engine

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Calculator.Ast

/// Maintains "memory" state for a calculator
type CalcState =
    { memory: System.Collections.Generic.IDictionary<string, double>;
      mutable debugOutput: System.IO.TextWriter;
      mutable debugMode: bool }

let private ws = spaces
let private ws1 = spaces1
let private str_ws s = skipString s >>? ws
let private str_ws1 s = skipString s >>? ws1

//matches variable "names" like the following regex: [a-z_][a-z_0-9]*
let pname =
    let isAsciiLetterOrUnderscore c = isAsciiLetter c || isAnyOf "_" c
    let isAsciiLetterOrUnderscoreOrDigit c = isAsciiLetterOrUnderscore c || isDigit c
    many1Satisfy2 isAsciiLetterOrUnderscore isAsciiLetterOrUnderscoreOrDigit |>> Name

//a placeholder for the pexpr parser which will parse BEDMAS operations
let pexpr, private pexprRef = createParserForwardedToRef<Expr, _> ()

//parse a float (constant) or variable (name) and convert it to a Term expression 
let pterm = ((pfloat |>> Constant) <|> (pname |>> Variable)) |>> Term

//parse any supported functions
let pfunction =
    let toFunction (fname, expr) =
        match fname with
        | "sin" -> Sin expr
        | "cos" -> Cos expr
        | "tan" -> Tan expr
        | "sqrt" -> Sqrt expr
        | _ -> failwith (sprintf "Unrecognised function %s" fname)
    //match any of sin cos tan or sqrt
    let pfname = (pstring "sin" <|> pstring "cos" <|> pstring "tan" <|> pstring "sqrt")
    pfname .>>? ws1 .>>.? pexpr |>> toFunction |>> Function

//parse an assignment command
// let Name = Expr
let passignment = (str_ws1 "let" >>. pname .>> ws .>> str_ws "=") .>>. pexpr |>> VarAssignment

//parse a deletion command
// del Name
let pdeletion = str_ws1 "del" >>. pname |>> VarDeletion

//implement the pexpr parser
do pexprRef :=
    let opp = new OperatorPrecedenceParser<Expr, unit, unit> ()
    let expr = opp.ExpressionParser
    opp.TermParser <- (pfunction .>> ws) <|> (pterm .>> ws) <|> between (str_ws "(") (str_ws ")") expr
    //BEDMAS
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Subtract (x, y)))
    opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> Add (x, y)))
    opp.AddOperator(InfixOperator("*", ws, 3, Associativity.Left, fun x y -> Multiply (x, y)))
    opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun x y -> Divide (x, y)))
    opp.AddOperator(InfixOperator("^", ws, 5, Associativity.Left, fun x y -> Power (x, y)))
    opp.AddOperator(InfixOperator("mod", ws, 6, Associativity.Left, fun x y -> Modulo (x, y)))
    expr

let pcommand_eof = (passignment <|> pdeletion <|> (pexpr |>> Expr)) .>> eof

    