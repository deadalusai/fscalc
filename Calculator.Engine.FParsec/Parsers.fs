module Calculator.Engine

open System.Text.RegularExpressions
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Calculator.Ast

let private ws = spaces
let private ws1 = spaces1
let private str_ws s = skipString s >>? ws
let private ws_str s = ws >>. skipString s
let private str_ws1 s = skipString s >>? ws1
let private ws_str_ws s = ws >>. skipString s .>> ws
let private betweenBrackets p = between (str_ws "(") (ws_str ")") p

//matches "names"
let private pname : Parser<Name, unit> = regexL "[a-zA-Z_][a-zA-Z_0-9]*" "name"

//a placeholder for the pexpr parser which will parse BEDMAS operations
let pexpr, private pexprRef = createParserForwardedToRef<Expr, _> ()

//parses any expression surrounded with brackets
let pbracketedExpr = betweenBrackets pexpr <?> "bracketed expression"

//parse a float (constant) or variable (name) and convert it to a Term expression
let pterm = (pfloat |>> Const <?> "constant") <|> (pname |>> Fetch <?> "variable")

//parse any supported Functions
let pfunctionCall =
    let pargs = sepEndBy1 (pterm <|> pbracketedExpr) ws1
    attempt (pname .>> ws1 .>>. pargs) |>> Call <?> "function call"
    
//Parse "= Expr" -> Expr
let private pEqExpr = ws_str_ws "=" >>. pexpr

//Parse a function definition: f a b = a + b + 2
let pfunctionDefinition =
    let funcdef = 
        //parse a name, then whitespace, then one or more (name, then possibly whitespace)
        pname .>> ws1 .>>. many1 (pname .>> ws)
    pipe2 (attempt funcdef) pEqExpr (fun (name, args) expr -> FunctionDef (name, args, expr)) <?> "function binding"
    
//Parse a variable definition
let pvariableDefinition =
    pipe2 (attempt pname) pEqExpr (fun name expr -> ValueDef (name, expr)) <?> "variable binding"

//parse an assignment command
// let Name = Expr [, Name2 = Expr2]
// let Name Arg [Arg2] = Expr
let pdefinitionList = 
    let manyBindings = sepBy1 (pfunctionDefinition <|> pvariableDefinition) (ws_str_ws ",")
    str_ws1 "let" >>. manyBindings

//attempts to parse a "negative" expression
//expression can be any term or bracketed expression
let pnegativeExpr =
    let negSymbols = (many1Chars (pchar '-')) 
    pipe2 negSymbols (pterm <|> pbracketedExpr) (fun s e -> if s.Length % 2 = 0 then e else Neg e) <?> "negative expression"

//implement the pexpr parser
do pexprRef :=
    let opp = new OperatorPrecedenceParser<Expr, unit, unit> ()
    opp.TermParser <-  choiceL [ pfunctionCall; 
                                 pterm; 
                                 pnegativeExpr; 
                                 pbracketedExpr; ] "expression" .>> ws
    //BEDMAS
    opp.AddOperator(InfixOperator("-",   ws, 1, Associativity.Left, fun x y -> Sub (x, y)))
    opp.AddOperator(InfixOperator("+",   ws, 2, Associativity.Left, fun x y -> Add (x, y)))
    opp.AddOperator(InfixOperator("*",   ws, 3, Associativity.Left, fun x y -> Mul (x, y)))
    opp.AddOperator(InfixOperator("/",   ws, 4, Associativity.Left, fun x y -> Div (x, y)))
    opp.AddOperator(InfixOperator("^",   ws, 5, Associativity.Left, fun x y -> Pow (x, y)))
    opp.AddOperator(InfixOperator("mod", ws, 6, Associativity.Left, fun x y -> Mod (x, y)))
    //Return the created expression parser
    opp.ExpressionParser