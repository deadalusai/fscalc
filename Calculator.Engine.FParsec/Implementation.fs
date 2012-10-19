module Calculator.Implementation

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type Stored = 
| Value of float
| Builtin of (float -> float)
| Function of Name list * Expr

type State = { MemoryMap : Map<string, Stored>;
               Debug: bool }
   
let private setMem state key value = { state with MemoryMap = Map.add key value state.MemoryMap }
let private clearMem state key = { state with MemoryMap = Map.remove key state.MemoryMap }

type ParseResult =
| ParseSuccess of Statement
| ParseError of string

type CommandResult = 
| SingleResult of State * float
| DefinitionResult of State * (string * string) list

let parseLine state line =
    let parserResult = runParserOnString pcommand_eof () "Input" line
    match parserResult with
    | Success (expr, state, pos) -> ParseSuccess expr
    | Failure (msg, err, state)  -> ParseError msg

let private getStored state name =
    match (Map.tryFind name state.MemoryMap) with
    | Some s -> s
    | None   -> failwith (sprintf "Name %s not defined" name)

let rec evalExpr state expr =
    let evalExpr' = evalExpr state
    match expr with
    //constants and variables
    | Constant c -> c
    | Variable n -> evalVariable state n
    //functions
    | FunctionCall (name, e) -> evalFunction state name e
    //operators
    | Add (l, r) -> (evalExpr' l) + (evalExpr' r)
    | Multiply (l, r) -> (evalExpr' l) * (evalExpr' r)
    | Subtract (l, r) -> (evalExpr' l) - (evalExpr' r)
    | Divide (l, r) -> (evalExpr' l) / (evalExpr' r)
    | Power (l, r) -> System.Math.Pow(evalExpr' l, evalExpr' r)
    | Modulo (l, r) -> (evalExpr' l) % (evalExpr' r)
    | Negative e -> -1.0 * (evalExpr' e)

and evalFunction state name expr = //TODO make expr -> exprList (list of arguments)
    match (getStored state name) with
    | Builtin f             -> f (evalExpr state expr)
    | Function (args, funExpr) -> evalUserFunction state args (expr::[]) funExpr
    | _                     -> failwith (sprintf "%s is not a function" name)

and private evalUserFunction initialState fArgNames fArgExprs fExpr =
        //assert that arguments have been provided
        let required = List.length fArgNames
        let got = List.length fArgExprs

        //TODO: pattern-match this?
        if not (required = got) then
            failwith (sprintf "Expected %i args, got %i" required got)
        
        //push each argument into the function state
        let args = List.zip fArgNames fArgExprs

        let updateState state (name, expr) =
            setMem state name (Value (evalExpr initialState expr))

        let fState = args |> List.fold updateState initialState
        
        evalExpr fState fExpr
     

and evalVariable state name =
    match (getStored state name) with
    | Value f -> f
    | _       -> failwith (sprintf "%s is not a value" name)

/// Execute a statement
let executeStatement state statement =
    match statement with
    | Single expression ->
        let result = (evalExpr state expression)
        let newState = setMem state "_" (Value result)
        SingleResult (newState, result)
        
    | DefinitionList definitions ->
        //Each update may optionally add to the list of "assignment results"
        let applyUpdate (state, reports) (def:Definition) =
            match def with
            | ValueDef (name, expr) ->
                let result = (evalExpr state expr)
                let newState = setMem state name (Value result)
                (newState, (name, sprintf "%g" result) :: reports)

            | FunctionDef (name, args, expr) ->
                let newState = setMem state name (Function (args, expr))
                (newState, (name, sprintf "Function %A = %A" args expr) :: reports)
        
        //fold the list of "updates" to generate a new state and a list of assignments
        let newState, reports = List.fold applyUpdate (state, []) definitions
        DefinitionResult (newState, List.rev reports)
