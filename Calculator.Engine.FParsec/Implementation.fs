module Calculator.Implementation

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type Stored = 
| Value of float
| Function of (float -> float)

type State = { MemoryMap : Map<string, Stored>;
               Debug: bool }

type ParseResult =
| ParseSuccess of Statement
| ParseError of string

type CommandResult = 
| SingleResult of State * float
| AssignmentResult of State * (string * float) list

let parseLine state line =
    let parserResult = runParserOnString pcommand_eof () "Input" line
    match parserResult with
    | Success (expr, state, pos) -> ParseSuccess expr
    | Failure (msg, err, state)  -> ParseError msg

let private getStored state name =
    match (Map.tryFind name.Key state.MemoryMap) with
    | Some s -> s
    | None   -> failwith (sprintf "Name %s not defined" name.Key)

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

and evalFunction state name expr =
    match (getStored state name) with
    | Function f -> f (evalExpr state expr)
    | _          -> failwith (sprintf "%s is not a function" name.Key)
    
and evalVariable state name =
    match (getStored state name) with
    | Value f -> f
    | _       -> failwith (sprintf "%s is not a value" name.Key)

/// Execute a statement
let executeStatement state statement =
    let setMem state key value = { state with MemoryMap = Map.add key value state.MemoryMap }
    let clearMem state key = { state with MemoryMap = Map.remove key state.MemoryMap }
    
    match statement with
    | Single expression ->
        let result = (evalExpr state expression)
        let newState = setMem state "_" (Value result)
        SingleResult (newState, result)
        
    | Assignment expressions ->
        //Each update may optionally add to the list of "assignment results"
        let applyUpdate (state, results) (name, expr) =
            let result = (evalExpr state expr)
            let newState = setMem state name.Key (Value result)
            (newState, (name.Key, result) :: results)
        
        //fold the list of "updates" to generate a new state and a list of assignments
        let newState, assignments = List.fold applyUpdate (state, []) expressions
        AssignmentResult (newState, List.rev assignments)
