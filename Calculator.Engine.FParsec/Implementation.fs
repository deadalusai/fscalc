module Calculator.Implementation

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type State = { MemoryMap : Map<string, float>; 
               FunctionMap : Map<string, (float -> float)>
               Debug: bool }

type ParseResult =
| Command of Command
| Error of string

type CommandResult = 
| SingleResult of State * float
| AssignmentResult of State * (string * float) list

let private parserStateFrom state =
    let functionNames = Map.toSeq state.FunctionMap |> Seq.map (fun (key, value) -> key) |> Set.ofSeq
    { Functions = functionNames }

let parseLine state line =
    let parserState = parserStateFrom state
    let parserResult = runParserOnString pcommand_eof parserState "Input" line
    match parserResult with
    | Success (expr, state, pos) -> Command expr
    | Failure (msg, err, state) -> Error msg
        
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
    match (Map.tryFind name.Key state.FunctionMap) with
    | Some fn -> fn (evalExpr state expr)
    | None -> failwith (sprintf "Function %s not defined" name.Key)
    
and evalVariable state name =
    match (Map.tryFind name.Key state.MemoryMap) with
    | Some v -> v
    | None -> failwith (sprintf "Variable %s does not exist" name.Key)

/// Execute a command
let executeCommand state command =
    let setMem state key value = { state with MemoryMap = Map.add key value state.MemoryMap }
    let clearMem state key = { state with MemoryMap = Map.remove key state.MemoryMap }
    
    match command with
    | Single expr ->
        let result = (evalExpr state expr)
        let newState = setMem state "_" result
        SingleResult (newState, result)
        
    | Assignment expressions ->
        //Each update may optionally add to the list of "assignment results"
        let applyUpdate (state, results) (name, expr) =
            let result = (evalExpr state expr)
            let newState = setMem state name.Key result
            (newState, (name.Key, result) :: results)
        
        //fold the list of "updates" to generate a new state and a list of assignments
        let newState, assignments = List.fold applyUpdate (state, []) expressions
        AssignmentResult (newState, List.rev assignments)
