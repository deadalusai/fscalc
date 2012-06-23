module Calculator.Implementation

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type State = { memoryMap : Map<string, float>; 
               functionMap : Map<string, (float -> float)>
               debug: bool }

type ParseResult =
| Command of Command
| Error of string

type AssignmentResult = { name : string; value : float }

type CommandResult = 
| ExpressionResult of State * float
| UpdateResult of State * AssignmentResult list

let private parserStateFrom state =
    let functionNames = Map.toSeq state.functionMap |> Seq.map (fun (key, value) -> key) |> Set.ofSeq
    { functions = functionNames }

let parseLine state line =
    let parserState = parserStateFrom state
    let parserResult = runParserOnString pcommand_eof parserState "Input" line
    match parserResult with
    | Success (expr, state, pos) -> Command expr
    | Failure (msg, err, state) -> Error msg
        
let rec evalExpr state expr =
    let evalExpr' = evalExpr state
    match expr with
    | FunctionCall (name, e) -> evalFunction state name e
    | Term t -> evalTerm state t
    | Add (l, r) -> (evalExpr' l) + (evalExpr' r)
    | Multiply (l, r) -> (evalExpr' l) * (evalExpr' r)
    | Subtract (l, r) -> (evalExpr' l) - (evalExpr' r)
    | Divide (l, r) -> (evalExpr' l) / (evalExpr' r)
    | Power (l, r) -> System.Math.Pow(evalExpr' l, evalExpr' r)
    | Modulo (l, r) -> (evalExpr' l) % (evalExpr' r)
    | Negative e -> -1.0 * (evalExpr' e)
        
and evalFunction state name expr =
    let name = (evalName name)
    match (Map.tryFind name state.functionMap) with
    | Some fn -> fn (evalExpr state expr)
    | None -> failwith (sprintf "Function %s not defined" name)

and evalTerm state term =
    match term with
    | Constant c -> c
    | Variable n -> evalVariable state n

and evalName name = match name with Name n -> n

and evalVariable state name =
    let key = (evalName name)
    match (Map.tryFind key state.memoryMap) with
    | Some v -> v
    | None -> failwith (sprintf "Variable %s does not exist" key)

/// Execute a command
let executeCommand state command =
    let setMem state key value = { state with memoryMap = Map.add key value state.memoryMap }
    let clearMem state key = { state with memoryMap = Map.remove key state.memoryMap }
    
    match command with
    | Expr expr ->
        let result = (evalExpr state expr)
        let newState = setMem state "_" result
        ExpressionResult (newState, result)
        
    | Update list ->
        //Each update may optionally add to the list of "assignment results"
        let applyUpdate (state, results) update =
            match update with
            | Assignment (name, expr) -> 
                let name = (evalName name)
                let result = (evalExpr state expr)
                let newState = setMem state name result
                (newState, { name = name; value = result } :: results)

            | Deletion (name) ->
                (evalVariable state name) |> ignore //checks to make sure the variable exists
                let name = (evalName name)
                let newState = clearMem state name
                (newState, results)
        
        //fold the list of "updates" to generate a new state and a list of assignments
        let newState, results = Seq.fold applyUpdate (state, []) list
        UpdateResult (newState, List.rev results)
