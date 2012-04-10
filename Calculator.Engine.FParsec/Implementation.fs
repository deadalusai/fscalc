module Calculator.Implementation

open System
open System.Collections.Generic

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type ParseResult =
| Command of Command
| Error of string

type CommandResult =
| EvalResult of double
| AssignmentResult of string * double
| DeletionResult of string

let initCalcState (defaults:IDictionary<string, Double>) : CalcState =
    { memory = new Dictionary<string, Double>(defaults);
      debugOutput = System.Console.Out;
      debugMode = false }

let parseLine (line:string) =
    let parserResult = runParserOnString pcommand_eof () "Input" line
    match parserResult with
    | Success (expr, state, pos) -> Command expr
    | Failure (msg, err, state) -> Error msg

let private debug (s:CalcState) format (a:'a) =
    if (s.debugMode = true && not (s.debugOutput = null)) then
        printfn format a
        
let rec evalExpr (s:CalcState) (expr:Expr) =
    debug s "eval expr: %A" expr
    match expr with
    | Term t -> evalTerm s t
    | Add (l, r) -> (evalExpr s l) + (evalExpr s r)
    | Multiply (l, r) -> (evalExpr s l) * (evalExpr s r)
    | Subtract (l, r) -> (evalExpr s l) - (evalExpr s r)
    | Divide (l, r) -> (evalExpr s l) / (evalExpr s r)
    | Power (l, r) -> Math.Pow(evalExpr s l, evalExpr s r)
    | Modulo (l, r) -> (evalExpr s l) % (evalExpr s r)
    | Function f -> evalFunction s f
        
and evalFunction (s:CalcState) (fn:Function) =
    debug s "eval function: %A" fn
    match fn with
    | Sin e -> Math.Sin(evalExpr s e)
    | Cos e -> Math.Cos(evalExpr s e)
    | Tan e -> Math.Tan(evalExpr s e)
    | Sqrt e -> Math.Sqrt(evalExpr s e)

and evalTerm (s:CalcState) (term:Term) =
    debug s "eval term: %A" term
    match term with
    | Constant c -> c
    | Variable n -> evalVariable s n

and evalName (name:Name) = match name with Name n -> n

and evalVariable (s:CalcState) (name:Name) =
    debug s "eval variable: %A" name
    let name = (evalName name)
    //look for a variable in memory
    if not (s.memory.ContainsKey name) then failwith (sprintf "variable %s does not exist" name)
    else s.memory.[name]

/// Execute a command
let executeCommand (s:CalcState) (eq:Command) (onResult:CommandResult -> unit) =
    debug s "execute command: %A" eq
    match eq with
    | Expr expr -> 
        let result = (evalExpr s expr)
        s.memory.["_"] <- result //set the last result to memory
        onResult (EvalResult result)
        
    | Update list ->
        //iterate through each update command, apply it and yield a result
        for command in list do
            match command with
            | Assignment (name, expr) -> 
                let name = (evalName name)
                if (name = "_") then failwith "_ is a protected variable name"
                let result = (evalExpr s expr)
                s.memory.[name] <- result //set the result to memory
                onResult (AssignmentResult (name, result))

            | Deletion (name) ->
                (evalVariable s name) |> ignore //checks to make sure the variable exists
                let name = (evalName name)
                s.memory.Remove name |> ignore //deletes the variable value
                onResult (DeletionResult name)