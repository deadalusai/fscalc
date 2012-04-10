namespace Calculator.Implementation

open System
open System.Collections.Generic

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type CalcResult =
| Result of Command
| Error of string

module CalcImpl =
    let initState (defaults:IDictionary<string, Double>) : CalcState =
        { memory = new Dictionary<string, Double>(defaults);
          debugOutput = System.Console.Out;
          debugMode = false }

    let parseLine (line:string) =
        let r = runParserOnString pcommand_eof () "Input" line
        match r with
        | Success (expr, state, pos) -> Result expr
        | Failure (msg, err, state) -> Error msg

    let debug (s:CalcState) (unit) =
        if (s.debugMode = true && not (s.debugOutput = null)) then
            printfn "Evaluating %A" unit
        
    let rec evalExpr (s:CalcState) (expr:Expr) =
        debug s expr
        match expr with
        | Term t -> evalTerm s t
        | Add (l, r) -> (evalExpr s l) + (evalExpr s r)
        | Multiply (l, r) -> (evalExpr s l) * (evalExpr s r)
        | Subtract (l, r) -> (evalExpr s l) - (evalExpr s r)
        | Divide (l, r) -> (evalExpr s l) / (evalExpr s r)
        | Power (l, r) -> Math.Pow(evalExpr s l, evalExpr s r)
        | Sqrt e -> Math.Sqrt(evalExpr s e)

    and evalTerm (s:CalcState) (term:Term) =
        debug s term
        match term with
        | Constant c -> c
        | Variable n -> evalVariable s n

    and evalName (name:Name) = match name with Name n -> n

    and evalVariable (s:CalcState) (name:Name) =
        let name = (evalName name)
        //look for a variable in memory
        if not (s.memory.ContainsKey name) then failwith (sprintf "variable %s does not exist" name)
        else s.memory.[name]

    /// Execute a command
    and executeCommand (s:CalcState) (eq:Command) =
        debug s eq
        match eq with
        | Expr expr -> 
            let result = (evalExpr s expr)
            s.memory.["_"] <- result //set the last result to memory
            result
        
        | VarAssignment (name, expr) -> 
            let name = (evalName name)
            if (name = "_") then failwith "_ is a protected variable name"
            let result = (evalExpr s expr)
            s.memory.[name] <- result //set the result to memory
            result

        | VarDeletion (name) ->
            (evalVariable s name) |> ignore //checks to make sure the variable exists
            s.memory.Remove (evalName name) |> ignore //deletes the variable value
            Double.NaN //return a default result (should be ignored)