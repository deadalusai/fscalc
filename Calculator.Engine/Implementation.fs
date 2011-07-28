namespace Calculator.Engine
open Microsoft.FSharp.Text.Lexing
open System
open System.Collections.Generic
open Lexer
open Parser

module CalcImpl =
    let initState (defaults:IDictionary<string, Double>) : CalcState =
        { memory = new Dictionary<string, Double>(defaults);
          debugOutput = System.Console.Out;
          debugMode = false }

    let parseLine (line:string) =
        Parser.start Lexer.tokenize (LexBuffer<char>.FromString line)

    let debug (s:CalcState) (unit) =
        if (s.debugMode = true && not (s.debugOutput = null)) then
            printfn "Evaluating %A" unit
        
    /// Evaluate a factor
    let rec evalFactor (s:CalcState) (factor:Factor) =
        debug s factor
        match factor with
        | Float (x)         -> x
        | Integer (x)       -> float x
        | Sqrt (f)          -> Math.Sqrt (evalFactor s f)
        | Sin (f)           -> Math.Sin (evalFactor s f)
        | Cos (f)           -> Math.Cos (evalFactor s f)
        | Tan (f)           -> Math.Tan (evalFactor s f)
        | Exponent (f1, f2) -> Math.Pow ((evalFactor s f1), (evalFactor s f2))
        | GroupExpr (expr)  -> evalExpr s expr
        | Variable (x)      -> evalVariable s x
        | Negative (x)      -> -(evalFactor s x)
        
    /// Evaluate a term
    and evalTerm (s:CalcState) (term:Term) =
        debug s term
        match term with
        | Times (term, fact)  -> (evalTerm s term) * (evalFactor s fact)
        | Divide (term, fact) -> (evalTerm s term) / (evalFactor s fact)
        | Mod (term, fact)    -> (evalTerm s term) % (evalFactor s fact)
        | Factor (fact)       -> evalFactor s fact

    /// Evaluate an expression
    and evalExpr (s:CalcState) (expr:Expr) =
        debug s expr
        match expr with
        | Plus (expr, term)  -> (evalExpr s expr) + (evalTerm s term)
        | Minus (expr, term) -> (evalExpr s expr) - (evalTerm s term)
        | Term (term)        -> evalTerm s term

    /// Evaluate a name
    and evalName (name:Name) =
        match name with
        | Name (x) -> x

    and evalVariable (s:CalcState) (name:Name) =
        let name = (evalName name)
        //look for a variable in memory
        if not (s.memory.ContainsKey name) then failwith (sprintf "variable %s does not exist" name)
        else s.memory.[name]

    /// Execute a command
    and executeCommand (s:CalcState) (eq:CalcCommand) =
        debug s eq
        match eq with
        | Equation (expr)         -> 
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
            s.memory.Remove (evalName name) |> ignore //deletes the variable value
            Double.NaN //return a default result (should be ignored)