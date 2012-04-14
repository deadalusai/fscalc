module Calculator.Implementation

open System
open System.Collections.Generic

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type IStateHost =
    interface
       abstract member GetVar : key:string -> double
       abstract member SetVar : key:string -> double -> unit
       abstract member RemoveVar : key:string -> unit
    end

type ParseResult =
| Command of Command
| Error of string

let parseLine (line:string) =
    let parserResult = runParserOnString pcommand_eof () "Input" line
    match parserResult with
    | Success (expr, state, pos) -> Command expr
    | Failure (msg, err, state) -> Error msg
        
let rec evalExpr (s:IStateHost) (expr:Expr) =
    match expr with
    | Term t -> evalTerm s t
    | Add (l, r) -> (evalExpr s l) + (evalExpr s r)
    | Multiply (l, r) -> (evalExpr s l) * (evalExpr s r)
    | Subtract (l, r) -> (evalExpr s l) - (evalExpr s r)
    | Divide (l, r) -> (evalExpr s l) / (evalExpr s r)
    | Power (l, r) -> Math.Pow(evalExpr s l, evalExpr s r)
    | Modulo (l, r) -> (evalExpr s l) % (evalExpr s r)
    | Negative e -> -1.0 * (evalExpr s e)
    | Function f -> evalFunction s f
        
and evalFunction (s:IStateHost) (fn:Function) =
    match fn with
    | Sin e -> Math.Sin(evalExpr s e)
    | Cos e -> Math.Cos(evalExpr s e)
    | Tan e -> Math.Tan(evalExpr s e)
    | Sqrt e -> Math.Sqrt(evalExpr s e)

and evalTerm (s:IStateHost) (term:Term) =
    match term with
    | Constant c -> c
    | Variable n -> evalVariable s n

and evalName (name:Name) = match name with Name n -> n

and evalVariable (s:IStateHost) (name:Name) =
    s.GetVar(evalName name)
    //if not (s.memory.ContainsKey name) then failwith (sprintf "variable %s does not exist" name)
    //else s.memory.[name]

/// Execute a command
let executeCommand (s:IStateHost) (eq:Command) =
    match eq with
    | Expr expr -> 
        let result = (evalExpr s expr)
        s.SetVar "_" result
        
    | Update list ->
        //iterate through each update command, apply it and yield a result
        for command in list do
            match command with
            | Assignment (name, expr) -> 
                let name = (evalName name)
                let result = (evalExpr s expr)
                s.SetVar name result

            | Deletion (name) ->
                (evalVariable s name) |> ignore //checks to make sure the variable exists
                let name = (evalName name)
                s.RemoveVar name