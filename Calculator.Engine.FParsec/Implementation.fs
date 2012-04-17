module Calculator.Implementation

open System
open System.Collections.Generic

open FParsec.Primitives
open FParsec.CharParsers

open Calculator.Ast
open Calculator.Engine

type IStateHost =
    interface
       abstract member GetVariable : key:string -> double
       abstract member SetVariable : key:string -> double -> unit
       abstract member ClearVariable : key:string -> unit
       abstract member InvokeFunction : name:string -> double -> double
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
    | FunctionCall (name, e) -> evalFunction s name e
        
and evalFunction (s:IStateHost) (name:Name) (e:Expr) =
    s.InvokeFunction (evalName name) (evalExpr s e)

and evalTerm (s:IStateHost) (term:Term) =
    match term with
    | Constant c -> c
    | Variable n -> evalVariable s n

and evalName (name:Name) = match name with Name n -> n

and evalVariable (s:IStateHost) (name:Name) =
    s.GetVariable(evalName name)
    
/// Execute a command
let executeCommand (s:IStateHost) (eq:Command) =
    match eq with
    | Expr expr -> 
        let result = (evalExpr s expr)
        s.SetVariable "_" result
        
    | Update list ->
        //iterate through each update command, apply it and yield a result
        for command in list do
            match command with
            | Assignment (name, expr) -> 
                let name = (evalName name)
                let result = (evalExpr s expr)
                s.SetVariable name result

            | Deletion (name) ->
                (evalVariable s name) |> ignore //checks to make sure the variable exists
                let name = (evalName name)
                s.ClearVariable name