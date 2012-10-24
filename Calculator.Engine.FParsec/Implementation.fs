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
   
type Statement =
| SingleExpr     of Expr
| DefinitionList of Definition list //definition ops can be done en-mass

type ParseResult =
| ParseSuccess of Statement
| ParseError   of string

type CommandResult = 
| SingleResult     of State * float
| DefinitionResult of State * (string * string) list

let private setMem state key value = { state with MemoryMap = Map.add key value state.MemoryMap }
let private clearMem state key = { state with MemoryMap = Map.remove key state.MemoryMap }

let private pcommand = choice [(pdefinitionList |>> DefinitionList);
                               (pexpr           |>> SingleExpr    )] .>> eof

let parseLine state line =
    let parserResult = runParserOnString pcommand () "Input" line
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
    | Fetch n    -> evalFetch state n
    //functions
    | FunctionCall (name, args) -> evalFunction state name args
    //operators
    | Add       (l, r) -> (evalExpr' l) + (evalExpr' r)
    | Multiply  (l, r) -> (evalExpr' l) * (evalExpr' r)
    | Subtract  (l, r) -> (evalExpr' l) - (evalExpr' r)
    | Divide    (l, r) -> (evalExpr' l) / (evalExpr' r)
    | Power     (l, r) -> System.Math.Pow(evalExpr' l, evalExpr' r)
    | Modulo    (l, r) -> (evalExpr' l) % (evalExpr' r)
    | Negative  e      -> -1.0 * (evalExpr' e)

and evalFunction state name argExprs =
    let evalBuiltinFunction state f fArgExprs =
        match fArgExprs with
        | argExpr::[] -> f (evalExpr state argExpr)
        | _           -> failwith (sprintf "Expected 1 args, got %i" (List.length argExprs))

    let evalUserFunction initialState fArgNames fArgExprs fExpr =
        //assert that arguments have been provided
        match (List.length fArgNames, List.length fArgExprs) with
        | (required, got) when not (required = got) -> failwith (sprintf "Expected %i args, got %i" required got)
        | _ -> 
            //push each argument into the function state
            let args = List.zip fArgNames fArgExprs
            let updateState state (argName, argExpr) = setMem state argName (Value (evalExpr initialState argExpr))
            let fState = args |> List.fold updateState initialState
            //And evaluate
            evalExpr fState fExpr
    
    match (getStored state name) with
    | Builtin f                -> evalBuiltinFunction state f argExprs 
    | Function (args, funExpr) -> evalUserFunction state args argExprs funExpr
    | _                        -> failwith (sprintf "%s is not a function" name)

and evalFetch state name =
    match (getStored state name) with
    | Value f -> f
    | _       -> failwith (sprintf "%s is not a value" name)

//Helper methods for sanity-checking user-defined functions
let rec private assertFunctionCallNotRecusive state functionName expr =
    //recursively check each sub-expression in expr, looking for a call to function functionName
    let check subExpr = 
        assertFunctionCallNotRecusive state functionName subExpr
    match expr with
    | FunctionCall (name, args) -> 
        //check the function called is not *this* function
        if name = functionName then 
            failwith (sprintf "Recursive functions are not allowed (%s)" name)
        //check each expression passed in argument
        List.iter check args
        //check the expression of the function being called
        match getStored state name with
        | Function (_, expr) -> check expr
        | _                  -> () //ignore builtins or values
    //check all other sub-expressions
    | Add      (l, r) -> check l; check r
    | Multiply (l, r) -> check l; check r
    | Subtract (l, r) -> check l; check r
    | Divide   (l, r) -> check l; check r
    | Power    (l, r) -> check l; check r
    | Modulo   (l, r) -> check l; check r
    | Negative e      -> check e
    //ignore constants and fetch operations
    | _ -> ()

let private assertFunctionArgumentsDistinct args =
    let checkUnique stack arg =
        if Set.contains arg stack then
            failwith (sprintf "Duplicate argument names not allowed (%s)" arg)
        Set.add arg stack
    args |> List.fold checkUnique Set.empty |> ignore

/// Execute a statement
let executeStatement state statement =
    match statement with
    | SingleExpr expression -> 
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
                //check for recursive functions! We can't branch, so all recursive functions will recurse forever...
                assertFunctionCallNotRecusive state name expr
                assertFunctionArgumentsDistinct args
                let newState = setMem state name (Function (args, expr))
                let signature = sprintf "%s %s" name (System.String.Join(" ", args))
                (newState, (signature, sprintf "%A" expr) :: reports)
        
        //fold the list of "updates" to generate a new state and a list of assignments
        let newState, reports = List.fold applyUpdate (state, []) definitions
        DefinitionResult (newState, List.rev reports)
