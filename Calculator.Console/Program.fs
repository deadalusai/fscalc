module Calculator.Console.Program

open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

let printVariables (state:State) =
    let variables = Map.toSeq state.MemoryMap |> Seq.sortBy (fun (key, _) -> key)
    let maxWidth = variables |> Seq.fold (fun max (key, _) -> if key.Length > max then key.Length else max) 0

    for key, value in variables do
        if not (key = "_") then
            let padding = String.replicate (maxWidth - key.Length) " "
            printfn "%s%s -> %g" padding key value

let printErr message =
    fprintfn stderr "ERROR: %s" message

let runEquation state line =
    let parseResult = (parseLine state line)
    match parseResult with
    | ParseError msg -> failwith msg
    | ParseSuccess statement ->
        if state.Debug then printfn "Statement: %A" statement
        
        let result = executeStatement state statement
        match result with
        | SingleResult (newState, value) ->
            printfn "= %g" value
            newState
        | AssignmentResult (newState, assignments) ->
            for (name, value) in assignments do
                printfn "%s = %g" name value
            newState

let processFile state fname =
    use reader = new System.IO.StreamReader(System.IO.File.OpenRead fname)
    let lines = seq {
        while not reader.EndOfStream do
            let line = reader.ReadLine().Trim()
            if line.Length > 0 && not (line.StartsWith "#") then
                yield line 
    }
    Seq.fold runEquation state lines

let handleInput state command =
    let none unit = None
    match command with
    | Exit            -> none (exit 0)
    | Help            -> none (printfn "%s" Help.helpText)
    | ClearScreen     -> none (try System.Console.Clear() with ex -> ())
    | PrintVariables  -> none (printVariables state)
    | PrintDebug      -> none (printfn "Debug mode %s" (if state.Debug then "enabled" else "disabled"))
    | SetDebug flag   -> Some { state with Debug = flag }
    | ReadFile fname  -> Some (processFile state fname)
    | ReadInput input -> Some (runEquation state input)

let initialState = { 
    MemoryMap = Map.ofSeq (seq { 
        yield "pi", 3.14159
    });
    FunctionMap = Map.ofSeq (seq {
        yield "sin",  System.Math.Sin
        yield "cos",  System.Math.Cos
        yield "tan",  System.Math.Tan
        yield "sqrt", System.Math.Sqrt
    });
    Debug = false 
}

//commands defines a sequence which provides the user input
let inputSeq = seq {
    while true do
        printf "> "
        let input = System.Console.ReadLine().Trim() 
        if input.Length > 0 then 
            yield input
}

//main handles parsing console commands and updating the application state
let main state input =
    try
        let command = Command.parse input 
        match handleInput state command with
        | Some newState -> newState
        | None          -> state

    with ex ->
        printErr ex.Message
        state

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
inputSeq |> Seq.fold main initialState |> ignore