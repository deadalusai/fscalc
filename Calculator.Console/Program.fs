module Calculator.Console.Program

open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

let printVariables (state:State) =
    Map.toSeq state.MemoryMap |> 
    Seq.sortBy (fun (key, value) -> key) |>
    Seq.iter (fun (key, value) ->
                if not (key = "_") then
                    printfn "%5s -> %g" key value)

let printErr message =
    fprintfn System.Console.Error "ERROR: %s" message

let runEquation state line =
    let parseResult = (parseLine state line)
    match parseResult with
    | Error msg -> 
        printErr msg
        state
    | Command command -> 
        if state.Debug then printfn "Command: %A" command
        //executeCommand host command state
        try
            let result = executeCommand state command
            match result with
            | SingleResult (newState, value) ->
                printfn "= %g" value
                newState
            | AssignmentResult (newState, assignments) -> 
                for (name, value) in assignments do
                    printfn "%s = %g" name value
                newState
        with ex ->
            printErr ex.Message
            state

let processFile state fname =
    try
        use reader = new System.IO.StreamReader(System.IO.File.OpenRead fname)
        let lines = seq {
            while not reader.EndOfStream do
                let line = reader.ReadLine().Trim()
                if line.Length > 0 && not (line.StartsWith "#") then
                    yield line 
        }
        Seq.fold runEquation state lines
    with ex ->
        printErr ex.Message
        state

let tryExecuteCommand state command =
    let do' unit = None
    match command with
    | Exit           -> do' (exit 0)
    | Help           -> do' (printfn "%s" Help.helpText)
    | ClearScreen    -> do' (try System.Console.Clear() with ex -> ())
    | PrintVariables -> do' (printVariables state)
    | Debug arg ->
        match arg with
        | "on"  -> Some { state with Debug = true }
        | "off" -> Some { state with Debug = false }
        | null  -> do' (printfn "Debug mode %s" (if state.Debug then "enabled" else "disabled"))
        | _     -> do' (printfn "Invalid argument: %s" arg)
    | ReadFile fname           -> Some (processFile state fname)
    | EquationCommand equation -> Some (runEquation state equation)

let initialState = { 
    MemoryMap = Map.ofSeq (seq { 
        yield "pi", 3.14159
    });
    FunctionMap = Map.ofSeq (seq {
        yield "sin", (fun arg -> System.Math.Sin(arg))
        yield "cos", (fun arg -> System.Math.Cos(arg))
        yield "tan", (fun arg -> System.Math.Tan(arg))
        yield "sqrt", (fun arg -> System.Math.Sqrt(arg))
    });
    Debug = false 
}

//commands defines a sequence which provides the user input
let commands = seq {
    while true do
        printf "> "
        match System.Console.ReadLine().Trim() with
        | "" -> ()
        | commandString -> yield Command.create commandString 
}

let processCommand state command =
    match tryExecuteCommand state command with
    | Some newState -> newState
    | None -> state

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
Seq.fold processCommand initialState commands |> ignore