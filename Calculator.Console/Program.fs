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

let processFile (fname:string) (state:State) =
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

let processCommand (state:State) (command:Command) =
    match command with
    | Exit -> exit 0
    | Help -> printfn "%s" Help.helpText; state
    | ClearScreen -> 
        try System.Console.Clear(); state
        with ex -> state
    | ReadFile fname -> processFile fname state
    | PrintVariables -> printVariables state; state
    | Debug arg ->
        match arg with
        | "on"  -> { state with Debug = true }
        | "off" -> { state with Debug = false }
        | null  -> printfn "Debug mode %s" (if state.Debug then "enabled" else "disabled"); state
        | _     -> printfn "Invalid argument: %s" arg; state
    | EquationCommand equation -> runEquation state equation

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
let commands = seq { 
    while true do
        printf "> "
        match System.Console.ReadLine().Trim() with
        | "" -> ()
        | commandString -> yield Command.create commandString 
}

let initialState = { MemoryMap = Map.ofSeq (seq { 
                        yield "pi", 3.14159
                     });
                     FunctionMap = Map.ofSeq (seq {
                        yield "sin", (fun arg -> System.Math.Sin(arg))
                        yield "cos", (fun arg -> System.Math.Cos(arg))
                        yield "tan", (fun arg -> System.Math.Tan(arg))
                        yield "sqrt", (fun arg -> System.Math.Sqrt(arg))
                     });
                     Debug = false }

//start the main loop
Seq.fold processCommand initialState commands |> ignore