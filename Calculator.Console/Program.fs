module Calculator.Console.Program

open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

let printsn (s:string) = System.Console.WriteLine s

let printVariables (state:State) =
    let variables = Map.toList state.MemoryMap |> 
                    List.sortBy (fun (key, _) -> key) |>
                    List.sortBy (fun (___, s) -> s.GetType().Name) // hack hack - sort by type (functions first)

    let widestKey, _ = variables |> Seq.maxBy (fun (key, _) -> key.Length)

    let rec recurse items =
        match items with
        | ("_", _____)::tail -> recurse tail //Skip printing the implicit variable
        | (key, value)::tail -> 
            let s = match value with
                    | Function _ -> "Function"
                    | Value v    -> sprintf "Value %g" v

            let padding = String.replicate (String.length widestKey - String.length key) " "
            printfn "%s%s -> %s" padding key s
            recurse tail
        | _                  -> ()
        
    recurse variables

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

let runFile state filename =
    use reader = new System.IO.StreamReader(System.IO.File.OpenRead filename)
    let fileInput = seq {
        while not reader.EndOfStream do
            let line = reader.ReadLine().Trim()
            if line.Length > 0 && not (line.StartsWith "#") then
                yield line 
    }
    fileInput |> Seq.fold runEquation state

let handleUserCommand state command =
    let none unit = None
    match command with
    | Exit            -> none (exit 0)
    | Help            -> none (printsn Help.helpText)
    | ClearScreen     -> none (try System.Console.Clear() with ex -> ())
    | PrintVariables  -> none (printVariables state)
    | PrintDebug      -> none (printfn "Debug mode %s" (if state.Debug then "enabled" else "disabled"))
    | SetDebug flag   -> Some { state with Debug = flag }
    | ReadFile fname  -> Some (runFile state fname)
    | ReadInput input -> Some (runEquation state input)

let initialState = { 
    MemoryMap = Map.ofSeq (seq { 
        yield "pi", Value 3.14159
        //Functions
        yield "sin",  Function System.Math.Sin
        yield "cos",  Function System.Math.Cos
        yield "tan",  Function System.Math.Tan
        yield "sqrt", Function System.Math.Sqrt
    });
    Debug = false 
}

//commands defines a sequence which provides the user input
let rec userInput = seq {
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
        match handleUserCommand state command with
        | Some newState -> newState
        | None          -> state

    with ex ->
        printErr ex.Message
        state

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
userInput |> Seq.fold main initialState |> ignore