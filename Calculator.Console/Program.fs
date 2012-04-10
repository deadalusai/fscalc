module Calculator.Console.Program

open System
open System.IO
open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

//some defaults..
let defaults = Map.empty
                  .Add ("pi", 3.14159)

//set up CalcState
let calcState = initCalcState defaults

let printErr message =
    fprintfn System.Console.Error "ERROR: %s" message

let runEquation (line:string) =
    try
        let parseResult = (parseLine line)
        match parseResult with
        | Error msg -> printErr msg
        | Command command ->
            let eachResult = (executeCommand calcState command)
            let printResult commandResult =
                match commandResult with
                | EvalResult result -> printfn "= %g" result
                | DeletionResult name -> printfn "deleted %s" name
                | AssignmentResult (name, result) -> printfn "%s = %g" name result
            //for each result, print to the output
            eachResult printResult
    with ex ->
        printErr ex.Message
    
let runFile (fname:string) =
    try
        use reader = new StreamReader (File.OpenRead fname)
        while not reader.EndOfStream do
            let line = reader.ReadLine().Trim()
            if line.Length > 0 && not (line.StartsWith "#") then
                runEquation line
    with ex ->
        printErr ex.Message

let processCommand command = 
    match command with
    //exit condition
    | Exit -> exit 0
    //show help
    | Help -> Help.write ()
    //clear screen
    | ClearScreen -> try Console.Clear() with ex -> ()
    //read from file
    | ReadFile (fname) -> runFile fname
    //control debug mode
    | Debug (setting) ->
        match setting with 
        | "on"  -> calcState.debugMode <- true
        | "off" -> calcState.debugMode <- false
        | null  -> ()
        | _     -> printErr (sprintf "unknown debug switch '%s'" setting)
        printfn "debug mode %s" (if calcState.debugMode then "on" else "off")
    //read variables
    | PrintVariables ->
        for key in calcState.memory.Keys |> Seq.sortBy (fun key -> key) do
            if not (key = "_") then
                printfn "%5s -> %g" key calcState.memory.[key]
    //evaluate as an equation!
    | EquationCommand (equation) -> runEquation equation

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
while true do
    printf "> "
    match Console.ReadLine().Trim() with
    //no input - continue
    | "" -> ()
    //a calculator command - process
    | commandString -> 
        let command = (createCommand commandString)
        processCommand command