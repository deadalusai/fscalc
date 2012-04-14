module Calculator.Console.Program

open System
open System.IO
open System.Collections.Generic
open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

let memory = new Dictionary<string, double> (Map.ofList [ ("pi", 3.14159) ])

let getVar (key:string) =
    if not (memory.ContainsKey key) then
        failwith (sprintf "Variable %s does not exist" key)
    memory.[key]

let setVar (key:string) (value:double) =
    match key with
    | "_" -> printfn "= %g" value
    | key -> printfn "%s = %g" key value
    memory.[key] <- value

let removeVar (key:string) =
    match key with
    | "_" -> failwith "_ is a protected variable name"
    | key -> 
        if not (memory.ContainsKey key) then
            failwith (sprintf "Variable %s does not exist" key)
        memory.Remove key |> ignore

let printVariables () =
    for key in memory.Keys |> Seq.sortBy (fun key -> key) do
        if not (key = "_") then
            printfn "%5s -> %g" key memory.[key]

let printErr message =
    fprintfn System.Console.Error "ERROR: %s" message

let host = { new IStateHost with 
                member this.SetVar k v = setVar k v
                member this.GetVar k = getVar k
                member this.RemoveVar k = removeVar k }

let runEquation (line:string) =
    try
        let parseResult = (parseLine line)
        match parseResult with
        | Error msg -> printErr msg
        | Command command -> executeCommand host command
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

//start the main loop
printfn "Calculator - type '?' for help, 'q' to quit"
while true do
    printf "> "
    match Console.ReadLine().Trim() with
    //no input - continue
    | "" -> ()
    //a calculator command - process
    | commandString ->
        match (Command.create commandString) with
        | Exit -> exit 0
        | Help -> Help.write ()
        | ClearScreen -> try Console.Clear() with ex -> ()
        | ReadFile (fname) -> runFile fname
        | PrintVariables -> printVariables ()
        | EquationCommand (equation) -> runEquation equation