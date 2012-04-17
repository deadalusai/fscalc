module Calculator.Console.Program

open System
open System.IO
open System.Collections.Generic
open Calculator.Ast
open Calculator.Implementation
open Calculator.Console.CommandHelpers

let memory = new Dictionary<string, double> (Map.ofList [ ("pi", 3.14159) ])
let mutable debug = false

let getVar (key:string) =
    if not (memory.ContainsKey key) then
        failwith (sprintf "Variable %s does not exist" key)
    memory.[key]

let setVar (key:string) (value:double) =
    match key with
    | "_" -> printfn "= %g" value
    | key -> printfn "%s = %g" key value
    memory.[key] <- value

let clearVar (key:string) =
    match key with
    | "_" -> failwith "_ is a protected variable name"
    | key -> 
        if not (memory.ContainsKey key) then
            failwith (sprintf "Variable %s does not exist" key)
        memory.Remove key |> ignore

let invokeFunction (name:string) (arg:double) =
    match name with
    | "sin"  -> Math.Sin(arg)
    | "cos"  -> Math.Cos(arg)
    | "tan"  -> Math.Tan(arg)
    | "sqrt" -> Math.Sqrt(arg)
    | _ -> failwith (sprintf "Function %s not defined" name)


let printVariables () =
    for key in memory.Keys |> Seq.sortBy (fun key -> key) do
        if not (key = "_") then
            printfn "%5s -> %g" key memory.[key]

let printErr message =
    fprintfn System.Console.Error "ERROR: %s" message

let host = { new IStateHost with 
             member this.SetVariable k v = setVar k v
             member this.GetVariable k = getVar k
             member this.ClearVariable k = clearVar k
             member this.InvokeFunction n a = invokeFunction n a }

let runEquation (line:string) =
    try
        let parseResult = (parseLine line)
        match parseResult with
        | Error msg -> printErr msg
        | Command command -> 
            if debug then printfn "Command: %A" command
            executeCommand host command
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
        | ReadFile fname -> runFile fname
        | PrintVariables -> printVariables ()
        | Debug arg ->
            match arg with
            | "on"  -> debug <- true
            | "off" -> debug <- false
            | null  -> printfn "Debug mode %s" (if debug then "enabled" else "disabled")
            | _     -> printfn "Invalid argument: %s" arg
        | EquationCommand equation -> runEquation equation