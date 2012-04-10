namespace Calculator.Console

open System
open System.IO
open Calculator.Ast
open Calculator.Implementation

module Main =
    //some defaults..
    let defaults = Map.empty
                      .Add ("pi", 3.14159)

    //set up CalcState
    let calcState = CalcImpl.initState defaults

    let printerr message =
        fprintfn System.Console.Error "ERROR: %s" message

    let runEquation (line:string) =
        try
            match (CalcImpl.parseLine line) with
            | Error msg -> 
                printerr msg
            
            | Result command -> 
                let value  = CalcImpl.executeCommand calcState command
                //print result
                match command with
                | Expr expr                  -> printfn "= %g" value
                | VarAssignment (name, expr) -> printfn "%s = %g" (CalcImpl.evalName name) value
                | VarDeletion (name)         -> ignore ()

        with ex ->
            printerr ex.Message
    
    let runFile (fname:string) =
        try
             let rec runner (lines:string list) =
                match lines with
                | [] -> ()
                | line::tail ->
                    let line = line.Trim()
                    //ignore whitespace and comment lines
                    if (line.Length > 0 && not (line.StartsWith("#"))) then
                        runEquation line
                    runner tail
             runner (File.ReadAllLines(fname) |> Seq.toList)   
        with ex ->
            printerr ex.Message

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
            | _     -> printerr (sprintf "unknown debug switch '%s'" setting)
            printfn "debug mode %s" (if calcState.debugMode then "on" else "off")
        //read variables
        | PrintVariables ->
            for key in calcState.memory.Keys |> Seq.sortBy (fun key -> key.Length) do
                if not (key = "_") then
                    let fmt = new Printf.TextWriterFormat<System.String> ""

                    printfn "%5s -> %g" key calcState.memory.[key]
        //evaluate as an equation!
        | EquationCommand (equation) -> runEquation equation

    let main () =
        //Start!
        printfn "Calculator - type '?' for help, 'q' to quit"
        while true do
            printf "> "
            match Console.ReadLine().Trim() with
            //no input - continue
            | "" -> ()
            //a calculator command - process
            | x ->
                let command = (CommandHelpers.createCommand x)
                processCommand command

    //Start the main loop
    main ()