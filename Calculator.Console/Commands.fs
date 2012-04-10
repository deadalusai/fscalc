module Calculator.Console.CommandHelpers

type Command =
| Exit
| Help
| ClearScreen
| PrintVariables
| Debug of string
| ReadFile of string
| EquationCommand of string

type private InputPair = 
    { Name : string; Args : string }
            
let private whitespace = [| ' '; '\t' |]

let private createInputPair (str:string) = 
    //split the input at the first whitespace
    //First value is the command name, second is any arguments passed to it
    let idx = (str.IndexOfAny whitespace)
    if idx = -1 then
        { Name = str; Args = null }
    else
        { Name = str.Substring(0, idx);
          Args = str.Substring(idx).Trim(); }

let createCommand (command:string) =
    let input = (createInputPair command)
    match input.Name with
    | "q"   | "exit"  -> Exit
    | "?"   | "help"  -> Help
    | "cls" | "clear" -> ClearScreen
    | "read"          -> ReadFile (input.Args)
    | "debug"         -> Debug (input.Args)
    | "print"         -> PrintVariables
    //unknown - assume it's an equation
    | _               -> EquationCommand (command)