module Calculator.Console.CommandHelpers

let private whitespace = [| ' '; '\t' |]

let private parseBool s =
    match s with
    | "on"  | "true"  | "1" -> true
    | "off" | "false" | "0" -> false
    | _                     -> failwith (sprintf "could not parse debug switch '%s'" s)

type Command =
| Exit
| Help
| ClearScreen
| PrintVariables
| PrintDebug
| SetDebug of bool
| ReadFile of string
| ReadInput of string

    static member parse (commandString:string) =
        let input = ReadInput commandString
        let tokens = commandString.Split(whitespace, 2, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        match tokens with
        //bare commands
        | [command] ->
            match command with
            | "q"   | "exit"  -> Exit
            | "?"   | "help"  -> Help
            | "cls" | "clear" -> ClearScreen
            | "print"         -> PrintVariables
            | "debug"         -> PrintDebug
            //unknown - assume it's an equation
            | _ -> input
        
        //commands with an argument
        | [command; arg] ->
            match command with
            | "read"  -> ReadFile arg
            | "debug" -> SetDebug (parseBool arg)
            //unknown - assume it's an equation
            | _ -> input
        
        //unknown - assume it's an equation or binding
        | _ -> input