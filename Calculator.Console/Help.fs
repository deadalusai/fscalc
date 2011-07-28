namespace Calculator.Console

module Help =
    let write () =
        printfn @"The following operations are supported:
    - multiplication -> X * Y
    - division       -> X / Y
    - addition       -> X + Y
    - subtraction    -> X - Y
    - parenthesis    -> (X + Y) * Z
    - square root    -> sqrt X
    - sine           -> sin X
    - cosine         -> cos X
    - tangent        -> tan X
    - exponentiation -> X ^ Y
    - modulo         -> X mod Y

Numbers can be integers or floating-point numbers. 
Full 'e' scientific notation is supported:

    1.1     -> 1.1
    .1      -> 0.1
    1.2e2   -> 120.0
    .1e-10  -> 0.00000000001 (printed as 1e-11)

Operations can be chained indefinitely:
    > 8 * (sqrt 2 + 6) ^ cos 20e-1
    = 3.47548

You can assign variables:
    > let sixty_four = 8 * 8
    sixty_four = 64

And use them in equations:
    > sqrt sixty_four
    = 8

The result of the last equation is always assigned to the variable '_'
    > _ - 6
    = 2

You can delete variables:
    > del sixty_four

You can read instructions from a file (including variable assignments)
Each line is interpreted as one equation
    > read C:\path\to\file.txt

Type 'print' to print out the variables in memory
Type 'debug on|off' to enable/disable debug mode
Type 'debug' to see the current debug mode
Type 'cls' to clear the screen
Type 'q' or 'exit' to quit
Type '?' or 'help' to see this help text"
