namespace Calculator.Engine
open System

type Factor =
    | Float     of Double
    | Integer   of Int64
    | Variable  of Name
    | Negative  of Factor
    | Exponent  of Factor * Factor
    | Sqrt      of Factor
    | Sin       of Factor
    | Cos       of Factor
    | Tan       of Factor
    | GroupExpr of Expr

and Term =
    | Times  of Term * Factor
    | Divide of Term * Factor
    | Mod    of Term * Factor
    | Factor of Factor

and Expr =
    | Plus  of Expr * Term
    | Minus of Expr * Term
    | Term  of Term

and Name =
    | Name of String
    
and CalcCommand =
    | Equation      of Expr
    | VarAssignment of Name * Expr
    | VarDeletion   of Name

/// Maintains "memory" state for a calculator
type CalcState =
    { memory: System.Collections.Generic.IDictionary<string, Double>;
      mutable debugOutput: System.IO.TextWriter;
      mutable debugMode: bool }